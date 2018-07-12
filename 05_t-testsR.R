########################
#Two sample t-tests
######################
# This lesson will focus on the t-test family of hypothesis tests that assess the difference in 2 group means
#We will learn to perform the test and to check the assumptions

#About the data
#The data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States. It began in the 1960s and since 1999 examines a nationally representative sample of about 5,000 people each year. The NHANES interview includes demographic, socioeconomic, dietary, and health-related questions. The physical exam includes medical, dental, and physiological measurements, as well as several standard laboratory tests. NHANES is used to determine the prevalence of major diseases and risk factors for those diseases. NHANES data are also the basis for national standards for measurements like height, weight, and blood pressure. Data from this survey is used in epidemiology studies and health sciences research, which help develop public health policy, direct and design health programs and services, and expand the health knowledge for the Nation.

#We are using a small slice of this data. We're only using a handful of variables from the 2011-2012 survey years on about 5,000 individuals.

###########
#Load tidyverse
library(tidyverse)

#Create new project, move nhanes.csv to the project directory, then import the data:
nh <- read_csv("./Desktop/nhanes.csv")
nh

#Notice that all the categorical variables are read in as characters. This data type is used for storing strings of text, for example, IDs, names, descriptive text, etc. 

#There's another related data type called factors. Factors are used to represent categorical variables with two or more levels (groups), e.g., "male" or "female" for Gender, or "Single" versus "Committed" for RelationshipStatus.

#For the most part, statistical analysis treats these two data types the same. It's often easier to leave categorical variables as characters. However, in some cases you may get a warning message alerting you that a character variable was converted into a factor variable during analysis. Generally, these warnings are nothing to worry about. You can, if you like, convert individual variables to factor variables, or simply use dplyr's `mutate_if` to convert all character vectors to factor variables:

nh <- nh %>% mutate_if(is.character, as.factor)
nh

#Now take a look at a few columns that are now factors. Remember, you can look at individual variables with dataset$variable

nh$RelationshipStatus
nh$Race
levels(nh$Race)

#To see the whole dataset, click on the name of the data.frame in the Environment panel in RStudio

#We have both adults and children in this dataset. Let's create a new dataset that only has adults. 
nha <- filter(nh, Age>=18)

#To prevent us from making any mistakes downstream, let's remove the `nh` object.
rm(nh)

################
#T-test
#The function for a t-test is t.test(). See the help using ?t.test. 
?t.test

#We will use the formula method as the syntax for t.test
# t.test(responseVariable ~ groupVariable, data=myDataFrame)
# *this depends on your data being in long form in the csv file* -- for more help on what long form means, please refer to the video on Importing Data into R

#Let's start with a simple question

#Are there differences in height for males versus females in this dataset?

#To assess this question, first we make sure that a t-test is the correct type of analysis. A t-test tests the difference in 2 means - yes that is what we want to do. Next we need to decide what type of t-test we need to perform by thinking through the assumptions. Domain specific knowledge and exploratory data analyses will help here.

#Random sampling -- YES

#Independent samples -- YES (men and women are different people - unrelated). Would be paired t-test if we were assessing height of husband-wife pairs or brother-sister pairs

#Normality -- ?? well, we need to assess this. We'll discuss this in a few minutes.

#Equal variance. Also called homoscedasticity of variances. To assess this assumption, you could think about the populations of men and women and what you know about the variability of height and conclude reasonably that the variance is equal for height

# I like density plots colored by group. Helps you see distribution of variable to assess equal variance. Also helps you see if there is a noticeable difference in groups.
ggplot(nha, aes(Height, color = Gender, fill = Gender)) + geom_density(alpha = 0.5)
#looks like there is a significant difference between the populations.
#looks like we are safe to assume equal variance

#The other assumption we need to talk about here is normality. Normality can be assessed graphically or via hypothesis tests. There are pros and cons to either approach. 

#We will assess normality using a QQ plot (quantile-quantile plot or quantile comparison plot or normal probability plot). This plot graphs the expected data value given a normal distribution on the X axis against the observed data value on the y axis. Let's have a look for height:

#first make datasets for each group
males <- nha %>% filter(Gender == "male")
females <- nha %>% filter(Gender == "female")

qqnorm(males$Height)
qqnorm(females$Height) 
#looks good

#Certain fields love hypothesis tests of normality and sometimes journals / reviewers will specifically request one. There is a theoretical problem with trying to _prove_ a null hypothesis and these tests are known to reject the null when sample sizes are large. My best advice is to use your brain, subject matter expertise, and graphical assessments as much as possible, but in case you are forced to do a hypothesis test for normality check out the following:

#shapiro.test (for Shapiro-Wilk test of normality)
#nortest package (for D'Agostino-Pearson omnibus test)

#Let's run the t-test. We said earlier that we wanted equal variance, so let's add that
t.test(Height~Gender, data=nha, var.equal = TRUE)
#men are significantly taller

#to write up results, you may want means and sd by group
nha %>%
  group_by(Gender) %>%
  summarize(meanHeight = mean(Height, na.rm = TRUE))

nha %>%
  group_by(Gender) %>%
  summarize(sdHeight = sd(Height, na.rm = TRUE))

######################
#Let's look at a second example
#Does BMI differ between diabetics and non-diabetics?

#EDA for this question
ggplot(nha, aes(BMI, color = Diabetes, fill = Diabetes)) +
  geom_density(alpha = 0.5)

#neither dist looks very normal
#There does seem to be a difference
#variance seems unequal

#create groups to check normality
yesD <- nha %>% filter(Diabetes == "Yes")
noD <- nha %>% filter(Diabetes == "No")

qqnorm(yesD$BMI)
qqnorm(noD$BMI) #ok but not great

#nonparametric test will be better than t-test due to non-normal distributions
wilcox.test(BMI~Diabetes, data=nha)

nha %>% 
  group_by(Diabetes) %>% 
  summarize(Q1BMI = quantile(BMI, probs = .25, na.rm = TRUE), medBMI = median(BMI, na.rm=TRUE), Q3BMI = quantile(BMI, probs = .75, na.rm = TRUE))
#people with diabetes have significantly higher BMI

#If you believe CLT allows for parametric test
t.test(BMI~Diabetes, data=nha)

#If you hypothesized that people with diabetes would have higher BMI at the outset, one-tailed test
#"No" will be first bc of alphabetical order, so we want less
?wilcox.test
wilcox.test(BMI~Diabetes, data=nha, alternative = "less")

## **A note on one-tailed versus two-tailed tests:**
#A two-tailed test is usually more appropriate. The hypothesis you're testing is spelled out in the output ("alternative hypothesis: true difference in means is not equal to 0"). If you do not know a priori whether the difference in means will be positive or negative, you want to do a two-tailed test. However, if we only wanted to test a very specific directional effect, we could use a one-tailed test and specify which direction we expect. This is more powerful if we "get it right", but much less powerful for the opposite effect. The p-value of a one-tailed test will be half of that of a two-tailed hypothesis test. BUT again, the **two-tailed test is almost always more appropriate** unless you know in what direction your results will be a priori.

## **A note on paired versus unpaired t-tests:**
#The tests we performed above were unpaired tests. Males and females are different people. The diabetics and nondiabetics are different samples.In these cases, an unpaired test is appropriate. 
#An alternative design might be when data is derived from samples who have been measured at two different time points or locations, e.g., before versus after treatment, left versus right hand, etc. In this case, a **paired test** would be more appropriate. A paired test takes into consideration the intra and inter-subject variability, and is more powerful than the unpaired test. There is a paired = TRUE argument for either the `t.test` or the `wilcox.test`.