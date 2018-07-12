#' ---
#' title: ANOVA
#' author: Data Services at the Claude Moore Health Sciences Library
#' date: University of Virginia
#' output:
#'    html_document:
#'      highlight: default
#' ---

#' This lesson will introduce you to running ANOVA in R. We will learn how to perform the test and check the assumptions. 

#' Remember that t-tests are used to assess differences in means between two groups whereas ANOVA is used to assess differences in means between multiple groups. A t-test is just a specific case of ANOVA when there are only two groups.

#' In this lesson, the data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States. It began in the 1960s and since 1999 examines a nationally representative sample of about 5,000 people each year. The NHANES interview includes demographic, socioeconomic, dietary, and health-related questions. The physical exam includes medical, dental, and physiological measurements, as well as several standard laboratory tests. NHANES is used to determine the prevalence of major diseases and risk factors for those diseases. NHANES data are also the basis for national standards for measurements like height, weight, and blood pressure. Data from this survey is used in epidemiology studies and health sciences research, which help develop public health policy, direct and design health programs and services, and expand the health knowledge for the Nation.

#' We are using a small slice of this data. We're only using a handful of variables from the 2011-2012 survey years on about 5,000 individuals.

#' ## First we will load the nhanes.csv dataset

library(tidyverse)
nh <- read_csv("./data/nhanes.csv")
nh

#' Let's turn the character variables into factors so that R knows they are grouped
nh <- nh %>% 
  mutate_if(is.character, as.factor)
nh

#' Because the nhanes dataset contains both children and adults, let's filter for just the adults and call that dataset nha

nha <- nh %>% filter(Age >= 18)

#' Then we'll remove the nh dataset to prevent confusion
rm(nh)

#' ## Let's examine the relationship between `Height` and `Gender` 

#' Let's first do this with an equal variance t-test.
t.test(Height~Gender, data=nha, var.equal=TRUE)

#' Males are taller than females. Now, let's do the same test as an ANOVA. To code the ANOVA in R, we will use a linear modeling (regression) framework. First, let's create the model and store it in an object called `fit`. 
fit <- lm(Height~Gender, data=nha)

#'You can display the fit object itself, but that isn't too interesting. You can get the more familiar ANOVA table by calling the `anova()` function on the `fit` object. More generally, the `summary()` function on a linear model object will tell you much more.
anova(fit)
summary(fit) #regression output

#' Notice that the results of the t-test are directly related to the results from the ANOVA and the linear model

#' 1. The p-values from all three tests (t-test, ANOVA, and linear regression) all look the same (p< 2.2e-16). This is because they _ARE_ all identical: a t-test is a specific case of ANOVA, which is a specific case of linear regression. 

#' 2. The test statistics are all related. The _t_ statistic from the t-test is **55.713**, which is the same as the t-statistic from the linear regression. If you square that, you get **3103.9**, the _F_ statistic from the ANOVA. 

#' Let's do a second example with more than 2 groups.
#' ## Let's look at the relationship between `BMI` and `Smoking Status`.

#' This time our grouping variable `SmokingStatus` has 3 levels
levels(nha$SmokingStatus)

#' First we'll look at some summary statistics
nha %>%
  filter(SmokingStatus != "NA") %>%
  group_by(SmokingStatus) %>%
  summarize(meanBMI = mean(BMI, na.rm = TRUE), sdBMI = sd(BMI, na.rm = TRUE))

#' People who are current smokers have lower BMI

#' Now run the test

fit <- lm(BMI~SmokingStatus, data=nha)
anova(fit)

#'The p-value for the F-test in the ANOVA table tells us that there is a significant difference in mean BMI between different Smoking statuses

#'To tell where the significant differences are, we'll conduct Tukey's Honest Significant Differences post-hoc. Tukey's HSD computes all pairwise mean difference calculation, comparing each group to each other group, identifying any difference between two groups that's greater than the standard error, while controlling the type I error for all multiple comparisons. Notice the syntax with `aov()` (**not** `anova()`) on the fitted linear model object, then `TukeyHSD()` on the `aov()` object.

TukeyHSD(aov(fit))

#' Former is significantly different from Current. Never is significantly different from Current. Never is not different from Former.

#' ## Checking Assumptions of ANOVA
#' Remember the assumptions are Random Sampling, normality for each group, and equal variance between groups. There is a built in `plot` function that will provide several diagnostic plots for linear models that also apply to ANOVA - remember that these methods are directly related.
plot(fit)

#' The first plot shows the residuals on the y-axis and the 'fitted values' (group means for SmokingStatus) on the x. We can use this plot to determine that the variance (the variability of the residuals) is roughly the same across SmokingStatus groups
#' The second plot is a qqplot of the residuals. Based on this plot we might choose instead to run a Kruskal-Wallis (the non-parametric alternative to ANOVA) because the distribution does not look very normal
#' The third plot shows fitted values against the square root of the standardized residuals. This transform eliminates the sign on the residual, so that large residuals (both + and -) plot at the top. We want to see a flat red line here indicating that there is equal variance across levels of SmokingStatus
#' The final plot idenitifies points with high leverage (outliers or points to pay special attention to)

#' Let's try a Kruskal-Wallis for these data
kruskal.test(BMI~SmokingStatus, data=nha)

#' The Dunn's test is the most appropriate post-hoc test for the Kruskal-Wallis. We need to choose a p-adjustment method to control for multiple testing. Here, I will select Benjamini-Hochberg which controls the false discovery rate.
#install.packages("dunn.test")
library(dunn.test)
dunn.test(x=nha$BMI, g=nha$SmokingStatus, method = "bh")
#' The output indicates that each of the groups is significantly different from the other 2

#' ## Plot results
#' We'll use a simple boxplot to examine the differences by group
nha %>%
  filter(SmokingStatus != "NA") %>%
  ggplot(aes(x = SmokingStatus, y = BMI)) + geom_boxplot() + theme_classic()

#' Now jazz it up. I add the actual data points and to avoid plotting the outliers twice, I turn off the outliers from the boxplot
nha %>%
  filter(SmokingStatus != "NA") %>%
  ggplot(aes(x = SmokingStatus, y = BMI)) + 
  geom_jitter(color = "sky blue", size = .3) + 
  geom_boxplot(outlier.shape = NA, fill = "navy", alpha = 0.5) + 
  theme_classic()
