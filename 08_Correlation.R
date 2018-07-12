#' ---
#' title: Correlation
#' author: Data Services at the Claude Moore Health Sciences Library
#' date: University of Virginia
#' output:
#'    html_document:
#'      highlight: default
#' ---

#' This lesson will introduce you to correlation analyses in R. We will cover how to run the test, how to check the assumptions and what to do in case assumptions are not met.
#' 
#'
#' In this lesson, the data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States. It began in the 1960s and since 1999 examines a nationally representative sample of about 5,000 people each year. The NHANES interview includes demographic, socioeconomic, dietary, and health-related questions. The physical exam includes medical, dental, and physiological measurements, as well as several standard laboratory tests. NHANES is used to determine the prevalence of major diseases and risk factors for those diseases. NHANES data are also the basis for national standards for measurements like height, weight, and blood pressure. Data from this survey is used in epidemiology studies and health sciences research, which help develop public health policy, direct and design health programs and services, and expand the health knowledge for the Nation.

#' We are using a small slice of this data. We're only using a handful of variables from the 2011-2012 survey years on about 5,000 individuals.

#' ## First we will load the nhanes.csv dataset
library(tidyverse)
nh <- read_csv("data/nhanes.csv")
nh

#' Now we will filter the nh dataset so it just contains adults (>= 18 years old) and save the new dataset as nha

nha <- nh %>%
  filter(Age >= 18)

#' ## The first analysis we will run is a correlation between Weight and Pulse
#' Create a scatterplot to see the relationship between these variables
nha %>%
  ggplot(aes(Weight, Pulse)) + geom_point()
# does not look like there is a strong correlation between these variables, but the cloud of points is sort of elongated out towards the top right

#' ### Hypotheses
#' Null hypothesis is that there is no relationship between Weight and Pulse
#' Alternative hypothesis is that there is a significant relationship between Weight and Pulse
#' I can see testing both sides -- as someone gets heavier, their Pulse increases because their heart is working harder -- OR -- as someone gets heavier, their Pulse decreases because they are generally less active overall. Therefore, I will do a two-tailed test

#' ### Check assumptions
#' Random sampling is met
#' Need to make sure each variable is normally distributed
qqnorm(nha$Pulse)
qqnorm(nha$Weight)
#' No reason to believe that there is a non-linear relationship
#' No obvious heteroscedasticity in variance as weight increases or as Pulse increases
#' 
#' ### Run test
?cor.test
cor.test(~ Weight + Pulse, data = nha)
#' r = 0.071 which is positive (as weight increases, pulse increases), but low in magnitude
#' We get a significant p-value but I am not convinced of its real-world relevance
#'
#' # Example 2
#' Let's examine testosterone levels in males as they age.
#' 
#' H0: there is no relationship between age and testosterone
#' H1: as age increases, testosterone decreases
#' 
#' Let's start by plotting the data
nha %>%
  filter(Gender == "male") %>%
  ggplot(aes(Age, Testosterone)) + geom_point()
#'
#' A few things stand out here. It looks like anyone above age 80 was classified as 80, so I will remove those datapoints in my analysis. Second, there is one extreme outlier with a testosterone level above 1500. A quick internet search tells me that this is very much outside the normal range for testosterone levels (250 - 1100 ng/dl). Therefore, I will remove this datapoint before running my analysis.
#' 
#' Let's filter the data
nhT <- nha %>%
  filter(Gender == "male" & Age < 80 & Testosterone < 1500)
#' And create a new scatterplot
nhT %>%
  ggplot(aes(Age, Testosterone)) + geom_point()

#' Now I'll assess the assumptions
#' Random sampling -- met
#' Based on the scatterplot, I saw no violations in homoscedasticity of variance
#' Based on the scatterplot, I saw no violations of assumption of a linear relationship between age and testosterone
#' Normality of each variable
qqnorm(nhT$Testosterone) # looks fine
qqnorm(nhT$Age) #not normally distributed
hist(nhT$Age) #mostly uniformly distributed -- still ok bc of central limit theorem

#' Run the test
cor.test(~ Age + Testosterone, data = nhT, alternative = "less")
#' r is -0.10 and is highly significant
#' As age increases, testosterone decreases
nhT %>%
  ggplot(aes(Age, Testosterone)) + geom_point() + 
  annotate("text", x = 23, y = 0, label = "r = -0.1018", size = 5)
