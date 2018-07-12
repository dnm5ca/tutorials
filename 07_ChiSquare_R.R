#' ---
#' title: Chi Square
#' author: Data Services at the Claude Moore Health Sciences Library
#' date: University of Virginia
#' output:
#'    html_document:
#'      highlight: default
#' ---

#' This lesson will introduce you to Chi Square analyses in R. We will cover both the Goodness of fit test (one factor) and the test of independence or chi square contingency test (2 factors).
#' 
#' In this lesson, the data we're going to work with comes from the National Health and Nutrition Examination Survey (NHANES) program at the CDC. NHANES is a research program designed to assess the health and nutritional status of adults and children in the United States. It began in the 1960s and since 1999 examines a nationally representative sample of about 5,000 people each year. The NHANES interview includes demographic, socioeconomic, dietary, and health-related questions. The physical exam includes medical, dental, and physiological measurements, as well as several standard laboratory tests. NHANES is used to determine the prevalence of major diseases and risk factors for those diseases. NHANES data are also the basis for national standards for measurements like height, weight, and blood pressure. Data from this survey is used in epidemiology studies and health sciences research, which help develop public health policy, direct and design health programs and services, and expand the health knowledge for the Nation.

#' We are using a small slice of this data. We're only using a handful of variables from the 2011-2012 survey years on about 5,000 individuals.

#' ## First we will load the nhanes.csv dataset
library(tidyverse)
nh <- read_csv("Desktop/nhanes.csv")
nh

#' Now we will filter the nh dataset so it just contains adults (>= 18 years old) and save the new dataset as nha

nha <- nh %>%
  filter(Age >= 18)

#' ## Chi Square Goodness of Fit Test
#'
#' ### Example 1. Gender in NHANES  
#' 
#' The chi square goodness of fit test assesses whether one factor is distributed as hypothesized. Do observed proportions match expected proportions?
?chisq.test
#' For the goodness of fit test, the chisq.test() works by comparing a table of observed proportions to a vector of theoretical proportions `p`
#' 
#' Therefore, we will have to supply a table and a vector as input.
#' 
#' Let's say we want to test whether the 2 genders coded in NHANES were equally sampled for adults.
#' 
#' We will start by making a table of the counts of each Gender.
#' 
#' The [`xtabs()`](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/xtabs.html) function is useful for creating tables of categorical data for use in the chisq.test() function. Let's create the Gender table.
xg <- xtabs(~Gender, data = nha)
xg

#' Looks pretty even! But let's do the test anyway
genderChi <- chisq.test(xg, p = c(.5, .5))
genderChi

prop.test(xg)

#' Check the assumption that expected values > 5. We know it is true given the N, but let's see how to do it.
genderChi$expected

#' ### Example 2. Race in NHANES
#' 
#' The original NHANES survey oversampled minority ethnicities. The nhanes.csv dataset attempted to undo that oversampling so that our sample would be representative of the US population overall. Let's see if our nhanes adults were sampled proportionally to the racial and ethnic proportions reported in the [US Census](https://www.census.gov/quickfacts/fact/table/US/PST045216). Note that this is actually several questions in the Census so I had to fudge a little (I divided Hispanic by 2 to get Mexican and Hispanic separately)
#'
#' Asian = .048
#' Black = .126
#' Hispanic = .089
#' Mexican = .089
#' Other = .035
#' White = .613
#' 
#' Create a Race table.
#' 
xr <- xtabs(~Race, data=nha)
xr

#' Now test our observed race proportions in nha against those from the census. Note that I input them in the same order as the table so they are matched up properly
raceChi <- chisq.test(xr, p = c(.048, .126, .089, .089, .035, .613))
raceChi
#' The result is significant. Let's now compare the expected values to the observed values to see where the differences are.
raceChi$expected
xr
#' More Asians & White people in nha than in Census
#' 
#' Fewer Black, Hispanic, Mexican, & Other people in nha than in Census
#' 
#' ## Chi Square Test of Independence
#' 
#' The chi-square test of independence is used to assess the relationship between two factors. The null hypothesis is that the two factors are  independent from one another or that one factor does not affect the other.
#' 
#' The `xtabs()` function is useful for creating contingency tables (with 2 factors) too. 
#' 
#' ### Example 1. Gender and Diabetes in NHANES
#' 
#' Let's see if Gender and Diabetes are independent or related. The null hypothesis is that gender and diabetes are independent, meaning that under the null hypothesis we expect a proportionally equal number of diabetics across each sex.
#' 
#' We'll first create a gender by diabetes status contingency table, and assign it to an object called `xt`
xt <- xtabs(~Gender+Diabetes, data=nha)
xt

#' There are two useful functions, `addmargins()` and `prop.table()` that add more information or manipulate how the data is displayed. The first adds the sums across the rows and the columns. 
#'
# Add marginal totals
addmargins(xt)
#' 
#' By default, `prop.table()` will divide the number of observations in each cell by the total. But you may want to specify _which margin_ you want to get proportions over. Let's do this for the first (row) margin.
#'
# Get the proportional table
prop.table(xt)
#each cell divided by grand total

#' That isn't really what we wanted. Do this over the first margin (rows) only.
?prop.table
prop.table(xt, margin=1)

#' Looks like men have slightly higher rates of diabetes than women. But is this significant?
chisq.test(xt)
#' Males seem to be at slightly higher risk of diabetes than females, but the difference is just short of being statistically significant.
#' 
#' ### Fisher's Exact Test
#' 
#' An alternative to the chi-square test is [Fisher's exact test](https://en.wikipedia.org/wiki/Fisher%27s_exact_test). 
#' 
#' Rather than relying on a critical value from a theoretical chi-square distribution, Fisher's exact test calculates the _exact_ probability of observing the contingency table as is. It's especially useful when there are very small _n_'s in one or more of the contingency table cells. Both the chi-square and Fisher's exact test give us p-values of approximately 0.06.
fisher.test(xt)
#' ### Example 2. Race and Insurance in NHANES
#' 
#' Let's create a different contingency table, this time looking at the relationship between race and whether the person had health insurance. Display the table with marginal totals.
xri <- xtabs(~Race+Insured, data=nha)
addmargins(xri)
#' Let's calculate the proportional table to show the proportion of people in each race category having health insurance. (we want to divide by the row sums)
prop.table(xri, margin=1)

#' Now, let's run a chi-square test for independence.
chisq.test(xri)

#' The result is _highly_ significant. In fact, so significant, that the display rounds off the p-value. Let's look at the expected values and compare them to the observed table to see where the differences are.
chisq.test(xri)$expected
xri
#' More uninsured Asian, Black, Hispanic, Mexican, & Other people than expected by chance
#' 
#' Fewer uninsured White people than expected by chance
#' 
#' A helpful plot for visualizing categorical data called a mosaic plot: (this is a base R plot, not ggplot2). 
mosaicplot(xri, main=NA)
#' These are the observed data. We can easily see the proportion of each race that is insured
#' 
#' Another way to visualize these data is with a stacked barplot
nha %>%
  ggplot(aes(Race)) + geom_bar(aes(fill = Insured))

#to see proportion insured, use position = "fill"
nha %>%
  ggplot(aes(Race)) + geom_bar(aes(fill = Insured), position = "fill")
