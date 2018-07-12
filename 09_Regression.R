#' ---
#' title: Regression
#' author: Data Services at the Claude Moore Health Sciences Library
#' date: University of Virginia
#' output:
#'    html_document:
#'      highlight: default
#' ---

#' This lesson will introduce you to linear regression modeling in R. We will cover how to create a model and analyze the significance of the slope and how to check the assumptions.
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

#' ## The first analysis we will run is a classic: Let's look at the relationship between height and weight.
#' 
#' H0: the slope describing the relationship between Height and Weight is not significantly different from 0
#' H1: as Height increases, Weight increases (a one-sided hypothesis)

fit <- lm(Weight~Height, data=nha)
confint(fit) #to get out confidence intervals for LM
summary(fit)

#' The relationship is highly significant. The intercept term is not very useful most of the time. Here it shows us what the value of Weight would be when Height=0cm, which could never happen. 
#' 
#' The Height coefficient (slope) is meaningful -- each one cm increase in height results in a 0.92 kg increase in weight. The p-value provided for this coefficient is the two-tailed value, so for our 1-tailed test, we would divide that by 2. In this case, it makes no difference because p is already so small.
#' 
#' Let's visualize the relationship with a scatterplot and add the least squares line of best fit:
nha %>%
ggplot(aes(x=Height, y=Weight)) + 
  geom_point() + 
  geom_smooth(method="lm")

#By default, this is only going to show the prediction over the range of the data. This is important! Don't extrapolate past the edges of your data

#' ### Assumptions of linear model:
#' 1. Linear relationship between x and y
#' 2. Random Sampling
#' 3. Equal variance across levels of X
#' 4. Normality of residuals

#' To check the these assumptions, R has a nice built-in plotting feature
plot(fit)

#' first plot shows most residuals are centered on 0, there are some high positive residuals, but no indication of unequal variance across X or a curve which would indicate departures from linearity
#' ...If you did see a violation in linearity, you may want to try modeling with non-linear terms (X^2) in a polynomial regression or try techniques other than least squares (glm, splines)
#' Normality of the residuals is not perfect...but LM is robust to modest departures in normality and we have large n --> ok
#' ...If you did not want to assume that the central limit theorem will provide adequate normality, there are other options (that are outside the scope of this video) -- check out rlm (in the MASS package) or lmRob (in the robust package)
#' ...For now, we will assume normality of residuals is met
#' The standardized residuals show the same pattern as the raw residuals -- nothing to be concerned about
#' The final plot shows the leverage of each point. For points with the highest leverage, to determine what impact they have on the regression, you could remove them from analysis and then re-examine the regression coefficients. If your overall conclusions do not change, don't worry too much. But if one or two datapoints are driving the effect that you see, that would be troublesome indeed.
#' ...In this case, looking at the scatterplot, I do not think that there are are datapoints that are particularly worrying

#' # Multiple Regression

#' Let's do a multiple linear regression analysis, where we attempt to model the effect of multiple predictor variables at once on some outcome. First, let's look at the effect of physical activity on testosterone levels.

fitP <- lm(Testosterone~PhysActive, data=nha)
summary(fitP)

#' The p-value is significant (p=0.01516), and the result suggests that increased physical activity is associated with increased testosterone levels. 
#' 
#' But, does increasing your physical activity increase your testosterone levels? Or is it the other way -- will increased testosterone encourage more physical activity? 
#' 
#' Or is it none of the above -- is the apparent relationship between physical activity and testosterone levels only apparent because both are correlated with yet a third, unaccounted for variable? 
#' 
#' Let's add Age into the model as well.

fitP <- lm(Testosterone~PhysActive+Age, data=nha)
summary(fitP)

#' Aha! After accounting for age, the testosterone / physical activity link is no longer significant. Every 1-year increase in age results in a highly significant decrease in testosterone, and since increasing age is also likely associated with decreased physical activity, perhaps age is the  confounder that makes this relationship apparent.

#' Adding other predictors can also swing things the other way. One variable that is hugely important to testosterone levels is sex! We know that men have much higher testosterone levels than females. Sex is probably the single best predictor of testosterone levels in our dataset. 
#' 
#' By not accounting for this effect, our unaccounted-for variation remains very high (low R^2 value). By accounting for Gender, we now reduce the residual error in the model, and the physical activity effect once again becomes significant. Also notice that our model fits much better (higher R-squared), and is much more significant overall

fitP <- lm(Testosterone ~ PhysActive+Age+Gender, data=nha)
summary(fitP)

#' We've only looked at the `summary()` and `confint()` functions for extracting information from an `lm` class object. However, there are several other accessor functions that can be used on a linear model object. Check out the help page for each one of these to learn more.

#' - `coefficients()`
#' - `predict.lm()`
#' - `fitted.values()`
#'- `residuals()`

#' We could easily spend hours learning the ins and outs of linear regression modeling (and I suggest you do just that!). This video provides just a taste of how to begin to interact with LMs in R.
