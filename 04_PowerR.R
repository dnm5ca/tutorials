#######################
#Power and sample size
#######################

#In this section we will calculate power and sample size for various statistical tests using the pwr package

#Remember that power is the probability that we reject an incorrect null hypothesis (probability of finding an effect if there really is one)
#We want power to be 0.80 or higher

#Power is interconnected to alpha, sample size, and the effect size. The effect size is the magnitude of the test statistic (the size of the smallest effect you wish to be able to detect)

#Power of a statistical test is determined from:
#1. Sample Size
#2. Effect size
#3. Alpha
#4. One-sided alternative (only for certain tests)

#A sufficient sample size for a statistical test is determined from:
#1. Power
#2. Effect size
#3. Alpha
#4. One-sided alternative (only for certain tests)

#This becomes challenging because the effect size is difficult to estimate. As much as possible, use pilot data or published studies to estimate the effect size so that you can prospectively estimate the necessary sample size.

#Because the power and sample size calculations differ depending on the type of statisitcal analysis you plan to perform, you will need to also determine which statistical test to perform in order to calculate the sample size / power for that test. You should have an analysis plan prior to collecting data anyway so this is good practice!

#We are going to cover power / sample size calculations for a two-sample t-test but the same logic applies to all of the other statistical tests

#################
#Load pwr package
#################
#install.packages("pwr")
library(pwr)

#################
#T-tests
#################
?pwr.t.test
# EXAMPLE: I'm interested to know if there is a difference in the mean height between men and women. We will analyze these data using a t-test. Let's say I measure 5 males and 5 females. What is the power?

#Well I first need to determine the effect size (d). We see in the help menu that d = (difference in means) / pooled SD. I will estimate that the mean height for males is 70in while for women it is 64in. Let's say the standard deviation is 3in
diff <- (70-64)/3 #this is a large effect size

pwr.t.test(n = 5, d = diff, sig.level = 0.05)
#with n=5 per group, I barely have enough power to find a height difference between sexes *if the d=2

#######
# EXAMPLE 2: I'm interested to know how many samples I will need to have 90% power when alpha = 0.05 and I want to detect the difference between the following groups A:(ybar = 818.92, s = 16.11) and B:(ybar = 828.28, s = 14.09)
diff2 <- (818.92-828.28)/(mean(16.11, 14.09)) #this is a moderate effect size

pwr.t.test(power = 0.9, d = diff2, sig.level = 0.05)
#63 per group

#when n is the missing parameter, plotting the output from pwr.t.test is helpful
ex2 <- pwr.t.test(power = 0.9, d = diff2, sig.level = 0.05)
plot(ex2)

##########
#Changing arguments of pwr.t.test
?pwr.t.test
#type = "paired"
#alternative = "less"

##########
#Power in t-test when n are not equal
#EXAMPLE 3: I am looking at the BMI difference between people needing surgery and not needing surgery, but my n will be very different n = 10 for surgery, 35 for no surgery. I don't really know what the effect size will be, so I will estimate a moderate effect (Cohen's d = 0.5)
pwr.t2n.test(n1 = 10, n2 = 35, d = 0.5) #defaults to 0.05 significance

#maybe need more people? How many more?
pwr.t2n.test(power = .80, n2 = 35, d = 0.5) #a lot more! Uh oh

#maybe the effect size will be bigger than I thought
pwr.t2n.test(power = .80, n2 = 35, d = 1)

#########
#Same principles can be applied to find power for other tests:
#chi square (pwr.chisq.test)
#correlation (pwr.r.test)
#ANOVA (pwr.anova.test)
#linear regression models (pwr.f2.test)

