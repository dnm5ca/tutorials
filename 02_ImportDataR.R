################################
#Importing data
###############################

#Option 1: make a vector
#entering one column of data directly into R (as a vector)
#use c(number, number, number)
#c means combine or concatenate

#10 pigs on diet1
diet1 <- c(60.8, 67, 65, 68.6, 61.7, 69.6, 77.1, 75.2, 71.5, 60.3)

#and other 10 pigs on diet2
diet2 <- c(62.4, 67.8, 61.3, 58.4, 70.1, 68.6, 64.7, 70, 61.6, 69.1)

str(diet1)
summary(diet1)
mean(diet1)
sd(diet1)

summary(diet2)
mean(diet2)
sd(diet2)

#################
#Option 2: import data from spreadsheet using read.csv
#easiest way is to import csv file
# *R always wants your data in long format*, meaning all the numbers in one column and the group labels in the other column

#create new project in R
#put csv file into project directory
#make R script in that directory

pig <- read.csv("pigs.csv", header = TRUE)

str(pig)
summary(pig)

#to call one column use the $ operator
mean(pig$weight)
class(pig$diet)

################
#Option 3: import data from spreadsheet using read_csv
#as opposed to read.csv, read_csv prints data to the screen more beautifully and is better to use for large datasets
#read_csv comes from a package that we will need to install

#install.packages("tidyverse")
library(tidyverse)

pig2 <- read_csv("pigs.csv")

pig2
str(pig2)
summary(pig2)

#to call one column use the $ operator
mean(pig2$weight)
class(pig2$diet)

#calculate mean weight by diet
#use special symbol %>% to mean "then"
pig2 %>%
  group_by(diet) %>%
  summarize(meanweight = mean(weight))
