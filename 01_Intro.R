#Anything behind the hashtag or pound sign is a comment for you or colleagues

#Use comments to annotate your code so when you open this later you know what you were doing

####
#Introduction to R
####

#always write your codes in the SCRIPT window, not in the console window so that you can edit them rather than having to retype the whole line again

#using R as a calculator
2+2

#R knows order of operations and scientific notation
(5+3)^2
5+3^2
5e4

#assign objects using <-
weight_kg <- 55

#R is case sensitive
Weight_kg <- 60 #different than weight_kg

#Built-in Functions
sqrt(144)
log(1000)

#Get help with function
help(log)
?log #same as above

log(1000) #natural log!
log10(1000)
log(1000,  base = 10)


#Create vectors using c() function (concatenate / combine)
c(1,2,5)
c(1:5,11:15)

#assign vectors to object name
animal_weights <- c(50,60,66)
animal_weights

#vectors can also be character type
animals <- c("mouse", "rat", "dog")
animals

#Inspecting vectors
length(animals)
length(animal_weights)

class(animals)
class(animal_weights)

str(animal_weights) #structure
str(animals)

#sum
sum(animals) #errors out bc animals is character vector
sum(animal_weights)

#######################################################
#DataFrames
#data frames store heterogeneous tabular data in R: tabular, meaning that individuals or observations are typically represented in rows, while variables or features are represented as columns; heterogeneous, meaning that columns/features/variables can be different classes (on variable, e.g. age, can be numeric, while another, e.g., cause of death, can be text)

#NOTE
#before attempting to load data into R, please create a new R project (File-->New-->New Project) and put this script and the gapminder.csv dataset in the same directory as the Rproj file

#load data using read.csv
gm <- read.csv("gapminder.csv", header=TRUE)

#inspecting dataframes
gm
View(gm)

class(gm)

head(gm) #prints first 6 rows of df
tail(gm) #prints last 6 rows of df

names(gm) #column headings (variable names)
summary(gm)
str(gm)

#Using the $ to access variables
gm$pop

#####
#Descriptive statistics
#####
summary(gm$lifeExp)
mean(gm$lifeExp, na.rm=TRUE)
#would need mean(gm$lifeExp, na.rm = TRUE) if there are missing values in dataframe
median(gm$lifeExp)
sd(gm$lifeExp)
var(gm$lifeExp)

#to get Sum of Squares
#var = SS / (n-1) so SS = var * (n-1)
var(gm$lifeExp) * (length(gm$lifeExp) - 1)
