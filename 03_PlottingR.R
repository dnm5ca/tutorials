#######################
#Graphing using ggplot2
#######################
#This section will cover fundamental concepts for creating effective data visualizations using R. We will use the ggplot2 package to create plots layer-by-layer.

#About ggplot2
#ggplot2 is a very popular R package that extends R's visualization capabilities. It takes the hassle out of things like creating legends or mapping variables to plot aesthetics like color.

#The "gg" in ggplot2 come from Leland Wilkinson's Grammar of Graphics (1999). The *Grammar of Graphics* allows you to think beyond the garden variety plot types (e.g. scatterplot, barplot) and the consider the components that make up a plot or graphic, such as how data are represented on the plot (as lines, points, etc.), how variables are mapped to coordinates or plotting shape or color, what transformation or statistical summary is required, and so on. 

#Specifically, **ggplot2** allows you to build a plot layer-by-layer by specifying:

#- a **geom**, which specifies how the data are represented on the plot (points, lines, bars, etc.)
#- **aesthetics** that map variables in the data to axes on the plot or to plotting size, shape, color, etc.

################
#Install package
################
#First we will need to install the a package to give us access to all of the functionality in the ggplot2 graphing library and lots of other functionality
#install.packages("tidyverse") #do this once (hopefully already done from Import lesson)
library(tidyverse) #do this everytime you open R

################
#Load data
################
#For this lesson, we will use the same data as in the Intro to R lesson - gapminder.csv
#Make sure that your data file is in your project directory and that R can see the file in the Files tab
#This time though, we will load it using read_csv from the readr package (installed with the tidyverse)
gm <- read_csv("~/Dektop/LearningR/gapminder.csv")

#Show the first few lines of the data (looks different than read.csv)
gm

################
#Scatterplot
################
#Make blank plot, specifying x and y
ggplot(gm, aes(x = gdpPercap, y = lifeExp))

#add points
ggplot(gm, aes(x = gdpPercap, y = lifeExp)) + geom_point()

#The typical workflow for building a ggplot2 plot is to first construct the figure and save that to a variable (for example, `p`), and as you're experimenting, you can continue to re-define the `p` object as you develop pieces you like

#save canvas to object p
p <- ggplot(gm, aes(gdpPercap, lifeExp))
p

#add points
p + geom_point()

#transform x axis
p + geom_point() + scale_x_log10()

#Reassign p to keep the log scaled x. 
p <- p + scale_x_log10()
p
p + geom_point()

#Now let's play around with the look of the points
#Add points that are huge (`size=8`) blue (`color="blue"`) semitransparent (`alpha=(1/4)`) triangles (`pch=17`)
p + geom_point(color="blue", pch=17, size=8, alpha=1/4)

#Add points colored by continent
p + geom_point(aes(color=continent))

#That looks nice, let's save it
p <- p + geom_point(aes(color=continent))

#Because continent is a variable it needs to be wrapped in a call to `aes()`. If I want the colors to be some static value, I don't wrap it in `aes()`. I just specify it outright. Same thing with other features of the points.

###########
#Histogram
###########
#Make blank plot, specifying the variable you want to graph as x
ggplot(gm, aes(x = lifeExp))

#add the histogram geom
ggplot(gm, aes(x = lifeExp)) + geom_histogram()

#save just the blank canvas as object q
q <- ggplot(gm, aes(x = lifeExp))
q
#now add the histogram and experiment with different bin sizes
q + geom_histogram(bins=30)
q + geom_histogram(bins=10)
q + geom_histogram(bins=200)

#color histogram by continent
q + geom_histogram(aes(color=continent))
q + geom_histogram(aes(fill=continent))
#If you look at the help for `?geom_histogram` you'll see that by default it stacks overlapping points. This isn't really an effective visualization. Let's change the position argument.

q + geom_histogram(aes(fill=continent), position="identity")
#the histograms are blocking each other. What if we tried transparency? Faceting?
q + geom_histogram(aes(fill=continent), position="identity", alpha=1/3)

################
#Density plots
###############
#Alternatively we could plot a smoothed density curve instead of a histogram:
q + geom_density()

#add color by continent
q + geom_density(aes(color=continent), lwd = 2)

#add fill by continent
q + geom_density(aes(fill=continent), alpha=1/4)

##########
#Boxplot
##########
b <- ggplot(gm, aes(continent, lifeExp))
b

#add boxplot
b + geom_boxplot()

#############
#Titles and themes
#############
p <- ggplot(gm, aes(gdpPercap, lifeExp))
p + scale_x_log10() + geom_point(aes(color=continent))

#save those
p <- p + scale_x_log10() + geom_point(aes(color=continent))

#Add titles
p + ggtitle("Life expectancy vs GDP by Continent")

p + ggtitle("Life expectancy vs GDP by Continent") + xlab("GDP Per Capita (USD)") + ylab("Life Expectancy (years)")

#save those
p <- p + ggtitle("Life expectancy vs GDP by Continent") + xlab("GDP Per Capita (USD)") + ylab("Life Expectancy (years)")

#add theme
p + theme_bw()