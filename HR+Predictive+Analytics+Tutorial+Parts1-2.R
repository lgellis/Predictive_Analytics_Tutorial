
# If you are using local R install, you can use the following line of code to import
# hr = read.table("/filepath/HR_comma_sep.csv", head=TRUE, sep=",")

#If you are using DSX, import the file place your cursor in cell and "insert to code" as per the instructions here: 
#https://www.littlemissdata.com/blog/predictive-analytics-tutorial-part-1

#rename data frame
hr <- df.data.2 

#view columns
names(hr)

#quick view
head(hr)

#dim shows number of rows and columns
dim(hr)

#str shows the structure of each column including the type of data and sample values
str(hr)

#show summary statistics for each column.  For numeric: min, max, median, mean, 2st and 3rd quartile.  For factors show the counts of each.
summary(hr)

#install packages - do this one time
install.packages("data.table")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("ggplot")
install.packages("gcookbook")
install.packages("caret")
install.packages("hexbin")
install.packages("leaps")
install.packages("plyr")
install.packages("plotly")

# Load the relevant libraries - do this every time
library(data.table)
library(corrplot)
library(ggplot2)
library (gcookbook)
library(caret)
library(hexbin)
library(leaps)
library(plyr)
library(plotly)

#attach allows us to reference columns by their name 
attach(hr)

#Check Correlations of numeric columns
corMatrix <-cor(hr[1:8], use="complete.obs", method="pearson") 
#round to two decimals
round(corMatrix, 2)

#create correlation matricies
corrplot(corMatrix, method="circle")
corrplot(corMatrix, method="square")
corrplot(corMatrix, method="number")
corrplot(corMatrix, method="shade")
corrplot(M, type = "upper")
corrplot.mixed(corMatrix)


#rename
colNames <- c("satLevel", "lastEval", "numProj", "avgHrs", "timeCpny", "wrkAcdnt", "left", "fiveYrPrmo", "job", "salary")
setnames(hr, colNames)
names(hr)
attach(hr)

# Run a histogram for all numeric variables to understand distribution

hist(avgHrs/4, main="Distribution of Average Hours per Week", xlab="Avg Hours", breaks=7, col="lightblue")
hist(satLevel, main="Distribution of Satisfaction Level", xlab="Satisfaction Level", breaks=7, col="lightblue")
hist(lastEval, main="Distribution of Last Evaluations", xlab="Last Eval", breaks=7, col="lightblue")
hist(numProj, main="Distribution of Number of Projects", xlab="Number of Projects", breaks=7, col="lightblue")


# side by side pretty bar
ggplot(data=hr, aes(x=numProj, y=avgHrs, fill=salary)) + 
         geom_bar(position="dodge", color="black", stat="identity") +
         scale_fill_brewer(palette = "Pastel1") 



#density plot 
qplot(avgHrs/4, data=hr, geom="density", fill=salary, alpha=I(.5), 
      main="Avg Weekly Hours by Salary Category", xlab="Average Weekly Hours", 
      ylab="Density")


#create factor variables with a better format

hr$leftFactor <- factor(left,levels=c(0,1),
                     labels=c("Did Not Leave Company","Left Company")) 

hr$promoFactor <- factor(fiveYrPrmo,levels=c(0,1),
                     labels=c("Did Not Get Promoted","Did Get Promoted")) 

hr$wrkAcdntFactor <- factor(wrkAcdnt,levels=c(0,1),
                      labels=c("No Accident","Accident")) 

attach(hr)


#density plot 
qplot(avgHrs/4, data=hr, geom="density", fill=leftFactor, alpha=I(.5), 
      main="Avg Weekly Hours by Retention", xlab="Average Weekly Hours", 
      ylab="Density")


#boxplot
boxplot(avgHrs~job,data=hr, main="HR Data",
        xlab="Job Title", ylab="Avg Hours", col="lightblue") 

#violin plot
hrBox <-ggplot(hr, aes(y=avgHrs, x=job)) 
hrBox + geom_violin(trim=FALSE, fill="lightblue") 

#many dimension charts
qplot(avgHrs/4, timeCpny, data=hr, shape=leftFactor, color=salary, facets=numProj~promoFactor, size=I(3),
      xlab="avg hrs", ylab="time at company") 

hrScat <-ggplot(hr, aes(x=avgHrs/4, y=satLevel))
hrScat + geom_point()
#make the points more transparent so that it's less intense
hrScat + geom_point(alpha=.01)
hrScat + stat_bin2d

hrScat + stat_binhex()
hrScat + stat_binhex() + scale_fill_gradient(low="lightblue", high="red")

LEvSL <-ggplot(hr, aes(x=satLevel, y=lastEval))
LEvSL + geom_point()
LEvSL + stat_binhex() + scale_fill_gradient(low="lightblue", high="red")


