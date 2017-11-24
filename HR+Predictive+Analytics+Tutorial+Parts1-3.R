
# If you are using local R install, you can use the following line of code to import
# hr = read.table("/filepath/HR_comma_sep.csv", head=TRUE, sep=",")

#If you are using DSX, import the file place your cursor in cell and "insert to code" as per the instructions here: 
#https://www.littlemissdata.com/blog/predictive-analytics-tutorial-part-1
#Note: you need to load your data every time you open your notebook. 


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
install.packages("waffle")
install.packages("dummies")
install.packages("caTools")



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
library(waffle)
library(dummies)
library(caTools)


#attach allows us to reference columns by their name 
attach(hr)
#Check Correlations of numeric columns
corMatrix <-cor(hr[1:8], use="complete.obs", method="pearson") 
#round to two decimals
round(corMatrix, 2)

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

#Create the binary variable for highly rated but low satisfaction users

hr$greatEvalLowSat <- ifelse(lastEval>0.8 & satLevel <0.2, 1, 0)

#Visualize effects that it has on average hours
attach(hr)

x <-ggplot(hr, aes(factor(greatEvalLowSat), avgHrs))
x + geom_boxplot (aes(fill=factor(greatEvalLowSat)), outlier.color="red", outlier.shape = "star", outlier.size=4)


#Visualize distribution
hist(greatEvalLowSat, main="Distribution of New Variable: greatEvalLowSat", xlab="Great Eval and Low Sat", breaks=7, col="")


#Visualize distribution by job
ggplot(hr, aes(avgHrs, job, z= greatEvalLowSat)) + geom_tile(aes(fill = greatEvalLowSat)) + theme_bw() +   scale_fill_gradient(low="#c2dfff", high="#b7ceec") 


#Get distribution table
summary <- count(avgHrs, c('greatEvalLowSat'))

summary

#Visualize distribution proportions in a waffle chart.  Divide by 50 simply for visual effects.  
wafflechart <- c(`True - High Evaluation and Low Satisfaction`= 895/50, `False - High Evaluation and Low Satisfaction`=14104/50)
waffle(wafflechart, rows=15, size=1, colors=c("#41b3a3", "#c38d9e"), legend_pos="bottom") 


#Create the binary variable for highly rated but low satisfaction users

hr$lowSatHighHours <- ifelse(avgHrs/4>60 & satLevel <0.1, 1, 0)
attach(hr)

#Visualize effects on average hours worked
x <-ggplot(hr, aes(factor(lowSatHighHours), avgHrs))
x + geom_boxplot (aes(fill=factor(lowSatHighHours)), outlier.color="red", outlier.shape = "star", outlier.size=4)


#Visualize distribution 
hist(lowSatHighHours, main="Distribution of New Variable: lowSatHighHours", xlab="Low Sat High Hours", breaks=7, col="#b7ceec")


ggplot(data = hr) + 
  geom_point(mapping = aes(x = avgHrs/4, y = leftFactor)) + 
  facet_wrap(~lowSatHighHours)

#Get distribution table
summary <- count(avgHrs, c('lowSatHighHours'))

summary


#Visualize distribution proportions in a waffle chart.  Divide by 50 simply for visual effects.  
wafflechart <- c(`True - High Hours and Low Satisfaction`= 193/50, `False - High Hours and Low Satisfaction`=14806/50)

waffle(wafflechart, rows=10, size=1, colors=c("#41b3a3", "#c38d9e"), legend_pos="bottom") 

#create dummy variables for job and salary  All values need to be numeric for inclusion in the model
hr2 <- cbind (hr, dummy(job), dummy(salary))

#Ensure dummy variables are created
names(hr2)
head(hr2)

#Focus on satLevl and lastEval because they have the widest distribution of values

#Log Transforms for: satLevel, lastEval

hr2$satLevelLog <- log(satLevel)
hr2$lastEvalLog <- log(lastEval)


#SQRT Transforms for: satLevel, lastEval

hr2$satLevelSqrt <- sqrt(satLevel)
hr2$lastEvalSqrt <- sqrt(lastEval)

#Scale Transforms for: satLevel, lastEval

hr2$satLevelScale <- scale(satLevel)
hr2$lastEvalScale <- scale(lastEval)

#Peek at new columns
head(hr2)

#Ensure all columns present
names(hr2)

#Starting code credit to: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
set.seed(101) 
sample = sample.split(hr2, SplitRatio = .75)
train = subset(hr2, sample == TRUE)
test  = subset(hr2, sample == FALSE)


#See if the test/train sets have beens split appropriately
dim(train)
dim(test)

#Ensure data characteristics are roughly the same
summary(train)
summary(test)


