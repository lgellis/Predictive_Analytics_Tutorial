
# If you are using local R install:
    #You can download the file from https://raw.githubusercontent.com/lgellis/Predictive_Analytics_Tutorial
    #and use the following line of code to import: 
    #hr = read.table("/filepath/HR_comma_sep.csv", head=TRUE, sep=",")

    #OR 
    #You can import the file directly from the github repo with these line of code:
    #install.packages("data.table")
    #library(data.table)
    #hr= fread('https://raw.githubusercontent.com/lgellis/Predictive_Analytics_Tutorial/master/HR_comma_sep.csv')

#If you are using DSX, import the file place your cursor in cell and "insert to code" as per the instructions here: 
#https://www.littlemissdata.com/blog/predictive-analytics-tutorial-part-1
head(df.data.2)


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
install.packages("wesanderson")
install.packages("visreg")
install.packages("car")
install.packages("leaps")
install.packages("MASS")

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
library(wesanderson)
library(visreg)
library(car)
library(rpart)
library(leaps)
library(MASS)


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
corrplot(corMatrix, type = "upper")
corrplot.mixed(corMatrix)


#rename
colNames <- c("satLevel", "lastEval", "numProj", "avgHrs", "timeCpny", "wrkAcdnt", "left", "fiveYrPrmo", "job", "salary")
setnames(hr, colNames)
names(hr)
attach(hr)

corrplot(corMatrix, order = "hclust", addrect = 2, col = heat.colors(100), tl.col = "black")


# Change background color to lightblue
corrplot(corMatrix, type = "upper", order = "hclust", col = c("black", "white"), bg = "lightblue", tl.col = "black")


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


attach(hr)

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

# Example of a Bagplot
#install.packages("aplpack")
library(aplpack)

bagplot(avgHrs/4, satLevel, xlab="Avg Weekly Hrs", ylab="SatLevel",
  main="Bagplot Avg Hours by Sat Level")

attach(hr)
typeof(hr)
hr <- as.data.frame(hr)
typeof(hr)

#create dummy variables for job and salary  All values need to be numeric for inclusion in the model
hr2 <- cbind (hr, dummy(job), dummy(salary))


#Ensure dummy variables are created
names(hr2)
head(hr2)

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

#Create the binary variable for highly rated but low satisfaction users

hr2$greatEvalLowSat <- ifelse(lastEval>0.8 & satLevel <0.2, 1, 0)

#Visualize effects that it has on average hours
attach(hr2)

x <- ggplot(hr2, aes(factor(greatEvalLowSat), avgHrs))
x <- x + geom_boxplot (aes(fill=factor(greatEvalLowSat)), outlier.color="black",  outlier.size=1) 
x <- x + coord_flip() 
x + scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2"))

#Visualize distribution by job
ggplot(hr, aes(avgHrs, job, z= greatEvalLowSat)) + geom_tile(aes(fill = greatEvalLowSat)) + theme_bw() +   scale_fill_gradient(low="#98afc7", high="#646d7e") 


#Get distribution table
summary <- count(avgHrs, c('greatEvalLowSat'))

summary

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

#1) Throw everything in!
model.lr1 <- lm(avgHrs ~ ., train)
summary(model.lr1)
#adj rsquared - higher the better
summary(model.lr1)

#Adj R-squared - higher is better
#AIC, BIC - lower the better
aic <- AIC(model.lr1)
bic <-BIC(model.lr1)
pred.model.lr1 <- predict(model.lr1, newdata = test) # validation predictions
# mean Prediction Error and Std Error - lower is better
meanPred <- mean((test$avgHrs - pred.model.lr1)^2) # mean prediction error
stdError <- sd((test$avgHrs - pred.model.lr1)^2)/length(test$avgHrs) # std error

str(summary(model.lr1))

### Create a data frame to store the model metrics

# Note stringsAsFactors = FALSE
modelMetrics <- data.frame( "Model" = character(), "adjRsq" = integer(),  "AIC"= integer(), "BIC"= integer(), "Mean Prediction Error"= integer(), "Standard Error"= integer(), stringsAsFactors=FALSE)

# Append a row
modelMetrics[nrow(modelMetrics) + 1, ] <- c( "model.lr1", 0.279, aic, bic, meanPred, stdError)
modelMetrics

#2) Backward selection

step <- stepAIC(model.lr1, direction="backward")
step$anova # display results


model.bkwd.lr1 <- lm(avgHrs ~ satLevel + lastEval + numProj + timeCpny + left + satLevelLog + 
    lastEvalLog + satLevelSqrt + lastEvalSqrt, train)
summary(model.bkwd.lr1)

#Adj R-squared - higher is better
#AIC, BIC - lower the better
aic <- AIC(model.bkwd.lr1)
bic <-BIC(model.bkwd.lr1)
pred.model.bkwd.lr1 <- predict(model.bkwd.lr1, newdata = test) # validation predictions
# mean Prediction Error and Std Error - lower is better
meanPred <- mean((test$avgHrs - pred.model.bkwd.lr1 )^2) # mean prediction error
stdError <- sd((test$avgHrs - pred.model.bkwd.lr1 )^2)/length(test$avgHrs) # std error

# Append a row to our modelMetrics 
modelMetrics[nrow(modelMetrics) + 1, ] <- c( "model.bkwd.lr1", 0.2795, aic, bic, meanPred, stdError)
modelMetrics

#3) Create a Model with the highest Correlated Values
#Pull a correlation plot on all of the variables to see what are the most correlated to avgHrs
#remove the non-numeric
corData <- train[ -c(9:13) ]
head(corData)


corMatrix <-cor(corData, use="complete.obs", method="pearson") 
#round to two decimals
corrplot(corMatrix, type = "upper", order = "hclust", col = c("black", "white"), bg = "lightblue", tl.col = "black")
#some of the highest correlated are: numProj, lastEvalLog, timeCmpy, left, greatEvalLowSat

# Create a Model with the highest Correlated Values
model.lr2 <- lm(avgHrs ~ numProj + lastEvalLog + timeCpny + left + greatEvalLowSat, train)
summary(model.lr2)


#Adj R-squared - higher is better
#AIC, BIC - lower the better
aic <- AIC(model.lr2)
bic <-BIC(model.lr2)
pred.model.lr2 <- predict(model.lr2, newdata = test) # validation predictions
# mean Prediction Error and Std Error - lower is better
meanPred <- mean((test$avgHrs - pred.model.lr2 )^2) # mean prediction error
stdError <- sd((test$avgHrs - pred.model.lr2 )^2)/length(test$avgHrs) # std error

# Append a row to our modelMetrics 
modelMetrics[nrow(modelMetrics) + 1, ] <- c( "model.lr2",  0.2353, aic, bic, meanPred, stdError)
modelMetrics

#4) Best Subsets

model.bestSub=regsubsets(avgHrs ~ ., train, nvmax =25)

summary(model.bestSub)

reg.summary =summary(model.bestSub)

which.min (reg.summary$bic )
which.max (reg.summary$adjr2 )#just for fun


#Plot the variable adjusted r squared values by number of variables
plot(reg.summary$BIC ,xlab=" Number of Variables ",ylab=" BIC",type="l")
points (6, reg.summary$adjr2 [6], col =" red",cex =2, pch =20)

coef(model.bestSub, 6)

bestSubModel = lm(avgHrs ~ satLevel + numProj + timeCpny + NAsupport + NAtechnical + NAhigh,  data=train)

bestSubModel
summary(bestSubModel)

#Adj R-squared - higher is better
#AIC, BIC - lower the better
aic <- AIC(bestSubModel)
bic <-BIC(bestSubModel)
pred.bestSubModel <- predict(bestSubModel, newdata = test) # validation predictions
# mean Prediction Error and Std Error - lower is better
meanPred <- mean((test$avgHrs - pred.bestSubModel )^2) # mean prediction error
stdError <- sd((test$avgHrs - pred.bestSubModel )^2)/length(test$avgHrs) # std error

# Append a row to our modelMetrics 
modelMetrics[nrow(modelMetrics) + 1, ] <- c( "bestSubModel",  0.1789, aic, bic, meanPred, stdError)
modelMetrics

#standard model plots
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(model.bkwd.lr1)


par(mfrow = c(1, 1))
#qq plot
qqnorm(resid(model.bkwd.lr1))
qqPlot(model.bkwd.lr1)

#residual plot

#Cooks Distance
cutoff <- 4/((nrow(train)-length(model.bkwd.lr1$coefficients)-2))
plot(model.bkwd.lr1, which=4, cook.levels=cutoff)

#visualize the model
visreg2d(model.bkwd.lr1, "numProj", "lastEvalLog", plot.type="persp" )

visreg2d(model.bkwd.lr1, "numProj", "lastEvalLog", plot.type="image" )

visreg(model.bkwd.lr1, "numProj")


#import in the data set 
currentEmployees <- fread('https://raw.githubusercontent.com/lgellis/Predictive_Analytics_Tutorial/master/currentEmployees.csv')
currentEmployees
typeof(currentEmployees)
#convert to dataframe
currentEmployees <-as.data.frame(currentEmployees)


#Add the existing data transformations

colNames <- c("satLevel", "lastEval", "numProj", "timeCpny", "wrkAcdnt", "left", "fiveYrPrmo", "job", "salary")
setnames(currentEmployees, colNames)
attach(currentEmployees)
currentEmployees



#create factor variables with a better format

currentEmployees$leftFactor <- factor(left,levels=c(0,1),
                     labels=c("Did Not Leave Company","Left Company")) 

currentEmployees$promoFactor <- factor(fiveYrPrmo,levels=c(0,1),
                     labels=c("Did Not Get Promoted","Did Get Promoted")) 

currentEmployees$wrkAcdntFactor <- factor(wrkAcdnt,levels=c(0,1),
                      labels=c("No Accident","Accident")) 

attach(currentEmployees)

currentEmployees <- cbind (currentEmployees, dummy(job), dummy(salary))

#Log Transforms for: satLevel, lastEval

currentEmployees$satLevelLog <- log(satLevel)
currentEmployees$lastEvalLog <- log(lastEval)

#SQRT Transforms for: satLevel, lastEval

currentEmployees$satLevelSqrt <- sqrt(satLevel)
currentEmployees$lastEvalSqrt <- sqrt(lastEval)

#Scale Transforms for: satLevel, lastEval

currentEmployees$satLevelScale <- scale(satLevel)
currentEmployees$lastEvalScale <- scale(lastEval)

currentEmployees$greatEvalLowSat <- ifelse(lastEval>0.8 & satLevel <0.2, 1, 0)

currentEmployees$Predictions <- predict(model.bkwd.lr1, currentEmployees) # test predictions

head(currentEmployees)


#Calculate Overage Hours

currentEmployees$predictedOverageHours = currentEmployees$Predictions - 160

#In case the employees worked less than 160 hours per month set negative values to 0

currentEmployees$netPredictedOverageHours <- currentEmployees$predictedOverageHours 
currentEmployees$netPredictedOverageHours[currentEmployees$netPredictedOverageHours<0] <- 0


head(currentEmployees)


#Calculate total hours worked by each category

lowWageTotal = sum(currentEmployees[currentEmployees$salary=='low',]$predictedOverageHours) 
mediumWageTotal = sum(currentEmployees[currentEmployees$salary=='medium',]$predictedOverageHours) 
highWageTotal = sum(currentEmployees[currentEmployees$salary=='high',]$predictedOverageHours) 


#assume low salary makes $10/hr, medium is $25/hr and high is $50/hr and assume they get time and a half during overtime

#total paid is sum of all predictions of low * 10, medium * 20, high * 40

TotalPaidPerMonth = lowWageTotal*10*1.5 + mediumWageTotal*25*1.5 + highWageTotal*50*1.5
TotalPaidPerMonth
TotalPaidPerYear = TotalPaidPerMonth * 12
TotalPaidPerYear
