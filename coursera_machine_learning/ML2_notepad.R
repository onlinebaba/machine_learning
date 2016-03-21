library("caret");
library("kernlab");
data(spam);

# Split the data set into training and test sets, split based on the type that is 75% allocated to training set and rest to testing set
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE);
training <- spam[inTrain,];
testing <= spam[-inTrain,];
dim(training);

# option 2 is create multiple data sets, create 10 folds, return each set of the indices as a list and return the training set
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE)
sapply(folds,length)
folds[[1]][1:10]

folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE)
sapply(folds,length)
folds[[1]][1:10]

# option 3 is to create resampling with replacement, resample the data 10 times
set.seed(32323)
folds <- createResample(y=spam$type, times = 10, list=TRUE)
sapply(folds, length)
# you may get same values back because we are resampling replacement with values
folds[[1]][1:10]

# option 4, create timeslices. Create slices of 20 samples in first window and 10 samples in the next window
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

## Training Options
# Split the data set into training and test sets, split based on the type that is 75% allocated to training set and rest to testing set
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE);
training <- spam[inTrain,];
testing <= spam[-inTrain,];
dim(training);
modelFit <- train(type~., data=training, method="glm")

#args(train.default)

## PLOTTING PREDICTORS
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)
# get training and testing sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x=training[, c("age", "education","jobclass" )], y= training$wage, plot="pairs")
qplot(age, wage, data=training, color=jobclass)
qq <- qplot(age, wage,color=education, data=training)
qq + geom_smooth(method='lm', formula=y~x)

# cut2, making factors
library(Hmisc)
library(graphics)
cutWage <- cut2(training$wage, g=3)
table(cutWage)
# Boxplots with cut2
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
p1
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

# Tables
t1 <- table(cutWage, training$jobclass)
t1
# Proportions in each groups
prop.table(t1,1)

#density plots
qplot(wage, color=education, data=training, geom="density")

##### Basic Preprocessing

# get training and testing sets
library("caret");
library("kernlab");
data(spam);
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="", xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

# Standardizing - preProcess function
preObj <- preProcess(training[,-58], method=c("center","scale"))
mean(preObj)
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

trainCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

# preProcess argument
set.seed(32343)
modelFit <- train(type~., data=training, preProcess=c("center","scale"), method="glm")
modelFit

# Standardizing - Box-Cox transforms, take continous data and make them look like normal data
preObj <- preProcess(training[,-58], method="BoxCox")
trainCapsAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapsAveS);
qqnorm(trainCapsAveS)

