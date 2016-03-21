# Question 1
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

library("caret");
library("kernlab");

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
dim(training)
dim(testing)

#Question 2
# Load the cement data using the commands:
# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function 
# in the Hmisc package useful for turning continuous covariates into factors). 
# What do you notice in these plots?

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
summary(concrete)

# Answer
suppressMessages(library(Hmisc))
suppressMessages(library(graphics))
suppressMessages(library(gridExtra))
suppresuppressMessages(library(dplyr))ssMessages(library(ggplot2))

# Get the names of all the columns in the dataset
names(concrete)

# add row index column to training data set
training <- mutate(training, index=1:nrow(training))
names(training)
# [1] "Cement"              "BlastFurnaceSlag"    "FlyAsh"              "Water"              
# [5] "Superplasticizer"    "CoarseAggregate"     "FineAggregate"       "Age"                
# [9] "CompressiveStrength" "index" 

# Using the cut2() function in the Hmisc package useful for turning continuous covariates into factors
cutIndex <- cut2(training$index, g=10)
table(cutIndex)

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Cement
cementPlot <- qplot(index,CompressiveStrength,data=training,color=cut2(training$cement, g=10))
# BlastFurnaceSlag
cementPlot <- qplot(index,CompressiveStrength,data=training,color=cut2(training$BlastFurnaceSlag, g=10))
FlyAshPlot <- qplot(index, CompressiveStrength, data=training, color=cut2(training$FlyAsh, g=10))
grid.arrange( BlastFurnaceSlagPlot, FlyAshPlot, ncol=2)

#WaterPlot <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Water, g=10))
#SuperplasticizerPlot <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Superplasticizer, g=10))
#CoarseAggregatePlot <- qplot(index, CompressiveStrength, data=training, color=cut2(training$CoarseAggregate, g=10))
#FineAggregatePlot <- qplot(index, CompressiveStrength, data=training, color=cut2(training$FineAggregate, g=10))
#AgePlot <- qplot(index, CompressiveStrength, data=training, color=cut2(training$Age, g=10))
#grid.arrange(cementPlot, cementPlot, FlyAshPlot, WaterPlot, SuperplasticizerPlot, CoarseAggregatePlot, FineAggregatePlot, AgePlot,ncol=10)



#5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 80% of the variance. How many are there?


subset = training[,grep("^IL", names(training))]

preProcess(subset, thresh = 0.8, method = "pca")$numComp
