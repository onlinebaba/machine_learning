# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
summary(segmentationOriginal)
names(segmentationOriginal)
table(segmentationOriginal$Case)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set.

inTrain <- createDataPartition(y=segmentationOriginal$Case, p=0.50, list=FALSE);
training <- segmentationOriginal[inTrain,];
testing <- segmentationOriginal[-inTrain,];
dim(training);

# Set the seed to 125 and fit a CART (Classification and Regression Trees) model with the rpart method using
# all predictor variables and default caret settings.
table(segmentationOriginal$Class)
set.seed(125)
modFit <- train(Class ~., method="rpart", data=training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
library(rattle)
fancyRpartPlot(modFit$finalModel)

# Question2: If K is small in a K-fold cross validation, is the bias in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger? 
# If K is small, is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 
# Is K large or small in leave one out cross validation?


# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

# These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
# Fit a classification tree where Area is the outcome variable. Then predict the value of area for the 
# following data frame using the tree command with all defaults

newdata = as.data.frame(t(colMeans(olive)))

names(olive)
modFit <- train(Area ~ ., method = "rpart", data=olive)
print(modFit$finalModel)
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit,newdata=newdata)

# Question 4 Load the South Africa Heart Disease Data and create training and test sets with the 
# following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Then set the seed to 13234 and fit a logistic regression model 
# (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) 
# as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, 
# type-A behavior, and low density lipoprotein cholesterol as predictors. 
# Calculate the misclassification rate for your model using this function and a prediction on 
# the "response" scale:
# missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
modelFit <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl, data=trainSA , method="glm", family="binomial")
modelFit
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# What is the misclassification rate on the training set? 
missClass(trainSA$chd,predict(modelFit,newdata=trainSA))
# What is the misclassification rate on the test set?
missClass(testSA$chd,predict(modelFit,newdata=testSA))  
  
# Question 5: Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set.
# Then set the seed to 33833.
# Fit a random forest predictor relating the factor variable y to the remaining variables.
# Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
# The caret package uses by default the Gini importance.
# Calculate the variable importance using the varImp function in the caret package. 
# What is the order of variable importance?

# Set the variable y to be a factor variable in both the training and test set.
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

# Then set the seed to 33833.
set.seed(33833)
library(randomForest)
  modvowel <- randomForest(y ~ ., data = vowel.train)
  order(varImp(modvowel), decreasing = T)




