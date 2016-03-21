# Question 1 Combining Classifiers
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set.
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

# Fit these both with the train() command in the caret package.
# Fit (1) a random forest predictor relating the factor variable y to the remaining variables and
# library(randomForest)
randomForestPredictorModel <- train(y ~ ., method="rf", data = vowel.train,trControl=trainControl(method="cv"), number=3)
print(randomForestPredictorModel)

# (2) a boosted predictor using the "gbm" method. 
gbmPredictorModel <-  train(y ~., method="gbm", data = vowel.train,verbose=FALSE)
print(gbmPredictorModel)

randomForestResult <- predict(randomForestPredictorModel, vowel.test)
gbmResult <- predict(gbmPredictorModel, vowel.test)

#Calculate a cross-tabulation of observed and predicted classes with associated statistics.
confusionMatrix(vowel.test$y, randomForestResult)$overall['Accuracy']
confusionMatrix(vowel.test$y, gbmResult)$overall['Accuracy']

idx_agreed <- (randomForestResult == gbmResult)
confusionMatrix(vowel.test$y[idx_agreed], randomForestResult[idx_agreed])$overall['Accuracy']

# What are the accuracies for the two approaches on the test data set?

# What is the accuracy among the test set samples where the two methods agree?