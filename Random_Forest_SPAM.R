rm (list=ls())



library(kernlab)
library(ggplot2)
library(randomForest)
spamData = data(spam)
spamData = spam
set.seed(1907)
train <- sample(1:nrow(spamData), .66*nrow(spamData))
spam_train <- spamData[train,]
spam_test <- spamData[-train,]

#Error holder
modelName = c()
test_err = c()
train_err = c()

for (i in 1:15){
  spamBAG = randomForest(type~.,data = spam_train, n.tree =1000, mtry = i)
  
  spamBAGPred_test <- predict(spamBAG, newdata = spam_test,type='class')
  spamBAGPred_train <- predict(spamBAG, newdata = spam_train, type='class')
  
  spamBAGPred_test <- predict(spamBAG, newdata = spam_test,type='class')
  spamBAGPred_train <- predict(spamBAG, newdata = spam_train, type='class')
  
  BAG_test_err <- mean(spamBAGPred_test != spam_test$type)
  BAG_train_err <- mean(spamBAGPred_train != spam_train$type)
  
  modelName = c(modelName,i)
  test_err = c(test_err,BAG_test_err)
  train_err = c(train_err,BAG_train_err)
}

error = data.frame(Model_Name = modelName,Training_Error = train_err,Test_Error = test_err)

x11()
plot(error$Model_Name,error$Test_Error,type='o')
