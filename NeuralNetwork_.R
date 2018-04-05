install.packages("ElemStatLearn")
library(ElemStatLearn)
data(spam)

library(ISLR)
library(MASS)
library(class)

install.packages("lasso2")
library(neuralnet)

library(lasso2)

data(spam)
set.seed(1907)
spam_data = data.frame(spam)

spam_data$spam = ifelse(spam_data$spam=="spam", 1, 0)

xnam <- paste0("A.", 1:57)
f <- as.formula(paste("spam ~ ", paste(xnam, collapse= "+")))

k = 7
cv.error <- c()
  
for(i in 1:k){
  train <- sample(1:nrow(spam_data), .70*nrow(spam_data))
  spam_train <- spam_data[train,]
  spam_test <- spam_data[-train,]

  
  
  nn <- neuralnet(f,data=spam_data,hidden=1 , err.fct = 'ce',linear.output= FALSE)
  
  pr.nn <- compute(nn,spam_test[,1:57])
  pr.nn <- pr.nn$net.result*(max(spam_data$spam)-min(spam_data$spam))+min(spam_data$spam)
  
  test.cv.r <- (spam_test$spam)*(max(spam_test$spam)-min(spam_test$spam))+min(spam_test$spam)
  
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(spam_test)
  
  #pbar$step()
}


cv.error
plot(cv.error,xlab='MSE CV',col='cyan',border='blue',names='CV error (MSE)',main='CV error (MSE) for NN',horizontal=TRUE)

train.error = c()
test.error = c()
set.seed(1907)
# Testing and Cross validating (may take a while to compute)
for(i in 1:57)
{
  # Fit the net and calculate training error (point estimate)
  nn = neuralnet(f,data=spam_data,hidden=c(i),linear.output=F)
 train.error[i] = mean(((as.data.frame(nn$net.result)*(50-10)+10) - (spam$spam*(50-10)+10))^2)
  
  # Calculate test error through cross validation
  test.error[i]= crossvalidate(data_,hidden_l=c(i))
  
}


test.error
train.error

# Plot train error
x11()
plot(train.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Train error MSE',type='l',col='red',lwd=2)
# Plot test error
x11()
plot(test.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Test error MSE',type='l',col='blue',lwd=2)

# Number of neurons (index) that minimizes test/train error
which(min(test.error) == test.error)
which(min(train.error) == train.error)
