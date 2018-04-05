setwd("/Users/Alekya Kumar/Desktop/Data Sciences - Sem 1/Stats/R work")

rm(list=ls())
install.packages("ISLR")
library(ISLR)

# install some CRAN libararies 

install.packages("MASS")
install.packages("caret")

install.packages("gmlnet")
install.packages("ggplot2")
install.packages('e1071',dependencies = TRUE)

# load libarary 

library(class)
library(MASS)
library(caret)
library(leaps)
library()
#?Boston

boston_data <- read.delim("boston.csv" , sep = "\t" , header = TRUE)
data(Boston)
boston_data <- subset(Boston)

##############Training and Test Data Creation##############
set.seed(1907)
boston_data$crim <- ifelse(boston_data$crim > median(boston_data$crim), 1,0)
#boston_data$crim <- as.factor(boston_data$crim)
train <- sample(1:nrow(boston_data), .70*nrow(boston_data))
boston_train <- boston_data[train,]
boston_test <- boston_data[-train,]
y_true_train <- boston_train$crim
y_true_test <- boston_test$crim

#########################################
# Logistic Regression
#########################################

glm.fit <- glm(crim ~., data = boston_train)
plot(glm.fit)
summary(glm.fit)
names(glm.fit)

#######Predict############
y_hat_train<- predict(glm.fit, newdata = boston_train, type = "response")
y_hat_train <- round(y_hat_train)
y_hat_test <- predict(glm.fit, newdata = boston_test, type = "response")
y_hat_test <- round(y_hat_test)

#  Calculate the error rates
########################################
train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
train_err
test_err <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)
test_err

train_err

test_err

#########################################
#  Confusion Matrix
########################################
conf <- confusionMatrix(y_hat_test, y_true_test)
conf$table

#############################################
#Logistic Regression using hold-out method
#############################################

fit <- regsubsets(crim~., data = boston_train, method = "exhaustive", nvmax = 14)
select = summary(fit)$outmat
select

#########Fitting Model
train.error.store.lr <- c()
test.error.store.lr <- c()
for (i in 1:13){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- boston_train[, c(1,temp)]
  red.testing <- boston_test[,c(1,temp)]
  
  red.fit <- glm(crim~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training)
  pred.test = predict(red.fit, newdata = red.testing)
  
  test.error.lr <- (1/length(y_true_test))*sum((pred.test - y_true_test)^2)
  train.error.lr <- (1/length(y_true_train))*sum((pred.train - y_true_train)^2)
  
  train.error.store.lr <- c(train.error.store.lr, train.error.lr)
  
  test.error.store.lr <- c(test.error.store.lr, test.error.lr)
  
}
train.error.store.lr.df <- data.frame(train.error.store.lr)
test.error.store.lr.df <- data.frame(test.error.store.lr)


### Plot the results
upper.lr = max(train.error.store.lr, test.error.store.lr)
lower.lr = min(train.error.store.lr, test.error.store.lr)

x11()
plot(train.error.store.lr, type = "o", lty = 2, col = "blue", ylim = c(lower.lr -1, upper.lr +1) , xlab = "k", ylab = "error", main = "Model Selection for Logistic Regression")
lines(test.error.store.lr, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

#############Linear Discriminat Analysis########
lda.fit = lda(crim~., data = boston_train)
plot(lda.fit)
y_hat_train_lda = predict(lda.fit, newdata = boston_train)

y_hat_train_lda <- as.numeric(y_hat_train_lda$class)-1 ### Since the class is 1 and 2 
y_hat_test_lda = predict(lda.fit, newdata = boston_test)

y_hat_test_lda <- as.numeric(y_hat_test_lda$class)-1 ### Since the class is 1 and 2 

# Compute the error
train_err_lda <- sum(abs(y_hat_train_lda- y_true_train))/length(y_true_train)
train_err_lda
test_err_lda <- sum(abs(y_hat_test_lda- y_true_test))/length(y_true_test)
test_err_lda

#########Confusion Matrix######
conf_lda <- confusionMatrix(y_hat_test_lda, y_true_test)
conf_lda$table

#############################################
#Linear Discriminant Analysis using hold-out method
#############################################

#########Fitting Model
train.error.store.lda <- c()
test.error.store.lda <- c()
for (i in 1:13){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- boston_train[, c(1,temp)]
  red.testing <- boston_test[,c(1,temp)]
  
  red.lda.fit <- lda(crim~., data = red.training)
  
  pred.train.lda = predict(red.lda.fit, newdata = red.training)
  pred.test.lda = predict(red.lda.fit, newdata = red.testing)
  
  test.error.lda <- (1/length(y_true_test))*sum((pred.test - y_true_test)^2)
  train.error.lda <- (1/length(y_true_train))*sum((pred.train - y_true_train)^2)
  
  train.error.store.lda <- c(train.error.store.lda, train.error.lda)
  test.error.store.lda <- c(test.error.store.lda, test.error.lda)
  
}

train.error.store.lda.df <- data.frame(train.error.store.lda)
test.error.store.lda.df <- data.frame(test.error.store.lda)

### Plot the results
upper.lda = max(train.error.store.lda, test.error.store.lda)
lower.lda = min(train.error.store.lda, test.error.store.lda)

x11()
plot(train.error.store.lda, type = "o", lty = 1, col = "blue", ylim = c(lower.lda -1, upper.lda +1) , xlab = "k", ylab = "error", main = "Model Selection for Linear Discriminant Analysis")
lines(test.error.store.lda, type = "o", lty = 2, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))


#############KNN Prediction############

knn_predict = knn(boston_train,boston_test,boston_train$crim,k = 10)
y_hat_test_knn <- c(0, 1)[sapply(knn_predict, as.numeric)]

#######Compute Error#########
test_err_knn <- sum(abs(y_hat_test_knn- y_true_test))/length(y_true_test)
test_err_knn

#############Confusion Matrix##########
conf_knn <- confusionMatrix(y_hat_test_knn, y_true_test)
conf_knn$table

#train.error.store.knn <- c()
test.error.store.knn <- c()

for (i in 1:10){
  p <- knn(boston_train,boston_test,boston_train$crim, i)
  y_hat_test_knn <- c(0, 1)[sapply(p, as.numeric)]
  
  test.error.knn <- sum(abs(y_hat_test_knn- y_true_test))/length(y_true_test)
  
  #train.error.store.knn <- c(train.error.store.knn, train.error.lda)
  test.error.store.knn <- c(test.error.store.knn, test.error.knn)
}

test.error.store.knn.df <- data.frame(test.error.store.knn)
test.error.store.knn.df

### Plot the results
upper.knn = max(test.error.store.knn)
lower.knn = min(test.error.store.knn)

x11()
plot(test.error.store.knn, type = "o", lty = 2, col = "blue", ylim = c(lower.knn -1, upper.knn +1) , xlab = "k", ylab = "error", main = "Model Selection")
#lines(test.error.store.knn, type = "o", lty = 1, col = "red")
legend("topright", c("test"), lty = c(2,1), col = c("blue"))

