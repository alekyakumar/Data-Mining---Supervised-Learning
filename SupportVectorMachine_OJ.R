install.packages("ISLR")
library(ISLR)
install.packages("e1071")
library(e1071)
?OJ
data(OJ)
attach(OJ)

split <- sample(1:nrow(OJ), 0.33*nrow(OJ))
test_oj <- OJ[split,]
train_oj <- OJ[-split,]

linear.test.error <- c()
linear.train.error <- c()

#######Fitting SVM with Linear Kernel########
for (i in c(0.01,0.1, 1, 5, 10)){
  oj.linear.svm <- tune(svm, Purchase ~ .,data = train_oj, kernel = "linear",
                           ranges = list(cost = i))


  best_model_oj <- oj.linear.svm$best.model
  best_model_oj

#predict the test data
  y_hat <- predict(best_model_oj, newdata = test_oj)
  y_true <- test_oj$Purchase

  test_err <- length(which(y_true != y_hat))/length(y_true)

  linear.test.error<-c(linear.test.error,test_err)

  y_hat <- predict(best_model_oj, newdata = train_oj)
  y_true <- train_oj$Purchase

  train_err <- length(which(y_true != y_hat))/length(y_true)

  linear.train.error<-c(linear.train.error,train_err)
}
linear.test.error
linear.train.error

### Plot the results
upper.lr = max(linear.test.error, linear.train.error)
lower.lr = min(linear.test.error, linear.train.error)

x11()
plot(linear.train.error, type = "o", lty = 2, col = "green", ylim = c(lower.lr -1, upper.lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors")
lines(linear.test.error, type = "o", lty = 1, col = "violet")
legend("topright", c("training", "test"), lty = c(2,1), col = c("green","violet"))


#############Radial Kernel############
radial.train.error <- c()
radial.test.error<- c()
for (i in c(0.01,0.1, 1, 5, 10)){
  oj.radial.svm <- tune(svm, Purchase ~ .,data = train_oj, kernel = "linear",
                        ranges = list(cost = i))
  
  
  best_model_oj <- oj.radial.svm$best.model
  best_model_oj
  
  #predict the test data
  y_hat <- predict(best_model_oj, newdata = test_oj)
  y_true <- test_oj$Purchase
  
  test_err <- length(which(y_true != y_hat))/length(y_true)
  
  radial.test.error<-c(radial.test.error,test_err)
  
  y_hat <- predict(best_model_oj, newdata = train_oj)
  y_true <- train_oj$Purchase
  
  train_err <- length(which(y_true != y_hat))/length(y_true)
  
  radial.train.error<-c(radial.train.error,train_err)
}
radial.test.error
radial.train.error

### Plot the results
upper.lr = max(radial.test.error, radial.train.error)
lower.lr = min(radial.test.error, radial.train.error)

x11()
plot(radial.train.error, type = "o", lty = 2, col = "blue", ylim = c(lower.lr -1, upper.lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors for Radial Kernel")
lines(radial.test.error, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","red"))

table(predict = y_hat_rad, truth = y_true)

###########Polynimal Kernel#########
poly.train.error <- c()
poly.test.error<- c()
for (i in c(0.01,0.1, 1, 5, 10)){
  oj.poly.svm <- tune(svm, Purchase ~ .,data = train_oj, degree = 2, kernel = "polynomial",
                        ranges = list(cost = i))
  
  
  best_model_oj <- oj.poly.svm$best.model
  best_model_oj
  
  #predict the test data
  y_hat <- predict(best_model_oj, newdata = test_oj)
  y_true <- test_oj$Purchase
  
  test_err <- length(which(y_true != y_hat))/length(y_true)
  
  poly.test.error<-c(poly.test.error,test_err)
  
  y_hat <- predict(best_model_oj, newdata = train_oj)
  y_true <- train_oj$Purchase
  
  train_err <- length(which(y_true != y_hat))/length(y_true)
  
  poly.train.error<-c(poly.train.error,train_err)
}
poly.test.error
poly.train.error

### Plot the results
upper.lr = max(poly.test.error, poly.train.error)
lower.lr = min(poly.test.error, poly.train.error)

x11()
plot(poly.train.error, type = "o", lty = 2, col = "yellow", ylim = c(lower.lr -1, upper.lr +1) , xlab = "cost", ylab = "error", main = "Test and Training Errors for Radial Kernel")
lines(poly.test.error, type = "o", lty = 1, col = "orange")
legend("topright", c("training", "test"), lty = c(2,1), col = c("yellow","orange"))
