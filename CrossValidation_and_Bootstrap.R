rm(list = ls())
library(ISLR)
library(MASS)
library(class)
library(ElemStatLearn)
#install.packages("bootstrap")
library(neuralnet)
library(Metrics)
install.packages("lasso2")
library(lasso2)
library(boot)
library(bootstrap)
library(leaps)
###########################
# load the data
###########################
data(prostate)
names(prostate)
?prostate
prostate = prostate[,-c(10)]

# The hold out method

set.seed(1907)
train = sample(1:nrow(prostate), 0.75*nrow(prostate))
Y.train = prostate$lpsa[train]
Y.test = prostate$lpsa[-train]
X.train = prostate[train,]
X.test = prostate[-train,]

fit <- lm(lpsa ~ ., data = X.train)
pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
test.error
train.error
extractAIC(fit) #AIC
#############################################
# Utilize the hold-method over a spectrum
# of complexity paramters
#
#############################################
fit1 <- regsubsets(lpsa~., data = X.train, method = "exhaustive", nvmax = 8)
my_summary <- summary(fit1)
names(my_summary)
my_summary$cp
my_summary$bic
which.min(my_summary$cp) 
which.min(my_summary$bic) 

# Do the selection based on the "hold out method"
select = summary(fit1)$outmat
train.error.store <- c()
test.error.store <- c()
for (i in 1:8){
  temp = which(select[i,] == "*")
  temp = temp + 1
  
  red.training = X.train[, c(9,temp)]
  red.testing = X.test[,c(9,temp)]
  
  red.fit = lm(lpsa~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training)
  pred.test = predict(red.fit, newdata = red.testing)
  
  test.error = (1/length(Y.test))*sum((pred.test - Y.test)^2)
  train.error = (1/length(Y.train))*sum((pred.train - Y.train)^2)
  
  train.error.store = c(train.error.store, train.error)
  test.error.store = c(test.error.store, test.error)
  
}

train.error.store 
test.error.store
#############################
## Calculate bootstrap predition error 
## for the best models of size "k"
###########################################
# create functions that feed into "bootpred"
beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create X and Y
X = prostate[,]
Y = prostate[,9]

# Practice, WLOG lets look at a single model
#select = summary(fit1)$outmat
#temp <- which(select[4,] == "*") # best 3 variable model

#res1 = bootpred(X[,temp], Y, nboot = 500, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 

# Generalize it, and search over the best possible subsets of size "k"
error_store = c()
for (i in 1:8){
  # Pull out the model
  temp = which(select[i,] == "*")
  
  res = bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store = c(error_store, res[[3]])
  
}
error_store
upper = max(train.error.store, test.error.store)
lower = min(train.error.store, test.error.store)
x11()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
lines(error_store, type = "o", lty = 3, col = "green")
legend("topright", c("train", "test", "bootstrap .632"), lty = c(2,1), col = c("blue", "red", "green"))

######################################################################################
# 5-fold CV for model selection
######################################################################################
set.seed (1907)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=5
set.seed (1987897)
folds=sample (1:k,nrow(prostate),replace =TRUE)

cv.errors = matrix(NA,5,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
rmse.cv
which.min(rmse.cv)# 5-fold CV indicates for 7-variable model

######################################################################################
# 10-fold CV for model selection
######################################################################################
set.seed (1907)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=10
set.seed (197)
folds=sample (1:k,nrow(prostate),replace =TRUE)

cv.errors = matrix(NA,10,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
rmse.cv
which.min(rmse.cv)# 10-fold CV indicates for 3-variable model

