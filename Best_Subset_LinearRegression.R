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
set.seed(1)
##########################
# load the data
###########################
data("prostate")
names(prostate)
?prostate
prostate = prostate[,-c(10)]

set.seed(1907)
train = sample(1:nrow(prostate), 0.8*nrow(prostate))
Y.train = prostate$lpsa[train]
Y.test = prostate$lpsa[-train]
X.train = prostate[train,]
X.test = prostate[-train,]



#########LM fit for Best Subset Selection#######
fit <- lm(lpsa ~ ., data = prostate[train,])
fit$coef

pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
test.error
train.error

#training = prostate[train, 1:9]
#testing = prostate[-train, 1:9]

fit1 <- regsubsets(lpsa~., data = X.train, method = "exhaustive", nvmax = 8)
my_summary <- summary(fit1)
names(my_summary)

my_summary$bic

extractAIC(fit) #AIC calculation
which.min(extractAIC(fit))



which.min(my_summary$bic)#Cp says 3 variables is best


#########Cross Validation#######
####10 FOld########

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=10
set.seed (1)
folds=sample (1:k,nrow(prostate),replace =TRUE)

cv.errors.10 = matrix(NA,10,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors.10[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}



rmse.cv.10 = sqrt(apply(cv.errors.10, 2, mean))
rmse.cv.10
which.min(rmse.cv.10)

k=5
folds=sample (1:k,nrow(prostate),replace =TRUE)

cv.errors.5 = matrix(NA,5,8)
for (j in 1:k) {
  best.fit = regsubsets(lpsa~ ., data = prostate[folds != j,], nvmax = 8)
  for (i in 1:8) {
    pred = predict(best.fit, prostate[folds == j, ], id = i)
    cv.errors.10[j, i] = mean((prostate$lpsa[folds == j] - pred)^2)
  }
}



rmse.cv.5 = sqrt(apply(cv.errors.5, 2, mean))
rmse.cv.5
which.min(rmse.cv.5)

#######Bootstrap##########

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
X <- prostate[,1:8]
Y <- prostate[,9]

select = summary(fit_subset)$outmat
select
error_store <- c()
for (i in 1:8){
  # Pull out the model
  temp <- which(select[i,] == "*")
  
  res <- bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store <- c(error_store, res[[3]])
  
}
error_store
