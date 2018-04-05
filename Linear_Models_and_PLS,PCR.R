install.packages("ISLR")
install.packages("gmlnet")
install.packages("pls")
library(pls)
library(glmnet)
library(ISLR)
data(College)
#########split into training and test set############
set.seed(1907)
train <- sample(1:nrow(College), round(nrow(College)*0.75))
College.train <- College[train,]
College.test <- College[-train,]


################Linear regression model###############
linear_model <- lm(Apps ~ ., data = College.train)
prediction_tr <- predict.lm(linear_model,College.train)
error_data <- mean((prediction_tr-College.train[,2])^2)

prediction_ts <- predict.lm(linear_model,College.test)
error_data <- mean((prediction_ts-College.test[,2])^2)


##############Ridge Regression#################
clear <- subset(College)
clear_data <- na.omit(College[,1:18])
#X <- as.matrix(clear_data[,2:18])
clear_data$Private = as.numeric(clear_data$Private)
X <- as.matrix(clear_data[,-2])
Y <- clear_data[,2]

ridge.model  = glmnet(X,Y,alpha = 0)
coef(ridge.model)
dim(coef(ridge.model))

ridge.model$lambda[25]
coef(ridge.model)[,25]
l2_norm <- sqrt(sum(coef(ridge.model)[2:16,25]^2))
l2_norm

ridge.model$lambda[75]
coef(ridge.model)[,75]
l2_norm <- sqrt(sum(coef(ridge.model)[2:16,75]^2))
l2_norm

ridge.model$lambda[100]
coef(ridge.model)[,100]
l2_norm <- sqrt(sum(coef(ridge.model)[2:16,100]^2))
l2_norm

predict(ridge.model, s = .0005, type = "coefficient")

##################Model Selection#####################
train_ridge <- sample(1:nrow(X), (nrow(X)*0.75))

cv.out <- cv.glmnet(X[train_ridge,], Y[train_ridge], alpha=0)
plot(cv.out)
names(cv.out)

sel_lambda <- cv.out$lambda.min
sel_lambda

###############Training and Test Prediction########
ridge.pred.train <- predict(ridge.model, s= sel_lambda, type = "coefficients")
ridge.pred.train
#ridge.pred.train.response <- predict(ridge.model, s= sel_lambda, newx = X[train_ridge,], type = "response")
#ridge.pred.train.response

#ridge.pred.test <- predict(ridge.model, s= sel_lambda, type = "coefficients")
#ridge.pred.test
ridge.pred.test.response <- predict(ridge.model, s= sel_lambda, newx = X[-train_ridge,], type = "response")
ridge.pred.test.response


error_data_ridge <- mean((ridge.pred.test.response-College.test[,2])^2)




############################LASSO MODEL#################################
train_lasso <- sample(1:nrow(X), (nrow(X)*0.75))
lasso.model <- glmnet(X[train_lasso,], Y[train_lasso], alpha = 1)

plot(lasso.model)

#########Coefficients checking##############
lasso.model$lambda[70]
coef(lasso.model)[,70]

lasso.model$lambda[10]
coef(lasso.model)[,10]

lasso.model$lambda[100]
coef(lasso.model)[,100]

lasso.model$lambda[82]
coef(lasso.model)[,82]

# selectig the Best Lambda
cv.out = cv.glmnet(X[train_lasso,], Y[train_lasso], alpha = 1)
bestlam = cv.out$lambda.min
bestlam
plot(cv.out)
lasso.model <- glmnet(X[train_lasso,], Y[train_lasso], alpha = 1, lambda = bestlam)

lasso.pred <- predict(lasso.model, s = bestlam, type = "coefficients")
lasso.pred

lasso.pred2 <- predict(lasso.model, s = bestlam, newx = X[-train_lasso,], type = "response")
lasso.pred2

y_hat_lasso <- lasso.pred2
y_true <- Y[-train_lasso]

error_data_lasso <- mean((lasso.pred2-College.test[,2])^2)
error_data_lasso

#################Coefficients not equal to zero ##############
lasso.model <- glmnet(X[train_lasso,], Y[train_lasso], alpha = 1, lambda = bestlam)
sum(coef(lasso.model)[,1] != 0)
names(coef(lasso.model)[,1][coef(lasso.model)[,1] != 0])

#########################PLS and PCA################


#############Test and Trainig  Set ##################
train_pcr <- sample(1:nrow(X), (nrow(X)*0.75))
train_pcr <- as.data.frame(train_pcr)
test_pcr = -train_pcr
y.test = College.test$Apps
y.train = College.train$Apps
pcr.fit <- pcr(Apps ~. , data  = College.train , scale = TRUE, validation = "none")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

# Evaluate performance of the model with "i" components in the pca regression for test and training.
#?predict.mvr
training_error_store <- c()
test_error_store <- c()
for (i in 1:17){
  pcr.pred.train = predict(pcr.fit, College.train, ncomp = i)
  pcr.pred.test = predict(pcr.fit, College.test, ncomp = i)
  train.error <- mean((pcr.pred.train-y.train)^2)
  test.error <- mean((pcr.pred.test-y.test)^2)
  training_error_store <- c(training_error_store, train.error)
  test_error_store <- c(test_error_store, test.error)
}

x11() 
plot(training_error_store,type="l")

x11()
plot(test_error_store,type="l")

test_error_store
which.min(test_error_store)
coef(pcr.pred.test, 16)

#########################################
## Partial Least Squares 
#########################################
pls.fit = plsr(Apps ~., data = College.train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")


training_error_store.pls <- c()
test_error_store.pls <- c()
for (i in 1:17){
  pls.pred.train = predict(pls.fit, College.train, ncomp = i)
  pls.pred.test = predict(pls.fit, College.test, ncomp = i)
  train.error.pls <- mean((pls.pred.train-y.train)^2)
  test.error.pls <- mean((pls.pred.test-y.test)^2)
  training_error_store.pls <- c(training_error_store.pls, train.error.pls)
  test_error_store.pls <- c(test_error_store.pls, test.error.pls)
}

x11() 
plot(c(1:17),training_error_store.pls,type="l")

x11()
plot(c(1:17),test_error_store.pls,type="l")

test_error_store.pls
which.min(test_error_store.pls)



