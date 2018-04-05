train_data <- read.delim("C:/Users/Alekya Kumar/Desktop/R work/ticdata2000.txt")
head(train_data)
colnames(train_data) <- c(paste("X",1:86, sep = ""))

old_test_data <- read.delim("C:/Users/Alekya Kumar/Desktop/R work/ticeval2000.txt")
target_data <- read.delim("C:/Users/Alekya Kumar/Desktop/R work/tictgts2000.txt")
#test_data <- data.frame(Y = data[,86])
test_data <- cbind(old_test_data,target_data)
View(test_data)
colnames(test_data) <- c(paste("X",1:86, sep = ""))

#train_data <- data.frame(train_data, Y=train_data[,86])
#View(train_data)
#rm(train_data)
##########################Linear regression Model#################
lm_caravan <- lm(X86 ~ ., data = train_data)
#prediction <- predict.lm(lm_caravan,train_data)
#error_data <- mean((prediction-filtered_train[,1])^2)

prediction_test <- predict.lm(lm_caravan,test_data)
error_data <- mean((prediction_test-target_data[,1])^2)

error_data
#################Possible subsets#####################
install.packages("leaps")
library(MMST)
library(leaps)

#Forward Selection
regfit.fwd <- regsubsets(X86 ~., data = train_data, nvmax = 85, method = "forward")
fwd.sum <- summary(regfit.fwd)
fwd.sum
plot(fwd.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(fwd.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(fwd.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which(fwd.sum$cp == min(fwd.sum$cp))
which(fwd.sum$rss == min(fwd.sum$rss))

coef(regfit.fwd, 8)

#Linear model for Forward
linear_model_fwd <- lm(X86 ~ X10 + X18 + X43 + X44 + X47 + X59 + X82 + X85, data = train_data)

prediction_test <- predict.lm(linear_model_fwd,test_data)
summary(linear_model_fwd)
error_data_fwd <- mean((prediction_test-target_data[,1])^2)

#Backward Selection
regfit.bwd <- regsubsets(X86 ~., data = train_data, nvmax = 85, method = "backward")
bwd.sum <- summary(regfit.bwd)
bwd.sum
plot(bwd.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(bwd.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(bwd.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which(bwd.sum$cp == min(bwd.sum$cp))
which(bwd.sum$bic == min(bwd.sum$bic))
which(bwd.sum$rss == min(bwd.sum$rss))

coef(regfit.fwd, 8)

#Linear model for Backward
linear_model_bwd <- lm(X86 ~ X10 + X18 + X21 + X46 + X47 + X59 + X82 + X85, data = train_data)

prediction_test <- predict.lm(linear_model_bwd,test_data)
summary(linear_model_bwd)
error_data_bwd <- mean((prediction_test-target_data[,1])^2)
error_data_bwd


summary(regfit.fwd)$outmat[7,]
summary(regfit.bwd)$outmat[7,]


###################Ridge and Lasso###################
 
X <- as.matrix(train_data[,1:85])
Y <- train_data[,86]


ridge.mod  = glmnet(X,Y,alpha = 0)
coef(ridge.mod)
dim(coef(ridge.mod))

predict(ridge.mod, s = .0005, type = "coefficient")
coef(ridge.mod)
dim(coef(ridge.mod))

cv.out <- cv.glmnet(X,Y, alpha=0)
plot(cv.out)
names(cv.out)

best_lambda_rid <- cv.out$lambda.min
best_lambda_rid

###############Training and Test Prediction########
ridge.pred.train <- predict(ridge.mod, s= best_lambda_rid, type = "coefficients")
ridge.pred.train

old_test_data <- as.matrix(old_test_data)
target_data <- as.matrix(target_data)

ridge.pred.test.response <- predict(ridge.mod, s= best_lambda_rid, newx = old_test_data , type = "response")
ridge.pred.test.response

y_hat <- ridge.pred.test.response
y_actual <- target_data
residual_sum <- sum((y_hat - y_actual)^2)
sum_square =sum((y_actual-mean(y_actual))^2)
test_error_ridge <- 1-(residual_sum/sum_square)
test_error_ridge


############################LASSO MODEL#################################

lasso.model <- glmnet(X, Y, alpha = 1)

plot(lasso.model)

# selectig the Best Lambda
cv.out = cv.glmnet(X, Y, alpha = 1)
bestlam = cv.out$lambda.min
bestlam
plot(cv.out)
lasso.model <- glmnet(X, Y, alpha = 1, lambda = bestlam)

lasso.pred <- predict(lasso.model, s = bestlam, type = "coefficients")
lasso.pred

lasso.pred2 <- predict(lasso.model, s = bestlam, newx = old_test_data, type = "response")
lasso.pred2

y_hat_lasso <- lasso.pred2
y_true <- target_data

residual_sum <- sum((y_hat - y_true)^2)
sum_square =sum((y_true-mean(y_true))^2)
test_error_lasso <- 1-(residual_sum/sum_square)
test_error_lasso
