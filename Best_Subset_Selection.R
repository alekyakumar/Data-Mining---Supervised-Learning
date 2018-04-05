set.seed(12345)
###############Formaing a data set#############
X = matrix(rnorm(20*1000), nrow = 1000, ncol = 20)


install.packages("leaps")
library(leaps)
beta <- rnorm(20)
beta[2] = 0
beta[5] = 0
beta[6] = 0
beta[9] = 0
beta[15]= 0
beta[17] = 0

epsilon <- rnorm(1000)
 
Y = as.vector((X%*%beta) + epsilon)


###########Split into test and train data###########


train <- sample(1:nrow(X), nrow(X)*0.10)

X.train <- as.data.frame(X[train,])
X.test <- as.data.frame(X[-train,])

Y.train <- as.data.frame(Y[train])
Y.test <- as.data.frame(Y[-train])

train_data <- cbind(X.train,Y.train)
colnames(train_data) <- c(paste("D",1:21, sep = ""))
test_data <- cbind(X.test,Y.test)
colnames(test_data) <- c(paste("D",1:21, sep = ""))

total <- cbind(X,Y)

#Best subset Selection 

regfit.full <- regsubsets(D21 ~ ., data = train_data , nvmax = 20 )
#Test Data 
test.mat = model.matrix(D21 ~ ., data = test_data)
val.error = rep (NA,20)
for (i in 1:20){
  coefi = coef(regfit.full, id = i)
  pred = test.mat[,names(coefi)]%*% coefi
  val.error[i] = mean((test_data$D21 - pred)^2)
}
val.error
which.min(val.error)
coef(regfit.full, 12)

plot(val.error, xlab = "Variables" , ylab = "MSE", main = "Test MSE", type = "l")

#Training Data

train.mat = model.matrix(D21 ~ ., data = train_data)
val.error.train = rep (NA,20)
for (i in 1:20){
  coefi = coef(regfit.full, id = i)
  pred.train = train.mat[,names(coefi)]%*% coefi
  val.error.train[i] = mean((train_data$D21 - pred.train)^2)
}
val.error.train
which.min(val.error.train)
coef(regfit.full, which.min(val.error.train))

plot(val.error.train, xlab = "Variables" , ylab = "MSE", main = "Train MSE", type = "l")


###True Model
total <- cbind(X,Y)
total<- as.data.frame(total)
regfit.true <- regsubsets(Y ~ ., data = total , nvmax = 20 )

true.mat = model.matrix(Y ~ ., data = total)
val.error.true = rep (NA,20)
for (i in 1:20){
  coefi = coef(regfit.true, id = i)
  pred = true.mat[,names(coefi)]%*% coefi
  val.error.true[i] = mean((total$Y - pred)^2)
}
val.error.true
which.min(val.error.true)
coef(regfit.true, which.min(val.error.true))

plot(val.error.true, xlab = "Variables" , ylab = "MSE", main = "Total MSE", type = "l")
