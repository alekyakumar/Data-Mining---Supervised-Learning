
install.packages("ISLR")
install.packages("e1071")
library(e1071)
install.packages("leaps")
library(leaps)
library(caret)
install.packages("car")
library(car)
library(ggplot2)
library(rpart)
library(randomForest)
library(geneplotter)
library(gbm)

new_new_red_wine<- read.delim("C:\\Users\\Alekya Kumar\\Desktop\\Data Sciences - Sem 1\\Stats\\Project\\winequality-red.csv" , sep = ";",header = TRUE)
#new_red_wine <- data.frame(new_red_wine)
summary(new_red_wine)

High <- ifelse(new_new_red_wine$quality<=6,'No','Yes')
new_data<-subset(new_new_red_wine,select=-quality)
new_data<-cbind(new_data,High)

set.seed(12345)
test_indis <-sample(1:nrow(new_data),.20*nrow(new_data))
test<-new_data[test_indis,]
training<-new_data[-test_indis,]
y_true<-as.numeric(test$High)-1 #0,No 1,yes(high)

######################################
#Grow a Single Tree
#####################################
model.control<-rpart.control(minsplit=5,xval=10,cp=0)
fit<-rpart(High ~ . ,data=training,method="class",control=model.control)

X11()
plot(fit,uniform=T,compress=T)
text(fit,use.n=TRUE,cex=.5)

#prune the tree back
min_cp=which.min(fit$cptable[,4])

X11()
plot(fit$cptable[,4],main='CP for model selection',ylab='cv error')

pruned_fit<-prune(fit,cp=fit$cptable[min_cp,1])
x11()
plot(pruned_fit)
text(pruned_fit,use.n=TRUE,cex=0.5)

#Compute the error for a single tree
my_pred <- predict(pruned_fit,newdata=test,type='class')
y_hat<-as.numeric(my_pred)-1
misclass_tree<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_tree

#################################
#Random Forest
#################################
rf.fit<-randomForest(High~.,data=training,n.tree=10000)
x11()
varImpPlot(rf.fit)
importance(rf.fit)

y_hat<-predict(rf.fit,newdata=test,type='response')
y_hat<-as.numeric(y_hat)-1
misclass_rf<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_rf

####################################
Bagging
####################################
bag.fit<-randomForest(High~.,data=training,n.tree=10000,mtry=11)
x11()
varImpPlot(bag.fit)
importance(bag.fit)

y_hat<-predict(bag.fit,newdata=test,type='response')
y_hat<-as.numeric(y_hat)-1
misclass_bag<-sum(abs(y_true-y_hat))/length(y_hat)
misclass_bag

########################################
Boosting
########################################
boost.train<-training
boost.train$High<-as.numeric(training$High)-1
boost.test<-test
boost.test$High<-as.numeric(test$High)-1

boost.fit<-gbm(High~.,data=boost.train,n.trees=1000,shrinkage=.1,
               interaction.depth=3,distribution='adaboost')
boost.fit2<-gbm(High~.,data=boost.train,n.trees=1000,shrinkage=.6,
                interaction.depth=3,distribution='adaboost')

summary(boost.fit)

#For shrinkage=.1
y_hat<-predict(boost.fit,newdata=boost.test,n.trees=1000,type='response')
misclass_boost.1<-sum(abs(y_hat-y_true))/length(y_true)
misclass_boost.1

#For shrinkage=.6

y_hat<-predict(boost.fit2,newdata=boost.test,n.trees=1000,type='response')
misclass_boost.6<-sum(abs(y_hat-y_true))/length(y_true)
misclass_boost.6

#For different shrinkages

shrink<-c(.1,.4,.6,.8)
max_iter<-1000
store_error<-c()
for (i in 1:length(shrink)){
  boost.fit<-gbm(High~.,data=boost.train,n.trees=max_iter,shrinkage=shrink[i],
                 interaction.depth=3,distribution='adaboost')
  temp<-c()
  for(j in 1:max_iter){
    y_hat<-predict(boost.fit,newdata=boost.test,n.trees=j,type='response')
    misclass_boost<-sum(abs(y_true-y_hat))/length(y_hat)
    temp<-c(temp,misclass_boost)
  }
  store_error<-cbind(store_error,temp) # max_iter*length(shrink)
}

colnames(store_error)<-paste('shrinkage',shrink,sep=':')
X11()
plot(store_error[,1],type='l',main='Error_Profiles',ylab='error',xlab='boosting')
lines(store_error[,2],col='red')
lines(store_error[,3],col='blue')
lines(store_error[,4],col='green')

High <- ifelse(new_new_red_wine$quality<=6,0,1)
new_data<-subset(new_new_red_wine,select=-quality)
new_data<-cbind(new_data,High)

set.seed(12345)
test_indis <-sample(1:nrow(new_data),.20*nrow(new_data))
test<-new_data[test_indis,]
training<-new_data[-test_indis,]
y_true_train <- training$High
y_true_test <- test$High

#########################################
# Logistic Regression
#########################################

glm.fit <- glm(High ~., data = training)
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = training, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_hat_test <- round(glm.probs.test)

#########################################
#  Calculate the error rates
########################################
train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)

train_err
test_err

####################################
## KNN(K Nearest Neighbours)
####################################
knn.test1 <- knn(training,test,training$High,k = 1)
knn.test5 <- knn(training,test,training$High,k = 5)
knn.test10 <- knn(training,test,training$High,k = 10)
y_hat_test_knn1 <- c(0, 1)[sapply(knn.test1, as.numeric)]
y_hat_test_knn5 <- c(0, 1)[sapply(knn.test5, as.numeric)]
y_hat_test_knn10 <- c(0, 1)[sapply(knn.test10, as.numeric)]

#########################################
#  Calculate the error rates
########################################
knn_test_err1 <- sum(abs(y_hat_test_knn1- y_true_test))/length(y_true_test)
knn_test_err5 <- sum(abs(y_hat_test_knn5- y_true_test))/length(y_true_test)
knn_test_err10 <- sum(abs(y_hat_test_knn10- y_true_test))/length(y_true_test)
knn_test_err1
knn_test_err5
knn_test_err10
