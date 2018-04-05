setwd("/Users/Alekya Kumar/Desktop/Data Sciences - Sem 1/Stats/R work/Assignment 3")
diabetes <- read.table("C:\\Users\\Alekya Kumar\\Desktop\\Data Sciences - Sem 1\\Stats\\R work\\Assignment 3\\DiabetesAndrews36_1.txt" , header = FALSE)
install.packages("car")
####Using the data set#####
diabetes <- diabetes[,4:10]
library(car)
colnames(diabetes) <-c("obs.no","glucose.area","insulin.area","SSPG","relative.weight","fasting.plasma","class")

x11()
scatterplotMatrix(~glucose.area+insulin.area+SSPG+relative.weight+fasting.plasma, data=diabetes,col = c("red","green","violet","pink"))
pairs(~glucose.area+insulin.area+SSPG+relative.weight+fasting.plasma, data=diabetes,col = c("red","green","violet")[diabetes$class])
legend("topright", c("Class 1","Class 2","Class 3"), lty = c(2,1,3), col = c("green","red","pink"))
cor(diabetes)
cov(diabetes)
######Divide into Train and Test data#############
train <- sample(1:nrow(diabetes), .67*nrow(diabetes))
dia_train <- diabetes[train,]
dia_test <- diabetes[-train,]
y_true_train <- dia_train$class
y_true_test <- dia_test$class


#############Linear Discriminat Analysis########

lda.fit = lda(class~. -obs.no, data = dia_train)
plot(lda.fit)

y_hat_train_lda = predict(lda.fit, newdata = dia_train)
y_hat_train_lda <- as.numeric(y_hat_train_lda$class) ### Since the class is 1 and 2 

y_hat_test_lda = predict(lda.fit, newdata = dia_test)
y_hat_test_lda <- as.numeric(y_hat_test_lda$class) ### Since the class is 1 and 2 

# Compute the error
train_err_lda <- sum(abs(y_hat_train_lda- y_true_train))/length(y_true_train)
test_err_lda <- sum(abs(y_hat_test_lda- y_true_test))/length(y_true_test)

#########Confusion Matrix######
conf_lda <- confusionMatrix(y_hat_test_lda, y_true_test)
conf_lda$table

################Quadratic Discriminant Analysis########

qda.fit <- qda(class ~.-obs.no, data = dia_train)
plot(qda.fit)

y_hat_train_qda <- predict(qda.fit,newdata = dia_train)
y_hat_train_qda <- as.numeric(y_hat_train_qda$class)

y_hat_test_qda <- predict(qda.fit,newdata = dia_test)
y_hat_test_qda <- as.numeric(y_hat_test_qda$class)

###########Compute error#############
train_err_qda <- sum((y_hat_train_qda - y_true_train))/length(y_true_train)

class_rate_test <- mean(y_hat_test_qda == y_true_test)
test_err_qda <- 1 - class_rate_test

#########Confusion Matrix######
conf_qda <- confusionMatrix(y_hat_test_qda, y_true_test)
conf_qda$table

#########Fit to one set of values################

new_val <- data.frame(0,0.98,122,544,186,184,NA)
colnames(new_val) <- c("obs.no","glucose.area","insulin.area","SSPG","relative.weight","fasting.plasma","class")

#LDA Predict

new_lda <- predict(lda.fit,newdata = new_val)
new_lda <- as.numeric(new_lda$class)
new_lda

#QDA Predict

new_qda <- predict(qda.fit,newdata = new_val)
new_qda <- as.numeric(new_qda$class)
new_qda
