install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("gam")
library(gam)
library(car)


data(spam)

set.seed(1907)
spam_data = data.frame(spam)

spam_data$spam = ifelse(spam_data$spam=="spam", 1, 0)


par(mfrow=c(1,3))
x11()


train <- sample(1:nrow(spam_data), .70*nrow(spam_data))
spam_train <- spam_data[train,]
spam_test <- spam_data[-train,]

spam_additive<-gam(spam_data$spam ~ ., data=spam_data,family=binomial)
x11()
plot(spam_additive,se=T,color = "green")

pr=predict(spam_additive,newdata=spam_test)
conf_gam= table(pr>.5,spam_test$spam) 
error_rate <- 1-sum(diag(conf_gam))/sum(conf_gam)
error_rate
