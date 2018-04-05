rm(list = ls())
library(ISLR)
library(MASS)
library(class)
library(caret)
library(car)
require(e1071)
install.packages("rpart")
library(rpart)

winedata = read.csv("C:\\Users\\Alekya Kumar\\Desktop\\Data Sciences - Sem 1\\Stats\\R work\\Assignment 4\\wine.data.txt", header = FALSE, sep = ",")

colnames(winedata) = c("Class","Alcohol","MA","Ash","AlcalinityofAsh","Mg","Tphenols","Flavanoids","NonFlavanoids","Proanthocyanins","ColorIntensity","Hue","OD280/OD315","Proline")
attach(winedata)

######################################################################
#Splitting into train and test dataset
######################################################################
set.seed(1907)
train = sample(1:nrow(winedata), 0.7*nrow(winedata))
winedata.train = winedata[train,]
winedata.test = winedata[-train,]
winedata.train.class = winedata.train$Class
winedata.test.class = winedata.test$Class

model.control = rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.winedata.train = rpart(Class~., data = winedata.train, method = "class", control = model.control)
summary(fit.winedata.train)
x11()
plot(fit.winedata.train, uniform = TRUE, compress = TRUE, margin = 0.1, main = "Classification Tree for Train Dataset")
text(fit.winedata.train, use.n = TRUE)
x11()
plot(fit.winedata.train$cptable[,4], main = "CP for model selection", ylab = "cv error")
fit.winedata.test = rpart(Class~., data = winedata.test, method = "class", control = model.control)
summary(fit.winedata.test)
x11()
plot(fit.winedata.test, uniform = TRUE, compress = TRUE, margin = 0.1, main = "Classification Tree for Test Dataset")
text(fit.winedata.test, use.n = TRUE)
x11()
plot(fit.winedata.test$cptable[,4], main = "CP for model selection", ylab = "cv error")
