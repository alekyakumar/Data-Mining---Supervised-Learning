Boston_data <- subset(Boston)
write.table(Boston, file = "Boston_data", sep = "\t", row.names = FALSE, col.names = names(Boston))
summary(Boston)
##division A & B##
pairs(Boston)

pairs(~ medv + lstat, data = Boston)
xyplot(medv ~ sqrt(crim), data=Boston, groups = chas, auto.key = list(columns=2))
xyplot(medv ~ sqrt(indus), data=Boston, groups = chas, auto.key = list(columns=2))


pairs(~ mpg + weight, data = I_Auto)
pairs(~ mpg + displacement, data = I_Auto)
pairs(~ mpg + horsepower, data = I_Auto)
pairs(~ mpg + acceleration, data = I_Auto)
boxplot(I_Auto$mpg~I_Auto$acceleration)
xyplot(mpg ~ year, data=I_Auto, groups = year, auto.key = list(columns=9))
xyplot(mpg ~ origin, data=I_Auto, groups = origin, auto.key = list(columns=3))


ggplot(data = I_Auto,aes(x=weight, y = mpg)) + geom_point()
xyplot(medv ~ lstat, data = Boston)



#######################################################################################################
####Subdivision B####


cor_boston <- select(Boston,crim,zn,indus,chas,nox,rm,age,dis,rad,tax,ptratio,black,lstat,medv)
cor(cor_boston)
corrplot(cor_boston, Method="number")
plot_cor <- cor(cor_Boston)

####Subdivision C####
xyplot(crim ~ chas, data = Boston, group = chas , type = c("p","smooth"), auto.key=list(columns=2))
xyplot(ptratio ~ chas, data = Boston, group = chas , type = c("p","smooth"), auto.key=list(columns=2))
xyplot(tax ~ chas, data = Boston, group = chas , type = c("p","smooth"), auto.key=list(columns=2))

subset_one <- subset(Boston, chas==1)
summary(subset_one)
subset_one <- subset(Boston, chas==0)
summary(subset_zero)

############Subdiviion  D ##############
#nrow(Boston[rm>7,])
#nrow(Boston[rm>8,])
