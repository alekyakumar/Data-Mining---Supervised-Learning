###setwd("C:\Users\Alekya Kumar\Desktop\R work")

I_Auto <- subset(Auto)
write.table(I_Auto, file = "Auto_dataset", sep = "\t", row.names = FALSE, col.names = names(I_Auto))
temp <- read.dilem("Auto_dataset" , sep = "\t" , header = TRUE)
hist(mpg, xlab = "mpg", main = "MPG plot")

#####creating histograms###
par=(mfrow(1,4))
hist(mpg, xlab = "MPG", main = "MPG")
dens <- density(mpg)
xlim <- range(dens$x)
ylim <- range(dens$y)
par=(mfrow(1,4))
#hist(mpg, xlim = xlim, ylim = ylim, probability = T, xlab = "MPG")


####plotting the variables against eachother###
summary(I_Auto)
I_Auto = select(I_Auto,mpg,cylinders,displacement,horsepower,weight,acceleration,year,origin)
pairs(I_Auto)
pairs(~ mpg + cylinders, data = I_Auto)
boxplot(I_Auto$mpg~I_Auto$cylinders)
pairs(~ mpg + weight, data = I_Auto)
pairs(~ mpg + displacement, data = I_Auto)
pairs(~ mpg + horsepower, data = I_Auto)
pairs(~ mpg + acceleration, data = I_Auto)
boxplot(I_Auto$mpg~I_Auto$acceleration)
xyplot(mpg ~ year, data=I_Auto, groups = year, auto.key = list(columns=9))
xyplot(mpg ~ origin, data=I_Auto, groups = origin, auto.key = list(columns=3))


ggplot(data = I_Auto,aes(x=weight, y = mpg)) + geom_point()




 