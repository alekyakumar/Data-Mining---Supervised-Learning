###setwd("C:\Users\Alekya Kumar\Desktop\R work")

I_Auto <- subset(Auto)
write.table(I_Auto, file = "Auto_dataset", sep = "\t", row.names = FALSE, col.names = names(I_Auto))
temp <- read.dilem("Auto_dataset" , sep = "\t" , header = TRUE)
hist(mpg, xlab = "cylinders", main = "MPG plot with cylinders")

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
pairs(~ mpg + weight, data = I_Auto)
pairs(~ mpg + displacement, data = I_Auto)
pairs(~ mpg + acceleration, data = I_Auto)
pairs(~ mpg + horsepower, data = I_Auto)
pairs(~ mpg + year, data = I_Auto)
pairs(~ mpg + origin, data = I_Auto)
pairs(~ mpg + name, data = I_Auto)

Auto_MR <- lm(mpg ~ cylinders + weight + displacement + acceleration + horsepower + year + origin , data=Auto)
Auto_MR
#plot(Auto_MR)
cor(Auto_MR)

#################Interaction among variables###################
new_auto <- lm(mpg ~ weight:displacement + weight:year + weight:origin, data = Auto)
new_auto <- lm(mpg ~ weight+displacement + weight*displacement, data = Auto)
new_auto <- lm(mpg ~ weight + displacement + year + origin + weight*displacement + displacement*year,  data = Auto) # very significant
new_auto <- lm(mpg ~ year*origin + weight + year + origin + displacement:horsepower, data = Auto)