set.seed(1)
x = rnorm(100)
y=x-2*x^2+rnorm(100)

install.packages("lattice")
install.packages("glmnet")
library(glmnet)
library(lattice)
library(boot)

new_data <- data.frame(x,y)

fit.glm.1 <- glm(y ~ x)
cv.glm(new_data, fit.glm.1)$delta[1]

fit.glm.2 <- glm(y ~ poly(x,2))
cv.glm(new_data,fit.glm.2)$delta[1]

fit.glm.3 <- glm(y ~ poly(x,3))
cv.glm(new_data, fit.glm.3)$delta[1]

fit.glm.4 <- glm(y ~ poly(x,4))
cv.glm(new_data, fit.glm.4)$delta[1]

summary(fit.glm.1)
summary(fit.glm.2)
summary(fit.glm.3)
summary(fit.glm.4)

