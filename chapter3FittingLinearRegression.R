# chapter 3: fitting regression models

library(ISLR)
library(MASS)
names(Boston)

#first some plots

plot(medv~lstat,Boston)

### linear regression
fit1 <- lm(medv~lstat,data=Boston)
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
?predict
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")



### multiple linear regression
fit2 <- lm(medv~lstat+age,data=Boston)
summary(fit2)
abline(fit2,col="red")
fit3 <- lm(medv~.,data=Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))
fit4 <- update(fit3,~.-age-indus)

### non linear and interactions
fit5 <- lm(medv~lstat*age,data=Boston)
summary(fit5)
fit6 <- lm(medv~lstat + I(lstat^2),data=Boston)
summary(fit6)
plot(medv~lstat,data=Boston)
points(Boston$lstat,fitted(fit6),pch=20,col="blue")
fit7 <- lm(medv~poly(lstat,4),data=Boston)
points(Boston$lstat,fitted(fit7),pch=20,col="red")

### qualitative variables
rm(list=ls())
fix(Carseats)
summary(Carseats)
fit1 <- lm(Sales~.+Income:Advertising+Age:Price,data=Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

###fancy functions
regplot <- function(x,y,...){
  fit <- lm(y~x)
  plot(y~x,...)
  abline(fit,col='red')
}
regplot(Carseats$Price,Carseats$Sales,ylab="sales",xlab="price")
