## Cross Validation, Bootstrapping

require(ISLR)
head(Auto)
dim(Auto) 

#### CROSS VALIDATION

set.seed(1)
?sample
train <- sample(392,196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
summary(lm.fit)
lm.pred <- predict(lm.fit,Auto)
mse <- mean((Auto$mpg-lm.pred)[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto,subset=train)
summary(lm.fit2)
lm.predict2 <- predict(lm.fit2,Auto)
mse <- mean((Auto$mpg-lm.predict2)[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto,subset=train)
lm.predict3 <- predict(lm.fit3,Auto)
mse <- mean((Auto$mpg-lm.predict3)[-train]^2)
##will be slightly different with a different trainig set...
set.seed(2)
train <- sample(392,196)
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto,subset=train)
lm.predict3 <- predict(lm.fit3,Auto)
mean((Auto$mpg-lm.predict3)[-train]^2)

###LOOCV
require(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta 

cv.error <-rep(0,5)
for (i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#### K-fold cross validation
set.seed(17)
cv.error <- rep(0,10)
for (i in 1:10){
  glm.fit <- glm(mpg~horsepower,data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}


#### BOOTSTRAP ####
require(dplyr)
set.seed(1)

#bootstrapping for a statistic
alpha.fn <- function(data,index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(Y)+var(X)-cov(X,Y) ))
}
head(Portfolio)
alpha.fn(Portfolio,1:100)
boot(Portfolio,alpha.fn,R=1000)

#for the coefficients of a linear model, you can do the same
boot.fn <- function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
## this calculates the coefficients for one sample from the data
boot.fn(Auto, sample(392,392,replace=T))
##feed it into boot...
boot(Auto, boot.fn, R=1000)

##or for a quadratic:

boot.fn <- function(data,index){
  temp <- coef(lm(data=Auto,mpg~horsepower + I(horsepower^2), subset=index))
  return(temp)  
}
boot.fn(Auto,1:10)
#feed into boot
boot(Auto,boot.fn,R=1000)

## note, the poly functions looks for orthogonal polynomials. 
## orthogonal functions are functions whose inner product
## integrate to zero in a particular interval

x <- seq(-10,10,.1)
y1 <- 1 + x
y2 <- x - x^2
y3 <- (1+x)*(x-(x^2))
require(ggplot2)
d <- data.frame(x,y1,y2,y3)
d

ggplot(d,aes(x=x)) + geom_line(aes(y=y3))

ggplot(d,aes(x=x)) + geom_line(aes(y= y1),col="red") + 
  geom_line(aes(y=y2),col="blue") +
  geom_line(aes(y=y3), col="purple") +
  scale_x_continuous(limits = c(-2,2)) +
  scale_y_continuous(limits = c(-10,10)) +
  #geom_area(aes(y = y1), fill = 'blue',alpha=.4) + 
  #geom_area(aes(y = y2), fill = 'red', alpha=.4) +
  geom_area(aes(y=y3), fill="purple", alpha=.3) +
  geom_vline(xintercept=-2) + 
  geom_vline(xintercept=2)



######### exercises applied ###########
set.seed(1)
#Default data set
head(Default)
### predict default
glm.fit <- glm(data=Default, default~balance + income, family=binomial)
summary(glm.fit)
probs <- predict(glm.fit,Default, type="response")
hist(probs)
N <- dim(Default)[1]
# validation set approach
train <- sample(N,N/2)
glm.fit <- glm(data=Default, default~balance+income, family=binomial, subset=train)
glm.prob <- predict(glm.fit, Default[-train,], type="response")
glm.pred <- rep('No',N/2)
glm.pred[glm.prob>.5] <- 'Yes' 
mean(glm.pred!=Default[-train,]$default)
table(glm.pred,Default[-train,]$default)

resam <- function(){
  train <- sample(N,N/2)
  glm.fit <- glm(data=Default, default~balance + income + student, family=binomial, subset=train)
  glm.prob <- predict(glm.fit, Default[-train,], type="response")
  glm.pred <- rep('No',N/2)
  glm.pred[glm.prob>.5] <- 'Yes' 
  return(mean(glm.pred!=Default[-train,]$default))
  table(glm.pred,Default[-train,]$default)
}
resam()

####### bootstrap coefficients ##########
require(boot)
boot.fn <- function(data,index){
  return(coef(glm(data=data,default~income + balance, family=binomial, subset=index)))
}
boot(Default,boot.fn,R=1000)
summary(glm(data=Default,default~income + balance, family=binomial))

#### program LOOCV yourself
err <- rep(0,dim(Default)[1])
for(i in 1:dim(Default)[1]){
  glm.fit <- glm(data=Default,default~income + balance, family=binomial, subset=-i)
  glm.p <- predict(glm.fit,Default[i,],type="response")
  if(glm.p>.5) glm.pred='Yes'
  else glm.pred='No'
  if(glm.pred!=Default$default[i]) err[i] <- 1
  
}
#or use function
glm.fit <- glm(data=Default,default~income + balance, family=binomial)
cv.err <- cv.glm(Default,glm.fit)



###### on generated data ######
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
d <- data.frame(x,y)
plot(data=d, y~x)

cv.err <- rep(0,4)
for (i in 1:4){
  glm.fit <- glm(data=d,y~poly(x,i))
  cv.err[i] <- cv.glm(d,glm.fit)$delta[1]
}

plot(1:4,cv.err,type='l')

glm.fit <- glm(data=d,y~poly(x,3))
plot(data=d,y~x)
points(d$x,fitted(glm.fit),col='red')
#seems like we need a quadratic component in this set! :)


################
require(MASS)
?Boston
head(Boston)
mu <- mean(Boston$medv)
se <- sd(Boston$medv)/sqrt(dim(Boston)[1])
boot.fn <- function(data,index){
  return(mean(data$medv[index]))
}

t.test(Boston$medv)
boot(Boston,boot.fn,R=1000)
c((mu - .412*qnorm(.975,0,1)), (mu + .412*qnorm(.975,0,1)))

#estimate for the median
boot.fn <- function(data,index){
  return(median(data$medv[index]))
}
boot(Boston,boot.fn,R=1000)


