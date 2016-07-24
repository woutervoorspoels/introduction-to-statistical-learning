#chapter 4 classification

## logistic regression
require(ggplot2)
library(ISLR)
require(GGally)
require(dplyr)
?Smarket
names(Smarket) 
head(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
#ggpairs(Smarket, mapping = aes(), colour = "black")
cor(Smarket)
cor(Smarket[,-9])

#fit a logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , 
            data=Smarket ,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

#used without new data, predict returns the model values for the DV
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
#if you want to be silly, you can do this
#(model values are truly uncertain, so this is basic overfitting):
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred, Smarket$Direction)

#validate silly predictions
Smarket.train <- filter(Smarket, Year<2005)
Smarket.test <- filter(Smarket, Year>2004)

glm.fit.train <- glm(data=Smarket.train, 
                     Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
#this is important: the predict fu
## note that if type="response" is not added, logodds are returned
predictions <- predict(glm.fit.train,Smarket.test,type="response")
prediction.qualitative <- rep("Down",dim(Smarket.test)[1])
prediction.qualitative[predictions>.5] <-'up' 
table(prediction.qualitative,Smarket.test$Direction)
mean(prediction.qualitative==as.character(Smarket.test$Direction))

# interestingly, we can simplfy the model, to get better predictions

#visualise
boxplot(data=Smarket.train, Lag1~Direction)
boxplot(data=Smarket.train, Lag2~Direction)
boxplot(data=Smarket.train, Lag3~Direction)
boxplot(data=Smarket.train, Volume~Direction)


glm.fit.train <- glm(data=Smarket.train, 
                     Direction~Lag1 + Lag2, family=binomial)
#this is important: the predict fu
predictions <- predict(glm.fit.train,Smarket.test,type="response")
prediction.qualitative <- rep("Down",dim(Smarket.test)[1])
prediction.qualitative[predictions>.5] <-'up' 
table(prediction.qualitative,Smarket.test$Direction)
mean(prediction.qualitative==as.character(Smarket.test$Direction))


###### LINEAR DISCRIMINANT ANALYSIS ######

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket.train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit, Smarket.test)
names(lda.pred)
lda.pred$posterior

lda.class=lda.pred$class
table(lda.class ,Smarket.test$Direction)

### nearest neighbours ####
require(class)
train.X=cbind(Smarket.train$Lag1 ,Smarket.train$Lag2)
test.X=cbind(Smarket.test$Lag1,Smarket.test$Lag2)
train.Direction= Smarket.train$Direction

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred,Smarket.test$Direction)
(48+87) /252

### Application to the caravan set

dim(Caravan)
str(Caravan)
table(Caravan$Purchase)
# nearest neighbours requires variables of the same scale (distance)
standardized.X <- scale(Caravan[,-86], center=TRUE,scale=TRUE)
# nearest neighbours
test <- seq(1,1000)
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]
set.seed(1)
knn.pred <- knn(train.X,test.X,train.Y,k=3)

#### these evaluations are interesting, and useful!
mean(test.Y!="No")
table(knn.pred,test.Y)
# how many of the predicted buyers effectively buy?
table(knn.pred,test.Y)[2,2]/sum(table(knn.pred,test.Y)[2,] )
# how sensitive?
table(knn.pred,test.Y)[2,2]/sum(table(knn.pred,test.Y)[,2] )
# how specific?
table(knn.pred,test.Y)[1,1]/sum(table(knn.pred,test.Y)[,1] )


### how does logistic regression fare?

glm.fit <- glm(Purchase~.,family=binomial,data=Caravan,subset=-test)
glm.probs <- predict(glm.fit,Caravan[test,], type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]<-"Yes"
table(glm.pred,test.Y)

glm.pred[glm.probs>.25]<-"Yes"
table(glm.pred,test.Y)
#how many predicted buyers buy?
table(glm.pred,test.Y)[2,2]/sum(table(glm.pred,test.Y)[2,])
#sensitivity?
table(glm.pred,test.Y)[2,2]/sum(table(glm.pred,test.Y)[,2])
#specificity?
table(glm.pred,test.Y)[1,1]/sum(table(glm.pred,test.Y)[,1])

############# Exercises #############
dim(Weekly)
pairs(Weekly)
Weekly$Year

fit.glm <- glm(Direction~.-Today, family=binomial,data=Weekly)
summary(fit.glm)
probs.glm <- predict(fit.glm,type="response")
pred.glm <- rep("Down", dim(Weekly)[1])
pred.glm[probs.glm>.5] <- 'Up'
table(pred.glm,Weekly$Direction)
table(pred.glm,Weekly$Direction)[2,2]/sum(table(pred.glm,Weekly$Direction)[2,])
# prediction of overall upgoing. so mostly right, but
mean((Weekly$Direction=='Up'))
#not much of a prediction over and above average...

