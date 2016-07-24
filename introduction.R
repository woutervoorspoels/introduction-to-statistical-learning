### vectors, data matrices, subsetting

x <- c(2,7,5)
x
y <- seq(from=4, length=3,by=3)
?seq
x+y
x/y
x^y
x[-2]
x[-c(2,3)]

z <- matrix(seq(1,12),4,3)
z[,1]
z[,1,drop=FALSE]

ls()
rm(y)


### generating random data and generating plots
x <- runif(50)
y <- rnorm(50)
plot(x,y)
plot(x,y,ylab='random normal',xlab='random uniform',col='blue',pch='.')
par(mfrow=c(2,1))
plot(x,y,ylab='random normal',xlab='random uniform',col='blue',pch='.')
hist(y,xlab='y')
par(mfrow=c(1,1))
