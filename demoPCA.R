lapply(iris[,1:4], mean)
apply(iris[,1:4],2, mean)
apply(iris[,1:4],2, var)

pcI <- prcomp(iris[,1:4], scale = TRUE)
pcI$center
pcI$scale
pcI$rotation
dim(pcI$x)
biplot(pcI,scale=0)
pcI$sdev
pcV <- pcI$sdev^2
pve <- pcV/sum(pcV)
pve
plot(pve, xlab = "Principal Component", ylab = "Proportion of 
     variance explained", ylim=c(0,1), type="b")

plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of 
     variance explained", ylim=c(0,1), type="b")

install.packages("pls")
library(pls)
set.seed(1)

pcModel <- pcr(mpg~., data=mtcars, scale = TRUE, validation = "CV")
summary(pcModel)
validationplot(pcModel ,val.type="MSEP")
predV <- predict(pcModel ,mtcars,ncomp =2) 
mean((predV-mtcars$mpg)^2)

