set.seed(100)
index <- sample(c(TRUE, FALSE), nrow(iris), p = c(0.2, 0.8), replace = TRUE)
myIris <- iris[index,3:4]
group <- iris$Species[index]
set.seed(100)
predGroup <- kmeans(myIris, centers = 3, nstart = 10)
predGroupC <- ifelse(predGroup$cluster==2, "setosa", 
                     ifelse(predGroup$cluster==1, "versicolor", "virginnica"))
predGroupC <- factor(predGroupC)
table(predGroupC, group)


#############################################################################
# Challenge: Find 3 clusters in mtcars dataset. Do not use mpg variable in 
# clustering. After clustering make a box plot of mpg vs clustering group 
#############################################################################

