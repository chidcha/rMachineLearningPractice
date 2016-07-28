set.seed(4)
index <- sample(c(TRUE, FALSE), nrow(iris), p = c(0.30, 0.70), 
                replace = TRUE)
myIris <- iris[index,3:4]

seedV <- 50

set.seed(seedV)
pLength <- runif(3, min(myIris$Petal.Length), max(myIris$Petal.Length))
set.seed(seedV)
pWidth <- runif(3, min(myIris$Petal.Width), max(myIris$Petal.Width))

fname <- sprintf("figures/kmeans/seed%02d/00.jpg", seedV)

jpeg(fname)
plot(myIris$Petal.Length, myIris$Petal.Width, pch = 19,
     xlab= "Petal Length", ylab = "Petal Width", xlim = c(0,7), ylim = c(0,7))
points(pLength, pWidth, pch = 8, col = 1:length(pLength), cex = 2.0)
dev.off()

for (i in 1:3){
  colNm <- paste("dist", i, sep = "")
  myIris[[colNm]] <- NA
}


myIris$group <- NA

for (i in 1:10){
  
  for (j in 1:3){
    myIris[[2+j]] <- sqrt((myIris$Petal.Length - pLength[j])^2 + 
                            (myIris$Petal.Width - pWidth[j])^2)
  }
  
  for (j in 1:nrow(myIris)){
    myIris$group[j] <- which.min(myIris[j,3:5])
  }
  
  fname <- sprintf("figures/kmeans/seed%02d/%02da.jpg", seedV, i)
  jpeg(fname)
  plot(myIris$Petal.Length, myIris$Petal.Width, pch = 19,
       xlab= "Petal.Length", ylab = "Petal.Width", col = myIris$group,
       xlim = c(0,7), ylim = c(0,7))
  points(pLength, pWidth, pch = 8, col = 1:length(pLength), cex = 2.0)
  dev.off()
  
  for (j in 1:3){
    if(any(myIris$group==j)){
      pLength[j] = mean(myIris$Petal.Length[myIris$group==j])
      pWidth[j] = mean(myIris$Petal.Width[myIris$group==j])
    }
  }
  
  fname <- sprintf("figures/kmeans/seed%02d/%02db.jpg", seedV, i)
  jpeg(fname)
  plot(myIris$Petal.Length, myIris$Petal.Width, pch = 19,
       xlab= "Petal.Length", ylab = "Petal.Width", col = myIris$group,
       xlim = c(0,7), ylim = c(0,7))
  points(pLength, pWidth, pch = 8, col = 1:length(pLength), cex = 2.0)
  dev.off()
}



      