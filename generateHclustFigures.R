findRows <- function(M){
  
  diag(disM) <- max(disM) + 1
  mv <- as.vector(disM)
  nr <- nrow(disM)
  nc <- ncol(disM)
  l <- which.min(mv)
  qt <- l%/%nc
  rm <- l%%nc
  
  if(rm == 0){
    row1 <-  nc
    row2 <- qt
  } else {
    row1 <- rm
    row2 <- qt + 1
  }
  
  c(row1, row2)
}


replaceWithMean <- function(df, n){
  p1 <- df[n[1],]
  p2 <- df[n[2],]
  m1 <- mean(c(p1[1,1], p2[1,1]))
  m2 <- mean(c(p1[1,2], p2[1,2]))
  
  df <- df[-n,]
  df <- rbind(df, c(m1,m2))
}




set.seed(4)
index <- sample(c(TRUE, FALSE), nrow(iris), p = c(0.05, 0.95), 
                replace = TRUE)
myIris <- iris[index,3:4]
species <- iris[index,5]
disM <- as.matrix(dist(myIris))
myIris1 <- myIris

xran <- range(myIris$Petal.Length)
yran <- range(myIris$Petal.Width)

jpeg("figures/hclust/00.jpg")
plot(myIris$Petal.Length, myIris$Petal.Width, pch = 19, 
     cex=1.5, col = "blue", xlim = xran, ylim = yran,
     xlab = "Petal Length", ylab = "Petal Width")
text(myIris1$Petal.Length, myIris1$Petal.Width, 
     labels = which(index==TRUE), 
     cex = 1.0, col="red")
dev.off()

nr <- nrow(myIris)
for (i in 1:(nr)){
  disM <- as.matrix(dist(myIris))
  rn <- findRows(disM)
  fname <- sprintf("figures/hclust/%02d.jpg",i)
  myIris <- replaceWithMean(myIris,rn)
  jpeg(fname)
  plot(myIris[,1], myIris[,2], pch = 19, 
       cex=1.5, col = "blue", xlim = xran, ylim = yran,
       xlab = "Petal Length", ylab = "Petal Width")
  text(myIris1$Petal.Length, myIris1$Petal.Width, 
       labels = which(index==TRUE), 
       cex = 1.0, col="red")
  dev.off()
}

disM <- dist(myIris1)
irisClust <- hclust(disM)

clusters <- cutree(irisClust, k = 3)
clusters <- ifelse(clusters==1, "setosa", ifelse(clusters==2, 
                                                 "versicolor", "virginica"))
clusters <- factor(clusters)
col <- ifelse(clusters == "setosa", "green", 
              ifelse(clusters=="versicolor", "red", "blue"))

library(sparcl)
jpeg("figures/hclust/dendogram.jpg")
ColorDendrogram(irisClust, y = col, labels = names(clusters), 
                main = "Dendogram",   branchlength = 5)
dev.off()

jpeg("figures/hclust/identifiedGroup.jpg")
plot(myIris1$Petal.Length, myIris1$Petal.Width, pch = 19, col = col,
     xlab = "Petal Length", ylab = "Petal Width")
legend(x=1,y=2.0, legend = c("group1", "group2", "group3"), 
       col = c("green", "red", "blue"), pch = 19, y.intersp=0.75, 
       cex = 0.75, bty = "n")
dev.off()


jpeg("figures/hclust/actualGroup.jpg")
col <- ifelse(species == "setosa", "green", 
              ifelse(species=="versicolor", "red", "blue"))
plot(myIris1$Petal.Length, myIris1$Petal.Width, pch = 17, col = col,
     xlab = "Petal Length", ylab = "Petal Width")
legend(x=1,y=2.0, legend = c("setosa", "versicolor", "virginica"), 
       col = c("green", "red", "blue"), pch = 17, y.intersp=0.75, 
       cex = 0.75, bty = "n")
dev.off()
