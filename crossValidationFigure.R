
jpeg("figures/crossValidation.jpg")

drawPoly <- function(x,y, col){
  polygon(c(x,x+1,x+1,x, x), c(y,y,y+0.5,y+0.5,y), col = col)
}


plot(1, type="n", axes=F, xlab="", ylab="", xlim = c(0,6), ylim= c(0.5,10),
     main = "5-fold cross-validation illustration")

polygon(c(1,6,6,1,1), c(9,9,9.5,9.5,9), col = "grey")

for (i in 1:5){
  for (j in 1:5) {
    col <- ifelse(i==j, "red", "green")
    drawPoly(i,j, col)
  }
}

drawPoly(1,6, "red")
drawPoly(1,7, "green")

for(i in 1:5){
  text(i+0.5, 0.5, label = paste("k = ", i, sep = ""))
}

for(i in 1:5){
  text(0.5, i+0.25, label = i)
}

text(3, 7.3, label = "Training Data")
text(3, 6.3, label = "Test Data")
text(0.4, 9.7, label = "original")
text(0.4, 8.9, label = "data")
dev.off()
