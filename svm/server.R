library(e1071)

server <- function(input, output){
  
  inSetosa <- iris$Species == "setosa"
  myIris <- iris[!inSetosa, c("Petal.Length", "Petal.Width", "Species")]
  myIris$Species <- factor(myIris$Species, levels = c("versicolor", "virginica"))
  
  set.seed(100)
  nr <- nrow(myIris)
  inTrain <- sample(1:nr, 0.6*nr)
  
  x1mean <- mean(myIris$Petal.Length[inTrain])
  x1sd <- sd(myIris$Petal.Length[inTrain])
  
  x2mean <- mean(myIris$Petal.Width[inTrain])
  x2sd <- sd(myIris$Petal.Width[inTrain])
  
  output$table <- renderTable({(iris[c(60,110,150),c(3:5)])})
  
  output$plot <- renderPlot({   
    plot(scale(myIris$Petal.Length[inTrain]), scale(myIris$Petal.Width[inTrain]), 
         pch = 19, col = myIris$Species[inTrain], 
         xlab = "Petal Length", ylab =  "Petal Width", cex.lab = 1.5,
         cex.axis = 1.5, xlim = c(-3,2.8))
    legend(x=-3, y = 1.8, legend = c("Versicolor", "Virginica"), 
           col = c("black", "red"), bty = "n", pch = 19, cex = 1.5)
    
    if(input$boundary) {
      if(input$kernal == "linear") {
        svmModel <- svm(Species ~ ., data = myIris, kernal = "linear", 
                        scale = TRUE, cost = input$cost, subset = inTrain)
        w <- t(svmModel$coefs) %*% svmModel$SV
        b <- -svmModel$rho
        
        x1val <- seq(-4.5, 4.2,length.out = 100)
        x2val <- seq(-1.8,2.2, length.out = 100)
        
        val <- data.frame(expand.grid(x1val, x2val))
        val$y <- w[1,1]*val$Var1 + w[1,2]*val$Var2 + b 
        
        names(val) <- c("Petal.Length", "Petal.Width", "y")
        
        pcol <- ifelse(val$y > 0, "black", "red")
        
        points(val$Petal.Length, val$Petal.Width, col = pcol, cex = 0.02)
        
        x1 <- (iris[input$index,3] - x1mean)/x1sd
        x2 <- (iris[input$index,4] - x2mean)/x2sd
        
        if(input$spoint) {
          points(x1, x2, pch = 8, cex = 2, col = "blue")
        }
        
        if(input$prediction) {
          yval <- predict(svmModel, data.frame(Petal.Length=iris[input$index,3],
                                               Petal.Width=iris[input$index,4]))
          
          pcol <- ifelse(yval=="versicolor", "black", "red")
          points(x1, x2, pch = 8, cex = 2, col = pcol)
        }
      }
      
      if(input$kernal == "radial") {
        
        svmModel <- svm(Species ~ ., data = myIris, kernal = "radial", cost = input$cost,
                        gamma = input$gamma, scale = TRUE, subset = inTrain)
        
        x1val <- seq(-1,8,length.out = 200)
        x2val <- seq(-1,8, length.out = 200)
        
        xval <- data.frame(expand.grid(x1val, x2val))
        names(xval) <- c("Petal.Length", "Petal.Width")
        yval <- predict(svmModel, xval)
        
        xval$Petal.Length <- (xval$Petal.Length - x1mean)/x1sd
        xval$Petal.Width  <- (xval$Petal.Width - x2mean)/x2sd
        
        pcol <- ifelse(yval=="versicolor", "black", "red")
        points(xval, col = pcol, cex = 0.02)
        
        x1 <- (iris[input$index,3] - x1mean)/x1sd
        x2 <- (iris[input$index,4] - x2mean)/x2sd
        
        if(input$spoint) {
          points(x1, x2, pch = 8, cex = 2, col = "blue")
        }
        
        if(input$prediction) {
          yval <- predict(svmModel, data.frame(Petal.Length=iris[input$index,3],
                                               Petal.Width=iris[input$index,4]))
          
          pcol <- ifelse(yval=="versicolor", "black", "red")
          points(x1, x2, pch = 8, cex = 2, col = pcol)
        }
        
      }

    }
    
    
    
    
    
  })
}