library(e1071)

server <- function(input, output){
  
  inSetosa <- iris$Species == "setosa"
  myIris <- iris[!inSetosa, c("Petal.Length", "Petal.Width", "Species")]
  myIris$Species <- factor(myIris$Species, levels = c("versicolor", "virginica"))
  
  
  output$plot <- renderPlot({   
    plot(scale(myIris$Petal.Length), scale(myIris$Petal.Width), pch = 19, col = myIris$Species, 
         xlab = "Petal Length", ylab =  "Petal Width", cex.lab = 1.5,
         cex.axis = 1.5)
    legend(x=3, y = 2.4, legend = c("Versicolor", "Virginica"), 
           col = c("black", "red"), bty = "n", pch = 19, cex = 1.5)
    
    if(input$boundary) {
      if(input$kernal == "linear") {
        svmModel <- svm(Species ~ ., data = myIris, kernal = "linear", 
                        scale = TRUE, cost = input$cost)
        w <- t(svmModel$coefs) %*% svmModel$SV
        b <- -svmModel$rho
        
        x1val <- seq(-4.5, 4.2,length.out = 100)
        x2val <- seq(-1.8,2.2, length.out = 100)
        
        val <- data.frame(expand.grid(x1val, x2val))
        val$y <- w[1,1]*val$Var1 + w[1,2]*val$Var2 + b 
        
        names(val) <- c("Petal.Length", "Petal.Width", "y")
        
        pcol <- ifelse(val$y > 0, "black", "red")
        
        points(val$Petal.Length, val$Petal.Width, col = pcol, cex = 0.02)
      }
      
      if(input$kernal == "radial") {
        
        svmModel <- svm(Species ~ ., data = myIris, kernal = "radial", cost = input$cost,
                        gamma = input$gamma, scale = TRUE)
        
        x1mean <- mean(myIris$Petal.Length)
        x1sd <- sd(myIris$Petal.Length)
        
        x2mean <- mean(myIris$Petal.Width)
        x2sd <- sd(myIris$Petal.Width)
        
        x1val <- seq(-1,8,length.out = 200)
        x2val <- seq(-1,8, length.out = 200)
        
        xval <- data.frame(expand.grid(x1val, x2val))
        names(xval) <- c("Petal.Length", "Petal.Width")
        yval <- predict(svmModel, xval)
        
        xval$Petal.Length <- (xval$Petal.Length - x1mean)/x1sd
        xval$Petal.Width  <- (xval$Petal.Width - x2mean)/x2sd
        
        pcol <- ifelse(yval=="versicolor", "black", "red")
        plot(scale(myIris$Petal.Length), scale(myIris$Petal.Width), pch = 19, 
             col = myIris$Species,
             main = "Non-Linear Decision Boundary", 
             xlab = "scale(Petal.Length)", ylab = "scale(Petal.Width)",
             xlim = c(-3,2.8))
        points(xval, col = pcol, cex = 0.02)
        
      }

    }
    
  })
}