library(shiny)
library(class)

server <- function(input, output){
  
  output$testData <- renderUI({
    set.seed(100)
    n <- input$n
    inTrain <- sample(1:nrow(iris), n)
    inTest <- which(1:nrow(iris) %in% inTrain == FALSE)
    selectInput("variable1", "Choose Observation Number:", as.list(rev(inTest))) 
  })
  
  output$plot1 <- renderPlot({
    set.seed(100)
    n <- input$n
    nn <- input$nn
    inTrain <- sample(1:nrow(iris), n)
    plot(iris[inTrain,3:4], pch = 1, col = iris[inTrain,5], 
         xlab = "Petal Length", ylab = "Petal Width",
         cex = 1.5, xlim = c(0,7), ylim = c(0,4))
    legend(x=0, y =4.0, legend = c("Setosa", "Versicolor", "Virginica"),
           pch = 1, col = c("black", "red", "green"), bty = "n",
           cex=1.5)
    testCl <- knn(iris[inTrain,3:4], iris[input$variable1, 3:4], 
                  iris[inTrain,5], k = nn)
    
    if(input$spoint) {
      points(iris[input$variable1, 3:4], pch = 8, cex = 1.5)
    }
    
    if(input$nnpoints) {
      myIris <- rbind(iris[input$variable1, 3:4], iris[inTrain,3:4])
      distM <- as.matrix(dist(myIris))
      inNn <- as.numeric(names(sort(distM[,1]))[2:(nn+1)])
      
      points(iris[inNn,3:4], pch = 3, col = iris[inNn,5])
    }
    
    if(input$prediction) {
      points(iris[input$variable1, 3:4], pch = 8, cex = 1.5, col = testCl)
    }
    
    
    
    
  })
  
  

  
  
}