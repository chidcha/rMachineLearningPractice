library(shiny)

server <- function(input, output){
  
  inSetosa <- iris$Species == "setosa"
  myIris <- iris[!inSetosa,]
  myIris$Species <- factor(myIris$Species, levels = c("versicolor", "virginica"))
  
  output$table1 <- renderTable({(iris[c(60,110,150),c(3,5)])})
  
  glmModel <- glm(Species ~ Petal.Length, data = myIris, family = binomial())
  predValue <- predict(glmModel, myIris, type = "response")
  
  output$plot1 <- renderPlot({
    vc <- myIris$Petal.Length[myIris$Species=="versicolor"]
    vg <- myIris$Petal.Length[myIris$Species=="virginica"]
    
    hist(vc, prob = TRUE, col=rgb(0,0,1,0), border = rgb(0,0,1,0), 
         xlim = c(2.5,7.5), xlab = "Petal Length", main = " ")
    
    if(input$versicolor){
      hist(vc, prob = TRUE, add = TRUE, col=rgb(0,0,1,0.2), breaks = 7)
      lines(density(vc), col = "blue", lwd = 2)
      legend(x=5.5, y= 0.86, legend = "Versicolor", 
             col = "blue", lty = 1, bty = "n")
    }
    
    if(input$virginica){
      hist(vg, prob = TRUE, add = TRUE, col = rgb(0,1,0,0.2), breaks = 7)
      lines(density(vg), col = "green", lwd = 2)
      legend(x=5.5, y= 0.79, legend = "Virginica", 
             col = "green", lty = 1, bty = "n")
    }
    
  })
  
  output$plot2 <- renderPlot({   
    pch <- ifelse(myIris$Species == "versicolor", 3, 4)
    col <- ifelse(myIris$Species == "versicolor", "blue", "green")
    set.seed(100)
    plot(jitter(myIris$Petal.Length, amount = 0.05),
            predValue, col = col, pch = pch, xlab = "Petal Length",
         ylab = "p(Virginica)", xlim = c(2.9,7.2), ylim = c(-0.1, 1.1),
         cex.lab = 1.5, cex.axis = 1.5)
    legend(x=3, y = 1.1, legend = c("Virginica", "Versicolor"),
           pch = c(4,3), col = c("green", "blue"), bty = "n", cex = 1.5)
    
    if(input$boundary) {
      abline(h=input$p, lty = 2)
    }
    
  })
  
  output$table2 <- renderTable({
    predC <- ifelse(predValue > input$p, "virginica", "versicolor")
    table(predC, myIris$Species)
    })
  
}