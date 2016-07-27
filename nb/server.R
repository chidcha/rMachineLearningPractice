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
  
    x <- seq(2.5, 7.5, length.out = 200)
    dvc <- dnorm(x, mean = mean(vc), sd = sd(vc))
    dvg <- dnorm(x, mean = mean(vg), sd = sd(vg))
    
    hist(vc, prob = TRUE, col=rgb(0,0,1,0), border = rgb(0,0,1,0), 
         xlim = c(2.5,7.5), ylim = c(0,0.9), xlab = "Petal Length", 
         main = " ", cex.axis = 1.5, cex.lab = 1.5)
    
    if(input$versicolor){
      hist(vc, prob = TRUE, add = TRUE, col=rgb(0,0,1,0.2), breaks = 7)
      lines(x,dvc, col = "blue", lwd = 2)
      legend(x=5.8, y= 0.88, legend = "Versicolor", 
             col = "blue", lty = 1, bty = "n", cex = 1.5)
    }
    
    if(input$virginica){
      hist(vg, prob = TRUE, add = TRUE, col = rgb(0,1,0,0.2), breaks = 7)
      lines(x,dvg, col = "green", lwd = 2)
      legend(x=5.8, y= 0.80, legend = "Virginica", 
             col = "green", lty = 1, bty = "n", cex = 1.5)
    }
    
    if(input$boundary) {
      x1 <- dnorm(input$pw, mean = mean(vc), sd = sd(vc))
      x2 <- dnorm(input$pw, mean = mean(vg), sd = sd(vg))
      col <- ifelse(x1> x2, "blue", "green")
      abline(v=input$pw, lwd = 2, col = col)
    }
    
  })
  
}