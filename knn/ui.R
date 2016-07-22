library(shiny)

ui <- fluidPage( 
  
  
  fluidRow(
    column(12, h2('k-nearest neighbors (k-NN)'), align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of Observations:", 20, min = 5, max = 150),
      numericInput("nn", "No of Nearest Neighbors:", 3, min = 1, max = 17),
      uiOutput("testData")
    ),
    
    mainPanel(
      plotOutput("plot1")
    )
  )
  
)