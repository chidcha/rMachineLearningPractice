library(shiny)

ui <- fluidPage( 
  
  
  fluidRow(
    column(12, h2('k-nearest neighbors (k-NN)'), align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of Observations:", 20, min = 5, max = 150),
      fluidRow(column(10, h3('Making Prediction'))),
      uiOutput("testData"),
      numericInput("nn", "No of Nearest Neighbors:", 3, min = 1, max = 17),
      checkboxInput("spoint", 
                    "Show the Test Point", value = FALSE),
      checkboxInput("nnpoints", 
                    "Show Nearest Neighbours", value = FALSE),
      checkboxInput("prediction", 
                    "Make Prediction", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("plot1")
    )
  )
  
)