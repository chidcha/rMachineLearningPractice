library(shiny)

ui <- fluidPage( 
  
  fluidRow(
    column(12, h2('Decision Tree'), align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Seed", value = 1, min = 0, max = 1000)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )   
)