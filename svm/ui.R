library(shiny)

ui <- fluidPage( 
  
  fluidRow(
    column(12, h2('Support Vector Machine'), align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("kernal", "Kernal", c("linear", "radial"), 
                  selected = "linear"),
      numericInput("cost", "Cost", value = 1, min = 0, max = 1000),
      numericInput("gamma", "Gamma", value = 0.5),
      checkboxInput("boundary", 
                    "Show the decision boundary", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
      )   
  )