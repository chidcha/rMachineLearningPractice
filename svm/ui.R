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
                    "Show the decision boundary", value = FALSE),
      fluidRow(column(10, h3('Making Prediction'))),
      selectInput("index", "Observation number", 51:150, 
                   selected = 100),
      checkboxInput("spoint", 
                    "Show the Test Point", value = FALSE),
      checkboxInput("prediction", 
                    "Make Prediction", value = FALSE)
    ),
    
    mainPanel(
      fluidRow(column(10, h4('Data Set'), align = "left")),
      tableOutput("table"),
      fluidRow(column(10, h4('Corresponding Points and Decision Boundary'), align = "left")),
      plotOutput("plot")
    )
      )   
  )