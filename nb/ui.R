library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("versicolor", "Versicolor Distribution", value = TRUE),
      checkboxInput("virginica", "Virginica Distribution", value = TRUE),
      checkboxInput("boundary", "Show Prediction Mechanism", value = FALSE),
      numericInput("pw", "Petal Length", 6, min = 2.5, max = 7.5, step = 0.1)
      ),
    mainPanel(
      fluidRow(column(10,tableOutput("table1"), align = "center")),
      plotOutput("plot1")
    )
  ))
  
