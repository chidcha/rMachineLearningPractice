library(shiny)

ui <- fluidPage( 
    
    navbarPage("Logistic Regression",
               navlistPanel(
                 tabPanel("Data Set",
                          fluidRow(fluidRow(
                            column(6, align="center", checkboxInput("versicolor", 
                                                    "Versicolor Distribution", value = TRUE)),
                            column(6, align="center", checkboxInput("virginica", 
                                                    "Virginica Distribution", value = TRUE))),
                            
                            fluidRow(
                              column(12, align="center", tableOutput("table1"))
                          ),
                          fluidRow(
                            column(12, plotOutput("plot1"))
                          ))),
                 
                 
                 tabPanel("Logistic Regression Model",
                          fluidRow(
                            column(6, align="left", numericInput("p", "Cut-off value (Virginica)", value = 0.5, min = 0, 
                                                                    max = 1, step = 0.05))),
                          fluidRow(
                            column(6, align="left", checkboxInput("boundary", 
                                                                    "Show the decision boundary", value = FALSE))),
                          fluidRow(
                            column(10, plotOutput("plot2"))),
                          fluidRow(
                            column(6, align = "center", tableOutput("table2"))
                          )
                          
                            
               ))
  
 
  )   
)