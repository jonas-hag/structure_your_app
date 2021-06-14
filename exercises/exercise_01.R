# Exercise 1
# modularise the app functionality:
# - Write a module that displays a dataset as a DT table. If the summary checkbox
#   is ticked, instead of the raw data show the summary table based on the
#   summary_column selection
# - the module should have a dataset input argument
# - write an app that shows the table for the mtcars, diamonds and CO2 dataset
# - for this, use the module

library(shiny)
library(DT)
library(dplyr)
data(diamonds, package = "ggplot2")

ui <- fluidPage(
  checkboxInput(
    inputId = "summary",
    label = "show summary",
    value = FALSE
  ),
  selectInput(
    inputId = "summary_column",
    label = "summary based on:",
    choices = colnames(mtcars)
  ),
  DTOutput(
    outputId = "table"
  )
)

server <- function(input, output, session) {
  table_data <- reactive({
    if (input$summary) {
      data <- mtcars %>% 
        group_by(.data[[input$summary_column]]) %>% 
        summarise(counts = n())
    } else {
      data <- mtcars
    }
    
    data
  })
  
  output$table <- renderDT({
    table_data()
  })
}

shinyApp(ui, server)