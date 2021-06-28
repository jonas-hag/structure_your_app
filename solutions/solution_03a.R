library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
data(diamonds, package = "ggplot2")

table_UI <- function(id, dataset) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      inputId = ns("summary_column"),
      label = "summary based on:",
      choices = colnames(dataset)
    ),
    DTOutput(
      outputId = ns("table")
    )
  )
}

table_server <- function(id, dataset, summary) {
  moduleServer(
    id,
    function(input, output, session) {
      table_data <- reactive({
        if (summary()) {
          data <- dataset %>% 
            group_by(.data[[input$summary_column]]) %>% 
            summarise(counts = n())
        } else {
          data <- dataset
        }
        
        data
      })
      
      output$table <- renderDT({
        table_data()
      })
      
      return(reactive({input$summary_column}))
    }
  )
}

ui <- fluidPage(
  checkboxInput(
    inputId = "summary",
    label = "Show summary"
  ),
  textOutput(
    outputId = "column_name"
  ),
  table_UI("mtcars_table", dataset = mtcars)
)

server <- function(input, output, session) {
  res_mtcars <- table_server("mtcars_table",
                             dataset = mtcars,
                             summary = reactive({input$summary})
  )
  
  output$column_name <- renderText({
    res_mtcars()
  })
}

shinyApp(ui, server)