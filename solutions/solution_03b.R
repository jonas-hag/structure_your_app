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
      
      mean_val <- reactive({
        dataset %>% 
          pull(.data[[input$summary_column]]) %>% 
          mean()
      })
      
      return(
        list(
          selected_col = reactive({input$summary_column}),
          col_mean = mean_val
        )
      )
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
  textOutput(
    outputId = "column_mean"
  ),
  table_UI("mtcars_table", dataset = mtcars)
)

server <- function(input, output, session) {
  res_mtcars <- table_server("mtcars_table",
                             dataset = mtcars,
                             summary = reactive({input$summary})
  )
  
  output$column_name <- renderText({
    res_mtcars$selected_col()
  })
  
  output$column_mean <- renderText({
    res_mtcars$col_mean()
  })
}

shinyApp(ui, server)