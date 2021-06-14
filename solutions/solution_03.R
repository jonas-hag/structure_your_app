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

graph_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(
      outputId = ns("plot_1")
    )
  )
}

graph_server <- function(id, dataset, column) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot_1 <- renderPlot({
        ggplot(dataset, aes(x = .data[[column()]])) +
          geom_boxplot()
      })
    }
  )
}

ui <- fluidPage(
  checkboxInput(
    inputId = "summary",
    label = "Show summary"
  ),
  textOutput(
    outputId = "column_mean"
  ),
  table_UI("mtcars_table", dataset = mtcars),
  graph_UI("mtcars_graph")
)

server <- function(input, output, session) {
  res_mtcars <- table_server("mtcars_table",
                             dataset = mtcars,
                             summary = reactive({input$summary})
  )
  
  output$column_mean <- renderText({
    res_mtcars$col_mean()
  })
  
  graph_server("mtcars_graph",
               dataset = mtcars,
               column = res_mtcars$selected_col)
}

shinyApp(ui, server)