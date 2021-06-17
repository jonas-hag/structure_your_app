# This app showcases how modules can be nested

library(shiny)
library(ggplot2)
library(DT)

selector_UI <- function(id, dataset) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      inputId = ns("selected_col"),
      label = "Selected column",
      choices = colnames(dataset)
    ),
    textOutput(
      outputId = ns("range")
    )
  )
}

selector_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      col_range <- reactive({
        range(dataset[, input$selected_col])
      })
      output$range <- renderText({
        paste0("The range is ", col_range()[1], " - ", col_range()[2])
      })
      
      return(reactive({input$selected_col}))
    }
  )
}

table_ui <- function(id, dataset) {
  ns <- NS(id)
  
  tagList(
    selector_UI(
      id = ns("my_col"),
      dataset = dataset
    ),
    DTOutput(
      outputId = ns("table")
    )
  )
}

table_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      selected_col <- selector_server(
        id = "my_col",
        dataset = dataset
      )
      
      output$table <- renderDT({
        dataset[, selected_col(), drop = FALSE]
      })
    }
  )
}

graph_ui <- function(id, dataset) {
  ns <- NS(id)
  
  tagList(
    selector_UI(
      id = ns("my_col"),
      dataset = dataset
    ),
    plotOutput(
      outputId = ns("plot")
    )
  )
}

graph_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      selected_col <- selector_server(
        id = "my_col",
        dataset = dataset
      )
      
      output$plot <- renderPlot({
        ggplot(dataset, aes(x = .data[[selected_col()]])) +
          geom_boxplot()
      })
    }
  )
}

ui <- fluidPage(
  table_ui(
    id = "table_1",
    dataset = mtcars
  ),
  graph_ui(
    id = "plot_1",
    dataset = mtcars
  )
)

server <- function(input, output, session) {
  table_server(
    id = "table_1",
    dataset = mtcars
  )
  graph_server(
    id = "plot_1",
    dataset = mtcars
  )
}

shinyApp(ui, server)