# This app shows a simple module based app

library(shiny)
library(ggplot2)

plot_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      inputId = ns("selected_col"),
      label = "selected column",
      choices = colnames(mtcars)
    ),
    plotOutput(
      outputId = ns("plot")
    )
  )
}

plot_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        ggplot(mtcars, aes(x = .data[[input$selected_col]], y = mpg)) +
          geom_point()
      })
    }
  )
}

ui <- fluidPage(
  plot_UI(
    id = "module_1"
  )
)

server <- function(input, output, session) {
  plot_server(
    id = "module_1"
  )
}

shinyApp(ui, server)