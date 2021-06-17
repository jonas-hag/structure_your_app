# Exercise 4:
# - write an app where you can dynamically add the graph2 module
# - the main app should have a selectInput where you can choose a column name
#   from the mtcars dataset and an actionButton to add a module
# - when the module is created, the module should include the selected column
#   at creation time and shouldn't change afterwards (not reactive!)
# hint:
# - include a mechanism so that the added modules have unique IDs (in best case
#   sequential)

library(shiny)
library(ggplot2)

graph2_UI <- function(id) {
  ns <- NS(id)
  
  div(
    id = id,
    selectInput(
      inputId = ns("plottype"),
      label = "plot type",
      choices = c("boxplot", "histogram")
    ),
    plotOutput(
      outputId = ns("plot_1")
    )
  )
}

graph2_server <- function(id, column) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot_1 <- renderPlot({
        p <- ggplot(mtcars, aes(x = .data[[column]]))
        
        if (input$plottype == "boxplot") {
          p <- p + geom_boxplot()
        } else {
          p <- p + geom_histogram()
        }
        
        p
        
      })
    }
  )
}