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

ui <- fluidPage(
  selectInput(
    inputId = "selected_col",
    label = "select a column",
    choices = colnames(mtcars)
  ),
  actionButton(
    inputId = "add_module",
    label = "add a plot"
  ),
  actionButton(
    inputId = "remove_module",
    label = "remove last plot"
  ),
  div(
    id = "add_here"
  )
)

server <- function(input, output, session) {
  counter <- reactiveVal(0)
  observeEvent(input$add_module, {
    counter(counter() + 1)
    current_id <- paste0("module_", counter())
    graph2_server(
      id = current_id,
      column = input$selected_col
    )
    insertUI(
      selector = "#add_here",
      ui = graph2_UI(id = current_id)
    )
  })
  
  observeEvent(input$remove_module, {
    # only remove a module if there is one left to be removed
    if (counter() > 0) {
      current_id <- paste0("module_", counter())
      removeUI(
        selector = paste0("#", current_id)
      )
      counter(counter() - 1)
    }
  })
}

shinyApp(ui, server)