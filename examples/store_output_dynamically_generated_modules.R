# This is an example app to show how to store the return values of dynamically
# generated modules in a reactive way

library(shiny)

module_UI <- function(id) {
  ns <- NS(id)
  
  div(id = id,
      selectInput(
        inputId = ns("sel_col"),
        label = "select a column",
        choices = colnames(mtcars)
      ))
}

module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(reactive({input$sel_col}))
    }
  )
}

ui <- fluidPage(
  actionButton(inputId = "add_module", label = "add"),
  textOutput(outputId = "change"),
  div(id = "add_here")
)

server <- function(input, output, session) {
  mod_results <- reactiveValues()
  observeEvent(input$add_module, {
    current_id <- paste0("id_", input$add_module)
    mod_results[[current_id]] <- module_server(id = current_id)
    insertUI(
      selector = "#add_here",
      ui = module_UI(id = current_id)
    )
  })
  
  test <- reactive({
    lapply(reactiveValuesToList(mod_results), function(current_module_output) {
      current_module_output()
    })
  })
  
  output$change <- renderPrint({
    test()
  })
}

shinyApp(ui, server)