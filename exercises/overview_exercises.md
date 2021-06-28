# Exercises
If you get stuck but need the results from a previous exercise to continue, you
can have a look in the [solutions folder](../solutions)

## Excercise 1
modularise the app functionality:

- basically, rewrite the functionality of the below app in a module
- write a module that displays a dataset as a DT table. If the summary checkbox
is ticked, instead of the raw data show the summary table based on the
`summary_column` selection
- the module should have a dataset input argument (hint: think about which of
the module `UI`/`server` functions need this argument)
- write an app that shows the table for the `mtcars`, `diamonds` and `CO2` dataset
- for this, use the module

```r
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
```

## Exercise 2
- rewrite the code from exercise 1 so that the module does not have a summary
`checkboxInput`
- instead there is only one summary `checkboxInput` in the main app
- rewrite the module from exercise 1 so that it has an additional
summary argument where you pass the summary value from the main app
- now all datasets should change from raw view to summary view simultaneously

## Exercise 3a
- rewrite the code from exercise 2 so that the module returns the selected
column name reactively
- in the main app, use this module for `mtcars`
- in the main app, show the column name that is selected in the module in a
`textOutput`

## Exercise 3b
- rewrite the code from exercise 3a so that the module also returns the mean of
the selected column
- to calculate the mean within the module, use a `reactive`,
e.g. `mean_column <- reactive({})`
- in the main app, show the mean of the selected column in a `textOutput` below
the column name

## Exercise 3c
if you didn't do the two previous exercises:

- rewrite the code from exercise 2 so that the module returns the selected
column name reactively and also the mean of the selected column
- to calculate the mean within the module, use a `reactive`,
e.g. `mean_column <- reactive({})`

if you did the previous exercises, start with the code from exercise 3b:

- write another module that takes a dataset (non reactive) and a selected
column name (reactive) as inputs and plots a boxplot of the selected column
- in the main app, use these two modules for `mtcars` so that the column that
is selected in the table module is passed to the plot module

## Exercise 4
- write an app where you can dynamically add the `graph2` module
- the main app should have a `selectInput` where you can choose a column name
from the `mtcars` dataset and an `actionButton` to add a module
- when the module is created, the module should include the selected column
at creation time and shouldn't change afterwards (not reactive!)

hint:

- include a mechanism so that the added modules have unique IDs

```r
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
```

## Exercise 5
- extend the code from exercise 4 so that it now also includes an `actionButton`
to remove the last added module
- the removal should work repeatedly until no module is left
- one should be possible to add modules at any point

hint:

- the module id naming needs to be sequential
