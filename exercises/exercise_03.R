# Exercise 3
# - rewrite the code from exercise 2 so that the module returns the selected
#   column reactively and also the mean of the selected column
# - to calculate the mean within the module, use a `reactive`,
#   e.g. mean_column <- reactive({})
# - write another module that takes a dataset (non reactive) and a selected
#   column (reactive) as inputs and plots a boxplot of the selected column
# - in the main app, use these two modules for mtcars so that the column that
#   is selected in the table module is passed to the plot module