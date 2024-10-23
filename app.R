library(shiny)
source('ui.R')
source('server.R')

runApp(shinyApp(ui = ui, server = server), launch.browser = F)