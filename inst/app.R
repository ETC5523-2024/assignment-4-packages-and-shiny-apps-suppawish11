library(shiny)
library(tidyverse)
library(spaceobs)
library(shinythemes)

# ui
ui <- fillPage(
  navbarPage("Launching Library", id = "top",

             theme = shinytheme("cerulean"),

             # Overall ranking panel
             tabPanel("Overall ranking"),

             fluidRow(
               column(4, )
             )


  )
)

server <- function(input, output){

}


shinyApp(ui = ui, server = server)
