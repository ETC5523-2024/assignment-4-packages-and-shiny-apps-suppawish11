library(shiny)
library(tidyverse)
library(spaceobs)
library(shinythemes)
library(kableExtra)
library(plotly)

# ui
ui <- fillPage(
  navbarPage("Space Launching", id = "top",

             theme = shinytheme("cerulean"),

             # Overall ranking panel
             tabPanel("Overall ranking",

             fluidRow(
               column(8,
                      sliderInput(
                        inputId = "year",
                        label = "Year",
                        min = min(space_objects$year),
                        max = max(space_objects$year),
                        value = 2023,
                        sep = ""
                      ))
             ),

             fluidRow(
               column(5, mainPanel(
                 htmlOutput("title_rank")
                 )),

                column(7,
                       mainPanel(
                         htmlOutput("title_rankplot")
                       ))
             ),

             fluidRow(
               style = "max-height: 60vh; overflow-y: auto;",

               column(5,


                      htmlOutput("ranking", height = 500)
             ),
              column(7,
                    plotlyOutput("rank_plot", height = 400)
                    )
             )
          ),

          tabPanel("Launched count",
                   fluidRow(
                     column(6,
                            selectInput(
                              inputId = "entity",
                              label = "Interested Entity?",
                              choices = space_objects$entity
                            )

                     ),

                     column(6,

                            mainPanel(htmlOutput("totallatest")))
                  ),

                  fluidRow(column(6,
                                  mainPanel(
                                    htmlOutput("title_time")
                                  ))
                           ),

                  fluidRow(
                    style = "max-height: 60vh; overflow-y: auto;",


                    column(6,
                           plotlyOutput("cumuplot", height = 400)
                           )
                  )
          )
      )
)

server <- function(input, output){

  #Title
  output$title_rank <- renderText(
    "<h4><b>Overall objects launched ranking</b></h4>"
  )

  output$title_rankplot <- renderText(
    "<h4><b>Bar plot of overall ranking in selected year</b></h4>"
  )

  output$title_time <- renderText(
    "<h4><b>Cumulative objects launched of each entity</b></h4>"
  )

  # Overall ranking output
  observe({
    req(input$year)

    #Ranking table
    output$ranking <- renderText({
      space_objects |>
        filter(year <= input$year, !entity == "World") |>
        group_by(entity) |>
        summarise(total = sum(num_objects)) |>
        arrange(-total) |>
        head(8) |>
        kable(col.names = c("Entity", "Total space objects launched")) |>
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
    })

    # Ranking plot
    output$rank_plot <- renderPlotly({

      ggplotly(space_objects |>
        filter(year <= input$year, !entity == "World") |>
        group_by(entity) |>
        summarise(total = sum(num_objects)) |>
        arrange(-total) |>
        head(8) |>
        mutate(entity = fct_reorder(entity, -total)) |>
        ggplot(aes(x = entity, y = total)) +
        geom_col() +
        labs(x = "Entity",
             y = "Total space objects launched") +
        theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1)))

    })
  })

  #Launched count
  observe({
    req(input$entity)

    #Total count latest year
    output$totallatest <- renderText({
      space_objects |>
        filter(entity == input$entity) |>
        group_by(entity) |>
        summarize(total = sum(num_objects)) |>
        kable(col.names = c("Entity", "Total objects launched")) |>
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = TRUE)
    })

    # Cumulative count
    output$cumuplot <- renderPlotly({

      ggplotly(space_objects |>
      group_by(entity) |>
      mutate(cumulative_launch = cumsum(num_objects)) |>
      filter(entity == input$entity) |>
      ggplot(aes(x = year,
                 y = cumulative_launch)) +
      geom_line(color = "red") +
      geom_point(size = 0.5, color = "red") +
      labs(x = "Year", y = "Total cumulative launched in each year"))

    })
  })


}


shinyApp(ui = ui, server = server)
