library(shiny)
library(tidyverse)
library(spaceobs)
library(kableExtra)
library(plotly)
library(shinydashboard)
library(DT)


# ui
ui <- dashboardPage(
  dashboardHeader(title = span("Space Launching",
                               style = "font-family: 'Courier New';
                               font-weight: bold;")),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Overall ranking", tabName = "overall", icon = icon("ranking-star")),
    menuItem("Launch count", icon = icon("rocket"), tabName = "launch"),
    menuItem("Data description", icon = icon("database"), tabName = "data")
  )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(

      '

      .content {
      background-color: #d8eef2;
      }

      .skin-blue .main-sidebar {
      background-color: #d8eef2;
      font-family: Courier New;
      font-weight: bold;
      }

      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #31a8bd;
                                }

      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #d8eef2;
                                color: #000000;
                                }
      '


      ))),
    tabItems(

      tabItem(tabName = "overall",
              fluidRow(
                box(
                  title = htmlOutput("itp_tbl"), width = 12, solidHeader = F, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  p("By selecting the year that you are interested in from the slider bar below,
                  the table will show the top 10 ranking of the entity based on the total objects launched in the selected year since 1957.
                    Moreover, it will generate a bar plot in the left side showing the top 10 ranking same as the table shown on the right side.")
                )
              ),

              fluidRow(
                column(12,
                       wellPanel(sliderInput(
                         inputId = "year",
                         label = "Year",
                         min = min(space_objects$year),
                         max = max(space_objects$year),
                         value = 2023,
                         sep = ""
                       )))
              ),

              fluidRow(
                box(
                  title = htmlOutput("title_rank"), width = 6, solidHeader = TRUE, status = "primary",
                  htmlOutput("ranking", height = 500)
                ),

                box(
                  title = htmlOutput("title_rankplot"), width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("rank_plot", height = 400)
                )
              )
      ),


      tabItem(tabName = "launch",

              fluidRow(
                column(6,
                       wellPanel(selectizeInput(
                         inputId = "entity",
                         label = "Interested Entity?",
                         choices = space_objects$entity
                       ))

                  ),

                column(6,
                       valueBoxOutput("progressBox"))
                ),

              fluidRow(
                box(
                  title = htmlOutput("itp_cumu"), width = 6, solidHeader = F, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  p("By selecting the entity that you are interested in the drop down box above,
                    it will generate a time serie plot showing the cumulative count of the total object launched into outer space of the selected entity in each year.")
                ),
                box(
                  title = htmlOutput("itp_numcount"), width = 6, solidHeader = F, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  p("By selecting the entity that you are interested in the drop down box above,
                    it will generate a time serie plot showing the count of object launeded into outer space of the selected entity in each year.")
                )
              ),

              fluidRow(
                box(
                  title = htmlOutput("title_time"), width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("cumuplot", height = 400)
                ),

                box(
                  title = htmlOutput("title_yearplot"), width = 6, solidHeader = TRUE, status = "primary",
                  plotlyOutput("yearplot", height = 400)
                )
              )
              ),

      tabItem(tabName = "data",
              uiOutput("desc"),

              fluidRow(wellPanel(dataTableOutput("datatable")))
              )



              )

    )
  )


# Server
server <- function(input, output){

  #Title
  output$title_rank <- renderText(
    "<h4><b>Top 10 overall objects launched ranking in selected year</b></h4>"
  )

  output$title_rankplot <- renderText(
    "<h4><b>Bar plot of top 10 overall ranking in selected year</b></h4>"
  )

  output$title_time <- renderText(
    "<h4><b>Cumulative objects launched of each entity</b></h4>"
  )

  output$title_yearplot <- renderText(
    "<h4><b>Number of objects launched of each year</b></h4>"
  )

  output$itp_tbl <- renderText(
    "<h4><b>Guide for interpretation of ranking table and plot</b></h4>"
  )

  output$itp_cumu <- renderText(
    "<h4><b>Guide for interpretation of cumulative plot</b></h4>"
  )

  output$itp_numcount <- renderText(
    "<h4><b>Guide for interpretation of object count plot</b></h4>"
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
        head(10) |>
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
        head(10) |>
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

    # Cumulative count
    output$cumuplot <- renderPlotly({

      ggplotly(space_objects |>
      group_by(entity) |>
      mutate(cumulative_launch = cumsum(num_objects)) |>
      filter(entity == input$entity) |>
      ggplot(aes(x = year,
                 y = cumulative_launch)) +
      geom_line(color = "red") +
      geom_point(size = 1, color = "red") +
      labs(x = "Year", y = "Total cumulative launched each year") +
        scale_x_continuous(breaks = seq(1957, 2023, by = 8),
                           limits = c(1957, 2023)))

    })


    output$yearplot <- renderPlotly({
      ggplotly(
        space_objects |>
          filter(entity == input$entity) |>
          ggplot(aes(x = year, y = num_objects)) +
          geom_line(color = "red") +
          geom_point(size = 1, color = "red") +
          labs(x = "Year", y = "Number of objects launched each year") +
          scale_x_continuous(breaks = seq(1957, 2023, by = 8),
                             limits = c(1957, 2023))
      )
    })

    output$progressBox <- renderValueBox({
      valueBox(
               space_objects |>
                 filter(entity == input$entity) |>
                 group_by(entity) |>
                 summarize(total = sum(num_objects)) |>
                 select(total) |>
                 pull(total),
               "Total objects launched in 2023",
               icon = icon("rocket"),
               color = "blue")})
  })


  # data desc
  output$desc <- renderUI({

    knitr::knit("data-desc.qmd", quiet = TRUE) |>
      markdown::markdownToHTML(fragment.only = TRUE) |>
      HTML()

  })

  output$datatable <- renderDataTable({
    space_objects
  })



}


shinyApp(ui = ui, server = server)
