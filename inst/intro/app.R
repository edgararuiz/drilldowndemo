library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(janitor)

ui <- dashboardPage(
  dashboardHeader(title = "Diamonds dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Main",
        value = "cut",
        fluidRow(
          box(title = "Cut", girafeOutput("cut"), width = 6),
          box(title = "Clarity", girafeOutput("clarity"), width = 6)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$cut <- renderGirafe({
    gg_cut <- diamonds %>%
      group_by(cut) %>%
      summarise(avgerage_price = median(price)) %>%
      ggplot() +
      geom_col_interactive(aes(cut, avgerage_price, data_id = cut)) +
      coord_flip()

    girafe(
      ggobj = gg_cut,
      options = list(opts_selection(type = "single"))
    )
  })

  observeEvent(input$cut_selected, {
    cut_last <- input$cut_selected
    if (!is.null(cut_last)) {
      output$clarity <- renderGirafe({
        gg_clarity <- diamonds %>%
          filter(cut == cut_last) %>%
          ggplot() +
          geom_boxplot_interactive(aes(clarity, price, data_id = clarity)) +
          labs(title = cut_last)

        girafe(
          ggobj = gg_clarity,
          options = list(opts_selection(type = "single"))
        )
      })
      session$sendCustomMessage(type = "cut_set", message = character(0))
    }
  })
}

shinyApp(ui, server)
