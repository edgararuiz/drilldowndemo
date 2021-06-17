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
        title = "Cut",
        value = "cut",
        fluidRow(
          box(
            sliderInput("carat", "Carats", 0, 5.1, step = 0.1, value = c(0.1, 0.5)),
            width = 6
          )
        ),
        fluidRow(
          box(title = "Cut", girafeOutput("cut"), width = 6)
        )
      )
    )
  )
)

server <- function(input, output, session) {

  tab_list <- NULL

  output$cut <- renderGirafe({
    gg_cut <- diamonds %>%
      filter(carat >= input$carat[[1]], carat <= input$carat[[2]]) %>%
      group_by(cut) %>%
      summarise(average_price = median(price)) %>%
      ggplot() +
      geom_col_interactive(aes(cut, average_price, data_id = cut, tooltip = average_price)) +
      coord_flip()

    girafe(
      ggobj = gg_cut,
      options = list(opts_selection(type = "single"))
    )
  })

  observeEvent(input$cut_selected, {

    cut_last <- input$cut_selected
    carat_from <- isolate(input$carat[[1]])
    carat_to <- isolate(input$carat[[2]])
    clarity_title <- paste(cut_last, "|", carat_from, "-", carat_to)
    cut_clean <- make_clean_names(clarity_title)

    if(!(cut_clean %in% tab_list)) {

      tab_list <<- c(tab_list, cut_clean)

      output[[cut_clean]] <- renderGirafe({
        gg_clarity <- diamonds %>%
          filter(
            carat >= carat_from,
            carat <= carat_to,
            cut == cut_last
          ) %>%
          ggplot() +
          geom_boxplot_interactive(aes(clarity, price, data_id = clarity)) +
          labs(title = clarity_title)

        girafe(
          ggobj = gg_clarity,
          options = list(opts_selection(type = "single"))
        )
      })
      appendTab(
        inputId = "tabs",
        tabPanel(
          clarity_title,
          value = cut_clean,
          fluidRow(box(
            title = clarity_title,
            girafeOutput(cut_clean),
            width = 12
          ))
        )
      )

    }
    updateTabsetPanel(session, "tabs", cut_clean)
    session$sendCustomMessage(type = "cut_set", message = character(0))
  })
}

shinyApp(ui, server)
