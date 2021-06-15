library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(janitor)
library(DT)
library(htmltools)

ui <- dashboardPage(
  dashboardHeader(title = "Diamonds dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Main",
        value = "cut",
        fluidRow(box(title = "Cut", girafeOutput("cut"), width = 12))
      )
    )
  )
)

server <- function(input, output, session) {
  tab_list <- NULL

  cut_last <- NULL
  cut_home <- NULL
  cut_close <- NULL

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
    if (length(input$cut_selected) > 0) cut_last <<- input$cut_selected
    if (!is.null(cut_last)) {
      cut_clean <- make_clean_names(paste("cl", cut_last))
      if (!(cut_clean %in% tab_list)) {
        cut_close <- paste0("close_", cut_clean)
        cut_home <- paste0("home_", cut_clean)
        output[[cut_clean]] <- renderGirafe({
          gg_clarity <- diamonds %>%
            filter(cut == cut_last) %>%
            ggplot() +
            geom_boxplot_interactive(aes(clarity, price , data_id = clarity))

          girafe(
            ggobj = gg_clarity,
            options = list(opts_selection(type = "single"))
          )
        })
        appendTab(
          inputId = "tabs",
          tabPanel(
            cut_last,
            value = cut_clean,
            fluidRow(box(
              title = "Clarity",
              actionLink(cut_close, "Close"), " | ",
              actionLink(cut_home, paste0("Go to main")),
              girafeOutput(cut_clean),
              width = 12
            ))
          )
        )
        tab_list <<- c(tab_list, cut_clean)
        observeEvent(input[[cut_close]], {
          removeTab("tabs", cut_clean)
          tab_list <<- tab_list[tab_list != cut_clean]
        })
        observeEvent(input[[cut_home]], {
          updateTabsetPanel(session, "tabs", "cut")
        })
      }
      updateTabsetPanel(session, "tabs", cut_clean)
      session$sendCustomMessage(type = "cut_set", message = character(0))
    }

    x_name <- paste0(cut_clean, "_selected")
    observeEvent(
      input[[x_name]],
      {
        clarity_name <- input[[x_name]]
        clarity_clean <- make_clean_names(paste("cl", cut_clean, clarity_name))
        if (length(clarity_name) > 0) clarity_last <<- clarity_clean
        if (!is.null(clarity_last)) {
          output[[clarity_clean]] <- renderDT(
            {
              diamonds %>%
                filter(clarity == clarity_name, cut == cut_last) %>%
                select(-clarity, -cut) %>%
                head(100)
            },
            rownames = FALSE
          )
          clarity_close <- paste0("close_", clarity_clean)
          clarity_home <- paste0("home_", clarity_clean)
          if (!(clarity_clean %in% tab_list)) {
            appendTab(
              inputId = "tabs",
              tabPanel(
                title = paste(cut_last, "-", clarity_name),
                value = clarity_clean,
                fluidRow(
                  box(
                    title = "Details",
                    actionLink(clarity_close, "Close"), " | ",
                    actionLink(clarity_home, paste0("Go to ", cut_last)),
                    br(), br(),
                    DTOutput(clarity_clean),
                    width = 10
                  )
                )
              )
            )
            tab_list <<- c(tab_list, clarity_clean)
            observeEvent(input[[clarity_close]], {
              removeTab("tabs", clarity_clean)
              tab_list <<- tab_list[tab_list != clarity_clean]
            })
            observeEvent(input[[clarity_home]], {
              updateTabsetPanel(session, "tabs", cut_clean)
            })
          }
          updateTabsetPanel(session, "tabs", clarity_clean)
          session$sendCustomMessage(type = paste0(cut_clean, "_set"), message = character(0))
        }
      }
    )
  })
}

shinyApp(ui, server)
