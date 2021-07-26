library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(janitor)
library(DT)

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
    cut_selected <- input$cut_selected
    carat_from <- isolate(input$carat[[1]])
    carat_to <- isolate(input$carat[[2]])
    cut_title <- paste(cut_selected, "|", carat_from, "-", carat_to)
    cut_clean <- make_clean_names(cut_title)

    if (!(cut_clean %in% tab_list)) {
      tab_list <<- c(tab_list, cut_clean)

      output[[cut_clean]] <- renderGirafe({
        gg_clarity <- diamonds %>%
          filter(
            carat >= carat_from,
            carat <= carat_to,
            cut == cut_selected
          ) %>%
          ggplot() +
          geom_boxplot_interactive(aes(clarity, price, data_id = clarity)) +
          labs(title = cut_title)

        girafe(
          ggobj = gg_clarity,
          options = list(opts_selection(type = "single"))
        )
      })

      cut_close <- paste0("close_", cut_clean)
      cut_home <- paste0("home_", cut_clean)

      appendTab(
        inputId = "tabs",
        tabPanel(
          cut_title,
          value = cut_clean,
          fluidRow(box(
            title = cut_title,
            actionLink(cut_close, "Close"), " | ",
            actionLink(cut_home, paste0("Go to main")),
            girafeOutput(cut_clean),
            width = 12
          ))
        )
      )

      observeEvent(input[[cut_close]], {
        removeTab("tabs", cut_clean)
        tab_list <<- tab_list[tab_list != cut_clean]
        updateTabsetPanel(session, "tabs", "cut")
      })
      observeEvent(input[[cut_home]], {
        updateTabsetPanel(session, "tabs", "cut")
      })
    }
    updateTabsetPanel(session, "tabs", cut_clean)
    session$sendCustomMessage(type = "cut_set", message = character(0))

    cut_clean_selected <- paste0(cut_clean, "_selected")
    observeEvent(
      input[[cut_clean_selected]],
      {
        clarity_name <- input[[cut_clean_selected]]
        clarity_title <- paste(cut_title, "|", clarity_name)
        clarity_clean <- make_clean_names(clarity_title)
        output[[clarity_clean]] <- renderDT(
          {
            diamonds %>%
              filter(
                clarity == clarity_name,
                cut == cut_selected,
                carat >= carat_from,
                carat <= carat_to
              ) %>%
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
              title = clarity_title,
              value = clarity_clean,
              fluidRow(
                box(
                  title = "Details",
                  actionLink(clarity_close, "Close"), " | ",
                  actionLink(clarity_home, paste0("Go to ", cut_title)),
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
            updateTabsetPanel(session, "tabs", cut_clean)
          })
          observeEvent(input[[clarity_home]], {
            updateTabsetPanel(session, "tabs", cut_clean)
          })
        }
        updateTabsetPanel(session, "tabs", clarity_clean)
        session$sendCustomMessage(type = paste0(cut_clean, "_set"), message = character(0))
      }
    )
  })
}

shinyApp(ui, server)
