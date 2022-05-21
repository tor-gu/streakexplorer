summaryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    #### Selected streak summary ----
    shiny::fluidRow(
      id = ns("summary_row"),
      shiny::column(
        12,
        shiny::h5(shiny::textOutput(ns("streak_summary_caption"))),
        shinycssloaders::withSpinner(DT::DTOutput(ns("streak_summary")))
      )
    ),
    #### Selected streak standings row ----
    shiny::fluidRow(
      id = ns("standings_row"),
      shiny::column(
        4,
        shiny::h5("Standings before"),
        DT::DTOutput(ns("standings_before"))
      ),
      shiny::column(
        4,
        shiny::h5("Standings after"),
        DT::DTOutput(ns("standings_after"))
      ),
      shiny::column(
        4,
        shiny::h5("Final standings"),
        DT::DTOutput(ns("standings_final"))
      )
    ),
    #### Selected streaks graphs and game_log row
    shiny::fluidRow(
      id = ns("graph_log_row"),
      shiny::column(
        6,
        shiny::h5("Standings graph"),
        shinycssloaders::withSpinner(shiny::plotOutput(ns("standings_graph")))
      ),
      shiny::column(
        6,
        shiny::h5("Game log"),
        shinycssloaders::withSpinner(DT::DTOutput(ns("game_log")))
      )
    )
  )
}

summaryServer <- function(id, db_pool, highlight_colors, franchises,
                          selected_streak) {
  moduleServer(id, function(input, output, session) {
    selected_streaks_summary_data <- reactive({
      req(selected_streak())
      summary_server_streak_summary_data(db_pool, franchises, selected_streak())
    })

    selected_streak_standings <- reactive({
      summary_server_get_streak_standings(db_pool, franchises,
                                          selected_streak())
    })

    observe({
      if (is.null(selected_streak())) {
        shinyjs::hide("summary_row")
        shinyjs::hide("standings_row")
        shinyjs::hide("graph_log_row")
      } else {
        shinyjs::show("summary_row")
        shinyjs::show("standings_row")
        shinyjs::show("graph_log_row")
      }
    })

    observeEvent(selected_streak(), {
      summary_ui_DT_standings_update(
        standings_before_proxy,
        selected_streak_standings()$streak_info,
        selected_streak_standings()$standings_before
      )
      summary_ui_DT_standings_update(
        standings_after_proxy,
        selected_streak_standings()$streak_info,
        selected_streak_standings()$standings_after
      )
      summary_ui_DT_standings_update(
        standings_final_proxy,
        selected_streak_standings()$streak_info,
        selected_streak_standings()$standings_final
      )

      game_log <- summary_server_get_streak_game_logs(db_pool,
                                                      selected_streak())
      summary_ui_DT_game_log_update(game_log_proxy, game_log$data)
      summary_ui_DT_streak_summary_update(
        streak_summary_proxy,
        selected_streaks_summary_data()$data
      )
      output$streak_summary_caption <- renderText(
        selected_streaks_summary_data()$caption)
    })

    ## Proxies ----
    streak_summary_proxy <- DT::dataTableProxy("streak_summary",
                                               session = session)
    standings_before_proxy <- DT::dataTableProxy("standings_before",
                                                 session = session)
    standings_after_proxy <- DT::dataTableProxy("standings_after",
                                                session = session)
    standings_final_proxy <- DT::dataTableProxy("standings_final",
                                                session = session)
    game_log_proxy <-
      DT::dataTableProxy("game_log", session = session)

    # Data tables.
    # For each of these, we do an initial render with a dummy table, and then
    # handle the updates through proxies
    output$streak_summary <- DT::renderDT({
      summary_ui_DT_streak_summary_init()
    })
    output$standings_before <- DT::renderDT({
      summary_ui_DT_standings_init(highlight_colors)
    })
    output$standings_after <- DT::renderDT({
      summary_ui_DT_standings_init(highlight_colors)
    })
    output$standings_final <- DT::renderDT({
      summary_ui_DT_standings_init(highlight_colors)
    })
    output$game_log <- DT::renderDT({
      summary_ui_DT_game_log_init()
    })

    # Standings graph
    output$standings_graph <- renderPlot({
      req(selected_streak())
      summary_server_build_streak_standings_graph(db_pool, highlight_colors,
                                                  franchises, selected_streak())
    })

  })
}
