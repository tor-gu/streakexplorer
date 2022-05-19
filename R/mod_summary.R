summaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    #### Selected streak summary ----
    shiny::fluidRow(
      id = "summary_row",
      shiny::column(
        12,
        #shiny::h5("Streak summary"),
        shiny::h5(shiny::textOutput("streak_summary_caption")),
        shinycssloaders::withSpinner(DT::DTOutput("streak_summary"))
      )
    ),
    #### Selected streak standings row ----
    shiny::fluidRow(
      id = "standings_row",
      shiny::column(
        4,
        shiny::h5("Standings before"),
        DT::DTOutput("standings_before")
      ),
      shiny::column(
        4,
        shiny::h5("Standings after"),
        DT::DTOutput("standings_after")
      ),
      shiny::column(
        4,
        shiny::h5("Final standings"),
        DT::DTOutput("standings_final")
      )
    ),
    #### Selected streaks graphs and game_log row
    shiny::fluidRow(
      id = "graph_log_row",
      shiny::column(
        6,
        shiny::h5("Standings graph"),
        shinycssloaders::withSpinner(shiny::plotOutput("standings_graph"))
      ),
      shiny::column(
        6,
        shiny::h5("Game log"),
        shinycssloaders::withSpinner(DT::DTOutput("game_log"))
      )
    )
  )
}

summaryServer <- function(id, selected_streak) {
  moduleServer(id, function(input, output, session) {
    selected_streaks_summary_data <- reactive({
      # req(selected_streak())
      # server_streak_summary_data(franchises, selected_streak())
      req(selected_streak())
      server_streak_summary_data(franchises, selected_streak())
    })

    selected_streak_standings <- reactive({
      server_get_streak_standings(franchises, selected_streak())
    })

    # TODO Maybe move this to app.R?
    observe({
      # if (is.null(selected_streak())) {
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
      DT_standings_update(
        standings_before_proxy,
        selected_streak_standings()$streak_info,
        selected_streak_standings()$standings_before
      )
      DT_standings_update(
        standings_after_proxy,
        selected_streak_standings()$streak_info,
        selected_streak_standings()$standings_after
      )
      DT_standings_update(
        standings_final_proxy,
        selected_streak_standings()$streak_info,
        selected_streak_standings()$standings_final
      )

      game_log <- server_get_streak_game_logs(selected_streak())
      DT_game_log_update(game_log_proxy, game_log$data)
      DT_streak_summary_update(
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
      DT_streak_summary_init()
    })
    output$standings_before <- DT::renderDT({
      DT_standings_init()
    })
    output$standings_after <- DT::renderDT({
      DT_standings_init()
    })
    output$standings_final <- DT::renderDT({
      DT_standings_init()
    })
    output$game_log <- DT::renderDT({
      DT_game_log_init()
    })

    # Standings graph
    output$standings_graph <- renderPlot({
      req(selected_streak())
      server_build_streak_standings_graph(franchises, selected_streak())
    })

  })
}
