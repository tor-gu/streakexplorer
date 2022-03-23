library(shiny)


initial_year_range <- c(1948, 1960)
theme <- bslib::bs_theme(
  bootswatch = "slate",
  heading_font = "1.2",
  font_scale = 0.8
)
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$style("#game_log td, th {padding: 0; text-align: right}"),
  tags$style("#streak_summary td, th {padding: 0; text-align: right}"),
  tags$style("#standings_before td, th {padding: 0; text-align: right}"),
  tags$style("#standings_after td, th {padding: 0; text-align: right}"),
  tags$style("#standings_final td, th {padding: 0; text-align: right}"),
  theme = theme,
  # theme=bslib::bs_theme(),
  titlePanel("Baseball Streak Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      fluidRow(
        column(
          12,
          sliderInput("years", "Years",
            min = 1948, max = 2021, step = 1,
            value = initial_year_range, sep = ""
          )
        )
      ),
      fluidRow(
        column(
          9,
          selectInput("leagues", "League",
            choices = c("All Leagues" = "BOTH", "AL" = "AL", "NL" = "NL")
          )
        ),
        column(3, )
      ),
      fluidRow(
        column(
          9,
          selectInput("divisions", "Divisions",
            choices = list(),
            multiple = TRUE
          ),
        ),
        column(
          3,
          checkboxInput("divisions_all", "All", value = TRUE)
        )
      ),
      fluidRow(
        column(
          9,
          selectInput("teams", "Teams", choices = list(), multiple = TRUE),
        ),
        column(
          3,
          checkboxInput("teams_all", "All", value = TRUE)
        )
      ),
      radioButtons("streak_type", "Streak Type",
        choices = c("HOT", "COLD"),
        selected = "HOT"
      )
    ),
    mainPanel(
      width = 7,
      fluidRow(
        column(12, plotly::plotlyOutput(outputId = "streaks"))
        ),
      fluidRow(
        column(12, h4(textOutput("streak_summary_caption")),
               shinycssloaders::withSpinner(DT::DTOutput("streak_summary")))
        ),
      fluidRow(id="standings_row",
        column(4, h5("Standings before"),
               DT::DTOutput("standings_before")),
        column(4, h5("Standings after"),
               DT::DTOutput("standings_after")),
        column(4, h5("Final standings"),
               DT::DTOutput("standings_final"))
      ),
      fluidRow(id="graph_log_row",
        column(6, h5("Standings graph"),
               shinycssloaders::withSpinner(plotOutput("standings_graph"))),
        column(6, h5("Game log"),
               shinycssloaders::withSpinner(DT::DTOutput("game_log")))
      )
    )
  )
)

server <- function(input, output, session) {
  # bslib::bs_themer()

  # Reactives and reactive values ----
  hot <- reactive({
    input$streak_type == "HOT"
  })

  lines_to_streaks <- reactive({
    message("loading lines_to_streaks")
    if (hot()) {
      SOMData::hot_streaks_lines_to_streaks
    } else {
      SOMData::cold_streaks_lines_to_streaks
    }
  })

  concordances <- reactive({
    message("loading concordances")
    if (hot()) {
      SOMData::hot_streaks_concordances
    } else {
      SOMData::cold_streaks_concordances
    }
  })

  years <- reactive({ input$years }) %>% debounce(333)
  max_rank <- reactiveVal(10)
  selected_line_id <- reactiveVal(NULL)
  selected_years <- reactive({ years()[[1]]:years()[[2]] })
  selected_leagues <- reactiveVal(c("AL", "NL"))
  selected_league_divisions <- reactiveVal(
    list("AL_None", "NL_None") %>%
      division_choice_values_as_league_and_division_list()
  )

  divisions_choices <- reactive({
    SOMData::franchises %>%
      filter_by_years(selected_years()) %>%
      filter_by_league(selected_leagues()) %>%
      truncate_years(selected_years()) %>%
      get_divisions() %>%
      generate_division_selection()
  })

  teams_choices <- reactive({
    SOMData::franchises %>%
      filter_by_years(selected_years()) %>%
      filter_by_league_divisions(selected_league_divisions()) %>%
      truncate_years(selected_years()) %>%
      generate_team_selection()
  })

  no_divisions_choices <- reactive({
    all(
      unname(unlist(divisions_choices())) %in% c("AL_None", "NL_None")
    )
  })

  filtered_lines <- reactive({
    req(years(), input$teams)
    message("filtering lines")
    max_rank(sql_get_max_rank(years()[[1]], years()[[2]], input$teams,
                          hot()))
    sql_get_lines(years()[[1]], years()[[2]], input$teams,
              hot(), max_rank()) %>%
      lines_remove_branch_descenders(max_rank(), hot())
  })

  selected_streak_id <- reactive({
    message("selected_streak_id")
    if (is.null(selected_line_id())) {
      NULL
    } else {
      lines_to_streaks() %>%
        dplyr::filter(LineId == selected_line_id()) %>%
        dplyr::pull(StreakId)
    }
  })

  selected_streak <- reactive({
    message("selected_streak")
    if (is.null(selected_streak_id())) {
      NULL
    } else {
      sql_get_streak(selected_streak_id(), hot())
    }
  })

  selected_streak_standings <- reactive({
    streak_get_standings(selected_streak(), SOMData::franchises)
  })

  highlight_data <- reactive({
    lines_highlight(
      filtered_lines(), concordances(),
      lines_to_streaks(), selected_line_id()
    )
  })

  # UI functions ----
  update_divisions_selection <- function() {
    updateSelectInput(session, "divisions",
                      choices = divisions_choices(),
                      selected = unlist(divisions_choices())
    )

    if (no_divisions_choices()) {
      shinyjs::disable("divisions")
      shinyjs::disable("divisions_all")
    } else {
      if (input$divisions_all) {
        shinyjs::enable("divisions_all")
        shinyjs::disable("divisions")
      } else {
        shinyjs::enable("divisions")
        shinyjs::enable("divisions_all")
      }
    }
    if (input$divisions_all) {
      updateSelectInput(session, "divisions",
                        choices = divisions_choices(),
                        selected = unlist(divisions_choices())
      )
    }
  }

  # Observers ----
  observeEvent(input$teams_all, {
    if (input$teams_all) {
      updateSelectInput(session, "teams",
        choices = teams_choices(),
        selected = unlist(teams_choices())
      )
      shinyjs::disable("teams")
    } else {
      shinyjs::enable("teams")
    }
  })

  observeEvent(input$divisions_all, {
    update_divisions_selection()
  })

  observeEvent(input$leagues, {
    selected_leagues(
      if (input$leagues == "BOTH") c("AL", "NL") else input$leagues
    )
    update_divisions_selection()
  })

  observeEvent(years(), {
    update_divisions_selection()
  })

  observeEvent(input$divisions, {
    selected_league_divisions(
      input$divisions %>% division_choice_values_as_league_and_division_list()
    )
    updateSelectInput(session, "teams",
      choices = teams_choices(),
      selected = teams_choices()
    )
  })

  observeEvent(plotly::event_data("plotly_click", source = "lines_plot"), {
    click_data <- plotly::event_data("plotly_click", source = "lines_plot")
    if (is.null(click_data)) {
      selected_line_id(NULL)
    } else {
      selected_line_id(click_data %>% dplyr::pull("key"))
    }
  })

  observeEvent(hot(), ignoreInit = TRUE, {
    selected_line_id(NULL)
  })

  observe({
    if (is.null(selected_streak_id())) {
      shinyjs::hide("standings_row")
      shinyjs::hide("graph_log_row")
    } else {
      shinyjs::show("standings_row")
      shinyjs::show("graph_log_row")
    }
  })


  # Renderers ----
  output$streaks <- plotly::renderPlotly({
    message("Rendering plotly...")
    highlight_data() %>% plot_lines(
      max_rank(),
      input$streak_type == "COLD"
    )
  })

  # TODO MOVE
  selected_streak_summary_data <- reactive({
    streak_summary_data(selected_streak(), hot())
  })

  output$streak_summary <- DT::renderDT({
    if (is.null(selected_streak_id())) {
      return(NULL)
    }
    DT::datatable(
      selected_streak_summary_data()$data,
      caption = NULL,
      rownames = FALSE,
      options = list(
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      ),
      extensions = "Select", selection="none"
    )
  })

  output$streak_summary_caption <- renderText({
    if (is.null(selected_streak_id())) {
      return(NULL)
    }
    selected_streak_summary_data()$caption
  })
  output$standings_before_caption <- renderText({
    req(selected_streak_id())
    paste0("Standings before ", selected_streak_summary_data()$data$Start)
  })
  output$standings_after_caption <- renderText({
    req(selected_streak_id())
    paste0("Standings after ", selected_streak_summary_data()$data$End)
  })
  output$standings_final_caption <- renderText({
    req(selected_streak_id())
    "Final standings"
  })
  output$standings_graph_caption <- renderText({
    req(selected_streak_id())
    "Standings graph"
  })
  output$game_log_caption <- renderText({
    req(selected_streak_id())
    "Game log"
  })

  output$standings_before <- DT::renderDT({
    message(paste("Rendering 'before' standings...", selected_streak_id()))
    if (is.null(selected_streak_id())) {
      return(NULL)
    }

    standings_DT(
      selected_streak_standings()$streak_info,
      selected_streak_standings()$standings_before
    )
  })

  output$standings_after <- DT::renderDT({
    message(paste("Rendering 'after' standings...", selected_streak_id()))
    if (is.null(selected_streak_id())) {
      return(NULL)
    }

    standings_DT(
      selected_streak_standings()$streak_info,
      selected_streak_standings()$standings_after
    )
  })

  output$standings_final <- DT::renderDT({
    message(paste("Rendering final standings...", selected_streak_id()))
    if (is.null(selected_streak_id())) {
      return(NULL)
    }

    standings_DT(
      selected_streak_standings()$streak_info,
      selected_streak_standings()$standings_final
    )
  })

  output$standings_graph <- renderPlot({
    req(selected_streak_id())
    message(paste("Rendering standings graph...", selected_streak_id()))

    streak <- selected_streak()

    division_teams <- franchises_get_division_by_team_year(
      SOMData::franchises, streak$Team, streak$Year)
    standings <- SOMData::standings %>%
      dplyr::filter(Year==streak$Year) %>%
      dplyr::right_join(division_teams$division)
    plot_standings_graph(standings, streak$Team, streak$StartDate,
                         streak$EndDate)
  })

  output$game_log <- DT::renderDT({
    message(paste("Rendering game log table...", selected_streak_id()))
    if (is.null(selected_streak_id())) {
      return(NULL)
    }

    game_log <- streak_game_log_data(
      selected_streak(),
      hot()
    )
    DT::datatable(
      game_log$data,
      caption = NULL,
      rownames = FALSE,
      options(
        ordering = FALSE, searching = FALSE, pageLength = 15,
        paging = nrow(game_log$data) > 15,
        pagingType = "simple",
        lengthChange = FALSE
      ),
      extensions = "Select", selection="none"
    )
  })
}

shinyApp(ui, server)
