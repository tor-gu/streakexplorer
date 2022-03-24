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
      fluidRow(id="summary_row",
        column(12, h4("Streak summary"),
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

  intensity_level_range <- sql_get_intensity_level_range()

  # Reactives and reactive values ----
  hot <- reactive({
    input$streak_type == "HOT"
  })

  lines_to_streaks <- reactive({
    if (hot()) {
      SOMData::hot_streaks_lines_to_streaks
    } else {
      SOMData::cold_streaks_lines_to_streaks
    }
  })

  concordances <- reactive({
    if (hot()) {
      SOMData::hot_streaks_concordances
    } else {
      SOMData::cold_streaks_concordances
    }
  })

  years <- reactive({ input$years }) %>% debounce(333)
  max_rank <- reactive({
    sql_get_max_rank(
      years()[[1]], years()[[2]], input$teams, hot())
  })

  selected_line_id <- reactiveVal(NULL)
  selected_years <- reactive({ years()[[1]]:years()[[2]] })
  selected_leagues <- reactiveVal(c("AL", "NL"))
  selected_league_divisions <- reactiveVal(
    list("AL_None", "NL_None") %>%
      division_choice_values_as_league_and_division_list()
  )
  selected_streak_summary_data <- reactive({
    streak_summary_data(selected_streak(), hot())
  })

  divisions_choices <- reactive({
    build_divisions_choices(
      SOMData::franchises,
      selected_years(),
      selected_leagues()
    )
  })

  teams_choices <- reactive({
    build_teams_choices(
      SOMData::franchises,
      selected_years(),
      selected_league_divisions()
    )
  })

  no_divisions_choices <- reactive({
    all(
      unname(unlist(divisions_choices())) %in% c("AL_None", "NL_None")
    )
  })

  lines <- reactive({
    req(input$teams, max_rank())
    lines_build_lines(years(), input$teams, max_rank(), hot())
  })

  selected_streak_id <- reactive({
    lines_get_selected_streak_id(lines_to_streaks(),
                                 selected_line_id())
  })

  selected_streak <- reactive({
    streaks_get_selected_streak(selected_streak_id(), hot())
  })

  selected_streak_standings <- reactive({
    streak_get_standings(selected_streak(), SOMData::franchises)
  })

  highlight_data <- reactive({
    lines_highlight(
      lines(), concordances(),
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
      shinyjs::hide("summary_row")
      shinyjs::hide("standings_row")
      shinyjs::hide("graph_log_row")
    } else {
      shinyjs::show("summary_row")
      shinyjs::show("standings_row")
      shinyjs::show("graph_log_row")
    }
  })

  observeEvent(selected_streak_id(), {
    standings_DT_update(
      standings_before_proxy,
      selected_streak_standings()$streak_info,
      selected_streak_standings()$standings_before
    )
    standings_DT_update(
      standings_after_proxy,
      selected_streak_standings()$streak_info,
      selected_streak_standings()$standings_after
    )
    standings_DT_update(
      standings_final_proxy,
      selected_streak_standings()$streak_info,
      selected_streak_standings()$standings_final
    )

    game_log <- streak_game_log_data(
      selected_streak(),
      hot()
    )
    DT::replaceData(game_log_proxy,
                    game_log$data,
                    resetPaging = FALSE,
                    rownames = FALSE
    )
    DT::replaceData(streak_summary_proxy,
                    selected_streak_summary_data()$data,
                    resetPaging = FALSE,
                    rownames = FALSE
    )
  })

  # Proxies ----
  streak_summary_proxy <- DT::dataTableProxy("streak_summary",
                                             session=session)
  standings_before_proxy <- DT::dataTableProxy("standings_before",
                                               session=session)
  standings_after_proxy <- DT::dataTableProxy("standings_after",
                                              session=session)
  standings_final_proxy <- DT::dataTableProxy("standings_final",
                                              session=session)
  game_log_proxy <- DT::dataTableProxy("game_log", session=session)


  # Renderers ----

  # This is the main graph
  output$streaks <- plotly::renderPlotly({
    plot_lines(highlight_data(),
      intensity_level_range,
      max_rank(),
      input$streak_type == "COLD"
    )
  })

  # Data tables.
  # For each of these, we do an initial render with a dummy table, and then
  # handle the updates through proxies
  output$streak_summary <- DT::renderDT({
    streak_summary_DT_init()
  })
  output$standings_before <- DT::renderDT({
    standings_DT_init()
  })
  output$standings_after <- DT::renderDT({
    standings_DT_init()
  })
  output$standings_final <- DT::renderDT({
    standings_DT_init()
  })
  output$game_log <- DT::renderDT({
    game_log_DT_init()
  })

  # Standings graph
  output$standings_graph <- renderPlot({
    build_standings_graph(
      SOMData::franchises, SOMData::standings, selected_streak()
    )
  })

}

shinyApp(ui, server)
