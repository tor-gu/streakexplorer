library(shiny)


initialYearRange <- c(1948,1949)
initialYears <- initialYearRange[[1]]:initialYearRange[[2]]
theme <- bslib::bs_theme(bootswatch="slate",
                         heading_font = "1.2",
                         font_scale = 0.8)
ui <- fluidPage(
  tags$style("#game_log td, th {padding: 0; text-align: right}"),
  tags$style("#streak_summary td, th {padding: 0; text-align: right}"),
  theme=theme,
  #theme=bslib::bs_theme(),
  titlePanel("Streak Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Years", min=1948, max=2021, step=1,
                  value=initialYearRange, sep=""),
      selectInput("leagues", "League",
                  choices = c("All Leagues" = "BOTH", "AL"="AL", "NL"="NL")),
      selectInput("divisions", "Divisions", choices=list(), multiple=TRUE),
      selectInput("teams", "Teams", choices=list(), multiple=TRUE),
      radioButtons("streak_type", "Streak Type", choices=c("HOT", "COLD"),
                   selected="HOT")
    ),
    mainPanel(
      plotly::plotlyOutput(outputId = "streaks"),
      shinycssloaders::withSpinner(DT::DTOutput("streak_summary")),
      tableOutput("standings"),
      shinycssloaders::withSpinner(DT::DTOutput("game_log"))
    )
  )
)

server <- function(input, output, session) {
  #bslib::bs_themer()

  hot <- reactive({input$streak_type == "HOT"})

    message("loading base lines")
    if (hot()) {
      SOMData::hot_streaks_lines
    } else {
      SOMData::cold_streaks_lines
    }
  })

  streaks <- reactive({
    message("loading streaks")
    if (hot()) {
      SOMData::hot_streaks
    } else {
      SOMData::cold_streaks
    }
  })

  lines_to_streaks <- reactive({
    message("loading lines_to_streaks")
    if (hot()) {
      SOMData::hot_streaks_lines_to_streaks
    } else {
      SOMData::cold_streaks_lines_to_streaks
    }
  })

  selected_years <- reactive({years()[[1]]:years()[[2]]})
  selected_leagues <- reactiveVal(c("AL", "NL"))
  selected_league_divisions <- reactiveVal(list())

  division_choices <- reactive({
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

  observeEvent(input$leagues, {
    selected_leagues(
      if(input$leagues == "BOTH") c("AL", "NL") else input$leagues)
    updateSelectInput(session, "divisions", choices=division_choices(),
                      selected=unlist(division_choices()))
  })

  observeEvent(years(), {
    updateSelectInput(session, "divisions", choices=division_choices(),
                      selected=unlist(division_choices()))
  })

  observeEvent(input$divisions, {
    selected_league_divisions(
      input$divisions %>% division_choice_values_as_league_and_division_list())
    updateSelectInput(session, "teams", choices=teams_choices(),
                      selected=teams_choices())

  })

  max_rank <- reactiveVal(10)
  years <- reactive({input$years}) %>% debounce(333)

  filtered_lines <- reactive({
    req(years(), input$teams)
    message("filtering lines")
    filtered <- base_lines() %>%
      dplyr::filter(Year >= years()[[1]] & Year <= years()[[2]]) %>%
      dplyr::filter(Team %in% input$teams)
    max_rank(filtered %>%
               dplyr::group_by(IntensityLevel) %>%
               dplyr::slice_min(Rank, n=10) %>%
               dplyr::ungroup() %>%
               dplyr::summarise(ms=max(Rank)) %>% dplyr::pull(ms)
    )
    #filtered %>%
    #  dplyr::filter(Rank <= max_rank()) %>%
    #  add_descenders(filtered)
    filtered %>%
      dplyr::filter(Rank <= max_rank()) %>%
      lines_remove_nubs()
  })

  lines <- reactive({
    message("building lines")
    #filtered_streaks() %>%
    #  lines_split_all(concordances(), levels(), hot()) %>%
    #  lines_bind() %>%
    #  lines_update_text(SOMData::game_logs)
    filtered_lines()
  })

  concordances <- reactive({
    message("loading concordances")
    if (hot()) {
      SOMData::hot_streaks_concordances
    } else {
      SOMData::cold_streaks_concordances
    }
  })

  selected_line_id <- reactiveVal(NULL)
  observeEvent(plotly::event_data("plotly_click", source="lines_plot"), {
    click_data <- plotly::event_data("plotly_click", source="lines_plot")
    if (is.null(click_data)) {
      selected_line_id(NULL)
    } else {
      selected_line_id(click_data %>% dplyr::pull("key"))
    }
  })
  observeEvent(hot(), ignoreInit = TRUE, {
    selected_line_id(NULL)
  })
  #selected_line_id <- reactive({
  #  click_data <- plotly::event_data("plotly_click", source="lines_plot")
  #  if (is.null(click_data)) {
  #    NULL
  #  } else {
  #    click_data %>% dplyr::pull("key")
  #  }
  #})

  selected_streak_id <- reactive({
    if (is.null(selected_line_id())) {
      NULL
    } else {
      lines_to_streaks() %>%
        dplyr::filter(LineId == selected_line_id()) %>%
        dplyr::pull(StreakId)
    }
  })

  highlight_data <- reactive({
    message("About to do highlighting")
    #print(selected_streak_id())
    #id <- NULL
    #if (!is.null(selected_streak_id())) {
    #  print(selected_streak_id())
    #  id <- lines() %>%
    #    dplyr::filter(StreakId==selected_streak_id()) %>%
    #    head(1) %>% dplyr::pull(Id)
    #}
    print(selected_line_id())
    lines_highlight(filtered_lines(), concordances(),
                    lines_to_streaks(), selected_line_id())
  })

  output$streaks <- plotly::renderPlotly({
    message("Rendering plotly...")
    highlight_data() %>% lines_plot(max_rank(),
                                    input$streak_type == "COLD")
  })

  output$streak_summary <- DT::renderDT({
    message(paste("Rendering summary table...", selected_streak_id()))
    if (is.null(selected_streak_id())) return(NULL)

    summary <- streak_summary_data(selected_streak_id(),
                                   isolate(streaks()),
                                   SOMData::game_logs)
    DT::datatable(
      summary$data,
      caption=summary$caption,
      rownames = FALSE,
      options=list(ordering=FALSE,
                   paging=FALSE,
                   searching=FALSE,
                   info=FALSE
      )
    )
  })

  output$game_log <- DT::renderDT({
    message(paste("Rendering game log table...", selected_streak_id()))
    #click_data <- plotly::event_data("plotly_click", source="lines_plot")
    #key <- click_data %>% dplyr::pull("key")
    if (is.null(selected_streak_id())) return(NULL)

    game_log <- streak_game_log_data(selected_streak_id(),
                                     isolate(streaks()),
                                     SOMData::game_logs)
    DT::datatable(
      game_log$data,
      caption=game_log$caption,
      rownames = FALSE,
      options(ordering=FALSE, searching=FALSE, pageLength=25,
              lengthChange=FALSE)
    )
  })

}

shinyApp(ui, server)
