library(shiny)


initialYearRange <- c(1948,1960)
initialYears <- initialYearRange[[1]]:initialYearRange[[2]]
hs_levels <- SOMData::hot_streaks %>% dplyr::pull(Level) %>% unique() %>% sort()
cs_levels <- SOMData::cold_streaks %>% dplyr::pull(Level) %>% unique() %>%
  sort()

ui <- fluidPage(

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
      DT::DTOutput("streak_summary"),
      tableOutput("standings"),
      DT::DTOutput("game_log"),
    )
  )
)

server <- function(input, output, session) {

  hot <- reactive({input$streak_type == "HOT"})

  levels <- reactive({
    if (hot()) {
      hs_levels
    } else {
      cs_levels
    }
  })

  base_lines <- reactive({
    message("loading base lines")
    if (hot()) {
      SOMData::hot_streaks_lines
    } else {
      SOMData::cold_streaks_lines
    }
  })

  lines_to_streaks <- reactive({
    message("loading lines_to_streaks")
    if (hot()) {
      SOMData::hot_streak_lines_to_streaks
    } else {
      SOMData::cold_streak_lines_to_streaks
    }
  })

  selected_years <- reactive({input$years[[1]]:input$years[[2]]})
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

  observeEvent(input$years, {
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

  filtered_lines <- reactive({
    message("filtering lines")
    filtered <- base_lines() %>%
      dplyr::filter(Year >= input$years[[1]] & Year <= input$years[[2]]) %>%
      dplyr::filter(Team %in% input$teams)
    max_rank(filtered %>%
               dplyr::group_by(Level) %>%
               dplyr::slice_min(Rank, n=10) %>%
               dplyr::ungroup() %>%
               dplyr::summarise(ms=max(Rank)) %>% dplyr::pull(ms)
    )
    #filtered %>%
    #  dplyr::filter(Rank <= max_rank()) %>%
    #  add_descenders(filtered)
    filtered %>%
      dplyr::filter(Rank <= max_rank())
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

  selected_id <- reactiveVal(NULL)
  near_rows <- reactiveVal(NULL)
  #selected_streak_id <- reactive({
  #  click_data <- plotly::event_data("plotly_click", source="lines_plot")
  #  if (is.null(click_data)) {
  #    NULL
  #  } else {
  #    click_data %>% dplyr::pull("key")
  #  }
  #})
  selected_line_id <- reactive({
    click_data <- plotly::event_data("plotly_click", source="lines_plot")
    if (is.null(click_data)) {
      NULL
    } else {
      click_data %>% dplyr::pull("key")
    }
  })

  selected_streak_id <- reactive({
    lines_to_streaks() %>%
      dplyr::filter(LineIdx == selected_line_id()) %>%
      dplyr::pull(StreakId)
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
    message("Rendering table...")
    #click_data <- plotly::event_data("plotly_click", source="lines_plot")
    #key <- click_data %>% dplyr::pull("key")
    #streak_id <- lines_to_streaks() %>%
    #  dplyr::filter(LineIdx==key) %>%
    #  dplyr::pull(StreakId)

    summary <- streak_summary_data(selected_streak_id(),
                                   filtered_lines(),
                                   SOMData::game_logs)
    DT::datatable(
      summary$data,
      caption=summary$caption,
      rownames = FALSE,
      options=list(ordering=FALSE,
                   paging=FALSE,
                   searching=FALSE
      )
    )
  })

  output$game_log <- DT::renderDT({
    message("Rendering table...")
    #click_data <- plotly::event_data("plotly_click", source="lines_plot")
    #key <- click_data %>% dplyr::pull("key")
    game_log <- streak_game_log_data(selected_streak_id(),
                                     filtered_lines(),
                                     SOMData::game_logs)
    DT::datatable(
      game_log$data,
      caption=game_log$caption,
      rownames = FALSE,
      options(ordering=FALSE, searching=FALSE)
    )
  })

}

shinyApp(ui, server)
