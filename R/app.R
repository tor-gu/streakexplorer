library(shiny)
library(magrittr)
library(dplyr)
library(plotly)
library(DT)
source("lines.R")
#source("streak_relations.r")
#source("plot.r")
source("util.R")
#source("segments.r")
source("ui_choices.R")
source("streaks.R")

franchises_by_season <- function(franchises, year) {
  franchises %>% filter(FirstSeason <= year &
                          (FinalSeason >= year | is.na(FinalSeason)))
}

franchises_by_seasons <- function(franchises, years) {
  purrr::map(years, function(year) franchises_by_season(franchises, year)) %>%
    data.table::rbindlist() %>% as_tibble() %>% unique()
}


initialYearRange <- c(1950,1960)
initialYears <- initialYearRange[[1]]:initialYearRange[[2]]
initialTeams <- SOMData::franchises %>%
  franchises_by_seasons(initialYearRange) %>%
  pull(TeamID) %>% unique()
hs_levels <- SOMData::hot_streaks %>% pull(Level) %>% unique() %>% sort()
cs_levels <- SOMData::cold_streaks %>% pull(Level) %>% unique() %>% sort()

ui <- fluidPage(
  titlePanel("Streak Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Years", min=1950, max=2020, step=1,
                  value=initialYearRange, sep=""),
      selectInput("leagues", "League",
                  choices = c("All Leagues" = "BOTH", "AL"="AL", "NL"="NL")),
      selectInput("divisions", "Divisions", choices=list(), multiple=TRUE),
      selectInput("teams", "Teams", choices=list(), multiple=TRUE),
      radioButtons("streak_type", "Streak Type", choices=c("HOT", "COLD"),
                   selected="HOT")
    ),
    mainPanel(
      plotlyOutput(outputId = "streaks"),
      tableOutput("streak_summary"),
      tableOutput("standings"),
      tableOutput("game_log"),
    )
  )
)

server <- function(input, output, session) {

  adjusted_hot_streaks <- reactive({
    message("adding ranks to hot streaks")
    SOMData::hot_streaks %>%
      som_add_rank(top=TRUE) %>%
      som_add_adj_level(hs_levels)
  })

  adjusted_cold_streaks <- reactive({
    message("adding ranks to cold streaks")
    SOMData::cold_streaks %>%
      som_add_rank(top=FALSE) %>%
      som_add_adj_level(cs_levels)
  })


  levels <- reactive({
    if (input$streak_type == "HOT") {
      hs_levels
    } else {
      cs_levels
    }
  })

  base_streaks <- reactive({
    message("loading base_streaks")
    if (input$streak_type == "HOT") {
      adjusted_hot_streaks()
    } else {
      adjusted_cold_streaks()
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

  filtered_streaks <- reactive({
    message("filtering streaks")
    filtered <- base_streaks() %>%
      dplyr::filter(Year >= input$years[[1]] & Year <= input$years[[2]]) %>%
      dplyr::filter(Team %in% input$teams)
    max_rank(filtered %>%
      dplyr::group_by(Level) %>%
      dplyr::slice_min(Rank, n=10) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(ms=max(Rank)) %>% pull(ms)
    )
    filtered %>%
      filter(Rank <= max_rank()) %>%
      add_descenders(filtered)
  })

  lines <- reactive({
    message("building lines")
    filtered_streaks() %>% lines_split_all(concordances(), levels()) %>%
      lines_bind() %>%
      lines_update_text(SOMData::game_logs)
  })

  concordances <- reactive({
    message("loading concordances")
    if (input$streak_type == "HOT") {
      SOMData::hot_streaks_concordances
    } else {
      SOMData::cold_streaks_concordances
    }
  })

  selected_id <- reactiveVal(NULL)
  near_rows <- reactiveVal(NULL)
  selected_streak_id <- reactive({
    click_data <- event_data("plotly_click", source="lines_plot")
    if (is.null(click_data)) {
      NULL
    } else {
      click_data %>% pull("key")
    }
  })

  highlight_data <- reactive({
    message("About to do highlighting")
    print(selected_streak_id())
    id <- NULL
    if (!is.null(selected_streak_id())) {
      print(selected_streak_id())
      id <- lines() %>%
        filter(StreakId==selected_streak_id()) %>%
        head(1) %>% pull(Id)
    }
    print(id)
    lines_highlight(lines(), filtered_streaks(), concordances(),
                    id)
  })

  output$streaks <- renderPlotly({
    message("Rendering plotly...")
    highlight_data() %>% lines_plot(max_rank())
  })

  output$streak_summary <- renderTable({
    message("Rendering table...")
    click_data <- event_data("plotly_click", source="lines_plot")
    key <- click_data %>% pull("key")
    streak_summary(key, filtered_streaks(), SOMData::game_logs)
  })

  output$game_log <- renderTable({
    message("Rendering table...")
    click_data <- event_data("plotly_click", source="lines_plot")
    key <- click_data %>% pull("key")
    streak_game_log(key, filtered_streaks(), SOMData::game_logs)
  })

}

shinyApp(ui, server)
