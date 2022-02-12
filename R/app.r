library(shiny)
library(magrittr)
source("streak_relations.r")
source("plot.r")
source("util.r")
source("segments.r")
source("ui_choices.r")

franchises_by_season <- function(franchises, year) {
  franchises %>% filter(FirstSeason <= year & (FinalSeason >= year | is.na(FinalSeason)))
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

ui <- fluidPage(
  titlePanel("Streak Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("years", "Years", min=1950, max=2020, step=1, value=initialYearRange, sep=""),
      selectInput("leagues", "League", choices = c("All Leagues" = "BOTH", "AL"="AL", "NL"="NL")),
      selectInput("divisions", "Divisions", choices=list(), multiple=TRUE),
      #uiOutput("divisions_select"),
      selectInput("teams", "Teams", choices=list(), multiple=TRUE),
      radioButtons("streak_type", "Streak Type", choices=c("HOT", "COLD"), selected="HOT")
    ),
    mainPanel(
      plotOutput("streaks", click="streak_click"),
      tableOutput("streak_info")
    )
  )
)

server <- function(input, output, session) {

  adjusted_hot_streaks <- reactive({
    SOMData::hot_streaks %>%
      som_add_adj_score(prop=.5, top=TRUE)
  })

  adjusted_cold_streaks <- reactive({
    SOMData::cold_streaks %>%
      som_add_adj_score(prop=.5, top=FALSE)
  })

  base_streaks <- reactive({
    if (input$streak_type == "HOT") {
      adjusted_hot_streaks()
    } else {
      adjusted_cold_streaks() %>%
        dplyr::mutate(Score=-Score, AdjScore=-AdjScore)
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
    selected_leagues(if(input$leagues == "BOTH") c("AL", "NL") else input$leagues)
    updateSelectInput(session, "divisions", choices=division_choices(),
                      selected=unlist(division_choices()))
  })

  observeEvent(input$years, {
    updateSelectInput(session, "divisions", choices=division_choices(),
                      selected=unlist(division_choices()))
  })

  observeEvent(input$divisions, {
    selected_league_divisions(input$divisions %>%
                                division_choice_values_as_league_and_division_list())
    updateSelectInput(session, "teams", choices=teams_choices(),
                      selected=teams_choices())

  })

  filtered_streaks <- reactive({
    base_streaks() %>%
      dplyr::filter(Year >= input$years[[1]] & Year <= input$years[[2]]) %>%
      dplyr::filter(Team %in% input$teams) %>%
      dplyr::group_by(Level) %>%
      dplyr::slice_max(Score, n=10) %>% dplyr::ungroup()
  })

  concordances <- reactive({
    if (input$streak_type == "HOT") {
      SOMData::hot_streaks_concordances
    } else {
      SOMData::cold_streaks_concordances
    }
  })

  selected_id <- reactiveVal(NULL)
  near_rows <- reactiveVal(NULL)

  observeEvent(input$streak_click, {
    near_points <- nearPoints(filtered_streaks(), input$streak_click, addDist = TRUE)
    if (nrow(near_points) > 0 ) {
      Level <- near_points %>% slice_max(order_by = dist_, n=1) %>% pull(Level)
      print(near_points)
      print(near_points %>% filter(Level==.env$Level))
      near_rows(near_points %>% filter(Level==.env$Level))
    } else {
      near_rows(NULL)
    }

    if (!is.null(near_rows()) > 0) {
      selected_id(near_rows() %>% slice_max(order_by="_dist", n=1) %>% pull(Id))
    } else {
      selected_id(NULL)
    }
  })

  plot_data <- reactive({
    plot_make_data(filtered_streaks(), concordances(), selected_id())
  })

  output$streaks <- renderPlot({
    plot_data() %>% plot_make_plot()
  })

  output$streak_info <- renderTable({
    near_rows()
  })

}

shinyApp(ui, server)
