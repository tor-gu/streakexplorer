#' Streak explorer app
#'
#' @param my_pool RMySQL DB pool
#' @param ... Additional arguments passed on to shiny::shinyApp
#'
#' @return A shiny::shinyApp
#' @export
#' @examples
#' \dontrun{
#' my_pool <- pool::dbPool(
#'   RMySQL::MySQL(),
#'   host = Sys.getenv("streak_explorer_db_host"),
#'   port = as.integer(Sys.getenv("streak_explorer_db_port")),
#'   user = Sys.getenv("streak_explorer_db_user"),
#'   password = Sys.getenv("streak_explorer_db_password"),
#'   dbname = Sys.getenv("streak_explorer_db_name")
#' )
#' streakexplorerApp(my_pool)
#' }
streakexplorerApp <- function(my_pool, ...) {
  se_pool <<- my_pool
  initial_year_range <- c(1948, 1960)
  theme <- bslib::bs_theme(
    bootswatch = "lumen",
    heading_font = "1.2",
    font_scale = 0.8
  )

  # UI ----
  ui <- shiny::fluidPage(
    ## Styling and header ----
    shinyjs::useShinyjs(),
    shiny::tags$style("#game_log td, th {padding: 0; text-align: right}"),
    shiny::tags$style("#streak_summary td, th {padding: 0; text-align: right}"),
    shiny::tags$style("#standings_before td, th {padding: 0; text-align: right}"),
    shiny::tags$style("#standings_after td, th {padding: 0; text-align: right}"),
    shiny::tags$style("#standings_final td, th {padding: 0; text-align: right}"),
    theme = theme,
    #theme=bslib::bs_theme(),

    ## Title ----
    shiny::titlePanel("Baseball Streak Explorer"),

    ## Sidebar layout ----
    shiny::sidebarLayout(
      ### Sidebar ----
      shiny::sidebarPanel(
        width = 5,

        #### Years slider ----
        shiny::fluidRow(shiny::column(
          12,
          shiny::sliderInput(
            "years",
            "Years",
            min = 1948,
            max = 2021,
            step = 1,
            value = initial_year_range,
            sep = ""
          )
        )),

        #### League selection ----
        shiny::fluidRow(shiny::column(
          9,
          shiny::selectInput(
            "leagues",
            "League",
            choices = c(
              "All Leagues" = "BOTH",
              "AL" = "AL",
              "NL" = "NL"
            )
          )
        ),
        # This empty column is a placeholder, where the 'all' box would go
        shiny::column(3, )),

        #### Division selection ----
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::selectInput(
              "divisions",
              "Divisions",
              choices = list(),
              multiple = TRUE
            ),
          ),
          shiny::column(3,
                        shiny::checkboxInput("divisions_all", "All",
                                             value = TRUE))
        ),

        #### Team selection ----
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::selectInput("teams", "Teams", choices = list(),
                               multiple = TRUE),
          ),
          shiny::column(3,
                        shiny::checkboxInput("teams_all", "All", value = TRUE))
        ),
        shiny::radioButtons(
          "streak_type",
          "Streak Type",
          choices = c("HOT", "COLD"),
          selected = "HOT"
        )
      ),

      ### Main Panel ----
      shiny::mainPanel(
        width = 7,

        #### Streaks graph ----
        shiny::fluidRow(shiny::column(
          12, plotly::plotlyOutput(outputId = "streaks")
        )),

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
    )
  )

  # Server ----
  server <- function(input, output, session) {
    #bslib::bs_themer()
    intensity_level_range <- sql_get_intensity_level_range()
    lzy_franchises <- sql_load_franchises()
    lzy_standings <- sql_load_standings()
    lzy_game_logs <- sql_load_game_logs()
    franchises <- lzy_franchises %>% dplyr::collect()

    ## Reactives and reactive values ----
    hot <- reactive({
      input$streak_type == "HOT"
    })

    lzy_lines_to_streaks <- reactive({
      if (hot()) {
        sql_load_hot_streaks_lines_to_streaks()
      } else {
        sql_load_cold_streaks_lines_to_streaks()
      }
    })

    lzy_streaks <- reactive({
      if (hot()) {
        sql_load_hot_streaks()
      } else {
        sql_load_cold_streaks()
      }
    })

    concordances <- reactive({
      if (hot()) {
        sql_load_hot_streaks_concordances()
      } else {
        sql_load_cold_streaks_concordances()
      }
    })

    years <- reactive({
      input$years
    }) %>% debounce(333)
    max_rank <- reactive({
      req(input$teams)
      team_ids <- franchises_franchise_ids_to_team_ids(franchises,
                                                       input$teams, years())
      streaks_get_max_rank(years()[[1]], years()[[2]], team_ids, hot())
    })

    selected_line_id <- reactiveVal(NULL)
    selected_years <- reactive({
      years()[[1]]:years()[[2]]
    })
    selected_leagues <- reactiveVal(c("AL", "NL"))
    selected_league_divisions <- reactiveVal(
      list("AL_None", "NL_None") %>%
        ui_division_choice_values_as_league_and_division_list()
    )
    selected_streak_summary_data <- reactive({
      req(selected_streak())
      streak_summary_data(selected_streak(), lzy_franchises)
    })

    divisions_choices <- reactive({
      ui_build_divisions_choices(franchises,
                                 selected_years(),
                                 selected_leagues())
    })

    teams_choices <- reactive({
      ui_build_teams_choices(franchises,
                             selected_years(),
                             selected_league_divisions())
    })

    no_divisions_choices <- reactive({
      all(unname(unlist(divisions_choices())) %in% c("AL_None", "NL_None"))
    })

    lines <- reactive({
      req(input$teams, max_rank())
      lines_build_lines(years(), input$teams, lzy_franchises,
                        max_rank(), hot())
    })

    selected_streak <- reactive({
      lines_get_selected_streak(
        lzy_lines_to_streaks(),
        lzy_streaks(),
        lzy_game_logs,
        selected_line_id())
    })

    selected_streak_standings <- reactive({
      streak_get_standings(lzy_standings, lzy_game_logs, selected_streak(),
                           lzy_franchises)
    })

    highlight_data <- reactive({
      lines_highlight(lines(),
                      concordances(),
                      lzy_lines_to_streaks(),
                      selected_line_id())
    })

    ## UI functions ----
    update_divisions_selection <- function() {
      if (no_divisions_choices()) {
        updateCheckboxInput(session, "divisions_all", value = TRUE)
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
      selected <-
        ui_get_updated_division_selection(divisions_choices(),
                                          input$divisions,
                                          input$divisions_all)
      updateSelectInput(
        session,
        "divisions",
        choices = divisions_choices(),
        selected = selected
      )
    }

    update_teams_selection <- function() {
      if (input$teams_all) {
        shinyjs::disable("teams")
      } else {
        shinyjs::enable("teams")
      }
      selected <- ui_get_updated_teams_selection(teams_choices(),
                                                 input$teams,
                                                 input$teams_all)
      updateSelectInput(session,
                        "teams",
                        choices = teams_choices(),
                        selected = selected)
    }

    ## Observers ----
    observeEvent(input$teams_all, {
      update_teams_selection()
    })

    observeEvent(input$divisions_all, {
      update_divisions_selection()
    })

    observeEvent(input$leagues, {
      selected_leagues(if (input$leagues == "BOTH")
        c("AL", "NL")
        else
          input$leagues)
      update_divisions_selection()
    })

    observeEvent(years(), {
      update_divisions_selection()
    })

    observeEvent(input$divisions, {
      selected_league_divisions(
        input$divisions %>%
          ui_division_choice_values_as_league_and_division_list())
      update_teams_selection()
    })

    observeEvent(
      plotly::event_data("plotly_click", source = "lines_plot"), {
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

      game_log <- streak_game_log_data(selected_streak())
      DT_game_log_update(game_log_proxy, game_log$data)
      DT_streak_summary_update(
        streak_summary_proxy,
        selected_streak_summary_data()$data
      )
      output$streak_summary_caption <- renderText(
        selected_streak_summary_data()$caption)
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


    ## Renderers ----

    # This is the main graph
    output$streaks <- plotly::renderPlotly({
      plot_lines(
        highlight_data(),
        intensity_level_range,
        max_rank(),
        input$streak_type == "COLD"
      )
    })

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
      build_standings_graph(lzy_franchises, lzy_standings, selected_streak())
    })

  }

  shiny::shinyApp(ui, server, ...)
}
