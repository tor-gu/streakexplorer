#' Streak explorer app
#'
#' @param my_pool RMySQL DB pool
#' @param initial_year_min Minimum year in the initial year range
#' @param initial_year_max Maximum year in the initial year range
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
#' streakexplorerApp(my_pool, 1948, 1960)
#' }
streakexplorerApp <- function(my_pool, initial_year_min, initial_year_max, ...) {
  franchises <- dplyr::tbl(my_pool, "franchises") %>% dplyr::collect()
  intensity_level_range <- dplyr::tbl(my_pool, "hot_streaks") %>%
    streaks_get_intensity_range(initial_year_min)
  initial_year_range <- c(initial_year_min, initial_year_max)

  # UI ----
  ui <-
    shiny::navbarPage(
      "Baseball Streak Explorer",
      collapsible = FALSE,
      inverse = TRUE,
      shiny::tabPanel(
        "Explorer",
        shiny::fluidPage(
          ## Styling and header ----
          shinyjs::useShinyjs(),
          shiny::tags$style("#game_log td, th {padding: 0; text-align: right}"),
          shiny::tags$style("#streak_summary td, th {padding: 0; text-align: right}"),
          shiny::tags$style("#standings_before td, th {padding: 0; text-align: right}"),
          shiny::tags$style("#standings_after td, th {padding: 0; text-align: right}"),
          shiny::tags$style("#standings_final td, th {padding: 0; text-align: right}"),

          ## Sidebar layout ----
          shiny::sidebarLayout(
            ### Sidebar ----
            shiny::sidebarPanel(
              width = 5,
              filterUI("filter", initial_year_range)
            ),

            ### Main Panel ----
            shiny::mainPanel(
              width = 7,

              #### Streaks graph ----
              plotUI("plot"),

              #### Selected streak summary ----
              summaryUI("summary")
            )
          )
        )
      ),
      shiny::tabPanel("About", shiny::includeMarkdown("")),
      theme = theme
    )


  # Server ----
  server <- function(input, output, session) {
    filter <- filterServer("filter", franchises)
    selected_streak <- plotServer("plot", my_pool, franchises,
                                  intensity_level_range, filter)
    summaryServer("summary", my_pool, franchises, selected_streak)
  }

  shiny::shinyApp(ui, server, ...)
}
