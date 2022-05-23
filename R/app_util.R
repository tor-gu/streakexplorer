#' app_get_intensity_range
#'
#' Use the streaks table to get the the minimum and maximum intensity
#' levels in the DB.  The year is specified to make the query quicker.
#'
#' @param lzy_streaks  Lazy streaks table
#' @param year  Year to query.
#'
#' @return Vector with min and max intensity levels, e.g. `c(1,101)`
app_get_intensity_range <- function(lzy_streaks, year) {
  lzy_streaks %>%
    dplyr::filter(Year==year) %>%
    dplyr::distinct(IntensityLevel) %>%
    dplyr::collect() %>%
    dplyr::summarize(min_level=min(IntensityLevel),
                     max_level=max(IntensityLevel)) %>%
    purrr::transpose() %>%
    unlist()
}

app_get_highlight_colors <- function() {
  list(
    background = "#00000008",
    base =   "#231427",
    low =    "#6C0F49",
    medium = "#A5004A",
    high =   "#FF0000"
  )
}

app_get_theme <- function() {
  bslib::bs_theme(
    bootswatch = "cerulean",
    heading_font = "1.2",
    font_scale = 0.8
  )
}
