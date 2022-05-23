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

# Utility functions copied from torgutil ----
#' Check that a column has a unique value
#'
#' @param tbl   Data table
#' @param column_name  Column name (unquoted)
#'
#' @return  TRUE or FALSE
tbl_is_column_value_unique <- function(tbl, column_name) {
  tbl %>% dplyr::pull({{column_name}}) %>% unique() %>% length()== 1
}

#' Given a table and two columns, build a named list.
#'
#' Build a named list from the \code{value_col} and \code{name_col}. Note
#' that there is no uniqueness test on the names column.
#'
#' @param tbl         Table
#' @param value_col   Column with list values
#' @param name_col    Column with list names
#'
#' @return A named list
tbl_as_named_list <- function(tbl, value_col, name_col) {
  result <- tbl %>% dplyr::pull({{value_col}}) %>% as.list()
  names(result) <- tbl %>% dplyr::pull({{name_col}}) %>% as.list()
  result
}
