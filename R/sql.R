# TODO delete this entire file, I think

sql_load_franchises <- function() {
  dplyr::tbl(se_pool, "franchises")
}

sql_load_standings <- function() {
  dplyr::tbl(se_pool, "standings")
}

sql_load_game_logs <- function() {
  dplyr::tbl(se_pool, "game_logs")
}

sql_load_hot_streaks <- function() {
  dplyr::tbl(se_pool, "hot_streaks")
}

sql_load_cold_streaks <- function() {
  dplyr::tbl(se_pool, "cold_streaks")
}

sql_load_hot_streaks_lines <- function() {
  dplyr::tbl(se_pool, "hot_streaks_lines")
}

sql_load_cold_streaks_lines <- function() {
  dplyr::tbl(se_pool, "cold_streaks_lines")
}

sql_load_hot_streaks_lines_to_streaks <- function() {
  dplyr::tbl(se_pool, "hot_streaks_lines_to_streaks")
}

sql_load_cold_streaks_lines_to_streaks <- function() {
  dplyr::tbl(se_pool, "cold_streaks_lines_to_streaks")
}

sql_load_hot_streaks_concordances <- function() {
  dplyr::tbl(se_pool, "hot_streaks_concordances")
}

sql_load_cold_streaks_concordances <- function() {
  dplyr::tbl(se_pool, "cold_streaks_concordances")
}
