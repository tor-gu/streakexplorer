sql_get_streak_game_log <- function(streak) {
  query_template <- (
    "
    SELECT * FROM game_logs WHERE
    Year = {streak$Year} AND
    Team = {streak$Team} AND
    GameIndex >= {streak$LoIndex} AND
    GameIndex <= {streak$HiIndex}
  "
  )
  query <- glue::glue_sql(
    query_template,
    streak_table = streak_table,
    streak_id = streak_id,
    .con = se_pool
  )
  DBI::dbGetQuery(se_pool, query) %>% tibble::as_tibble() %>%
    dplyr::mutate(
      Date=lubridate::as_date(Date),
      CompletedOn=lubridate::as_date(CompletedOn),
      CompletionOf=lubridate::as_date(CompletionOf))
}

sql_get_division_season_games <- function(year, teams) {
  query_template <- ("
    SELECT *
    FROM game_logs WHERE
      Year = {year} AND
      Team IN ({teams*})
  ")
  query <- glue::glue_sql(
    query_template,
    year = year,
    teams = teams,
    .con = se_pool
  )
  DBI::dbGetQuery(se_pool, query) %>%
    dplyr::mutate(
      Date=lubridate::as_date(Date),
      CompletedOn=lubridate::as_date(CompletedOn),
      CompletionOf=lubridate::as_date(CompletionOf))
}

sql_load_franchises <- function() {
  # Just load the whole thing into memory
  #dplyr::tbl(se_pool, "franchises") %>% tibble::as.tibble()
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
