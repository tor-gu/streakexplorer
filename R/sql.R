sql_get_connection <- function() {
  db_file <- system.file("extdata", "data.sqlite", package = "SOMData")
  DBI::dbConnect(RSQLite::SQLite(), db_file, extended_types = TRUE)
}

sql_get_intensity_level_range <- function() {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

  query <- ("
    SELECT MAX(IntensityLevel) as max_level,
           MIN(IntensityLevel) as min_level
    FROM hot_streaks
    WHERE Year == 1948
  ")

  result <- DBI::dbGetQuery(connection, query)
  c(result$min_level, result$max_level)
}


sql_get_max_rank <- function(min_year, max_year, teams, hot) {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

  table <- ifelse(hot, "hot_streaks", "cold_streaks")
  query_template <- (
    "
  WITH group_rank AS
     (SELECT Rank, row_number() OVER
        ( PARTITION BY IntensityLevel ORDER BY Rank ) rn
        FROM {table} WHERE
          Year >= {min_year} AND
          Year <= {max_year} AND
          Team IN ({teams*})
      )
  SELECT MAX(Rank) AS max_rank FROM group_rank WHERE rn <= 10
  "
  )
  query <- glue::glue_sql(
    query_template,
    table = table,
    min_year = min_year,
    max_year = max_year,
    teams = teams,
    .con = connection
  )
  DBI::dbGetQuery(connection, query) %>% dplyr::pull(max_rank)
}

# TODO DELETE THIS
sql_get_lines_old <- function(min_year, max_year, teams, hot, max_rank) {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

  table <- ifelse(hot, "hot_streaks_lines", "cold_streaks_lines")
  query_template <- (
    "
    SELECT * FROM {table} WHERE
      Year >= {min_year} AND
      Year <= {max_year} AND
      Team IN ({teams*}) AND
      Rank <= {max_rank}
  "
  )
  query <- glue::glue_sql(
    query_template,
    table = table,
    min_year = min_year,
    max_year = max_year,
    teams = teams,
    max_rank = max_rank,
    .con = connection
  )
  DBI::dbGetQuery(connection, query) %>% tibble::as_tibble()
}

# TODO make this cleaner
sql_get_lines <- function(min_year, max_year, teams, hot, max_rank) {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

  table <- ifelse(hot, "hot_streaks_lines", "cold_streaks_lines")
  query_template <- (
    "
    SELECT DISTINCT LineId AS LineId FROM {table} WHERE
      Year >= {min_year} AND
      Year <= {max_year} AND
      Team IN ({teams*}) AND
      Rank <= {max_rank}
  "
  )
  query <- glue::glue_sql(
    query_template,
    table = table,
    min_year = min_year,
    max_year = max_year,
    teams = teams,
    max_rank = max_rank,
    .con = connection
  )
  line_ids <- DBI::dbGetQuery(connection, query) %>% dplyr::pull(LineId)

  query_template <- ("
    SELECT * FROM {table} WHERE
      LineId in ({line_ids*})
  ")
  query <- glue::glue_sql(
    query_template,
    table = table,
    line_ids = line_ids,
    .con = connection
  )
  lines <- DBI::dbGetQuery(connection, query) %>% tibble::as_tibble()
  lines %>% dplyr::filter(Rank <= max_rank) %>% dplyr::count(LineId) %>%
    dplyr::filter(n>1) %>% dplyr::select(LineId) %>%
    dplyr::left_join(lines)
}

sql_get_streak_game_log <- function(streak, hot) {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

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
    .con = connection
  )
  DBI::dbGetQuery(connection, query) %>% tibble::as_tibble() %>%
    dplyr::mutate(
      Date=lubridate::as_date(Date),
      CompletedOn=lubridate::as_date(CompletedOn),
      CompletionOf=lubridate::as_date(CompletionOf))
}

sql_get_streak <- function(streak_id, hot) {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

  streak_table <- ifelse(hot, "hot_streaks", "cold_streaks")
  query_template <- (
    "
    SELECT Year, Team, LoIndex, HiIndex
    FROM {streak_table} WHERE StreakId = {streak_id} LIMIT 1
  "
  )
  query <- glue::glue_sql(
    query_template,
    streak_table = streak_table,
    streak_id = streak_id,
    .con = connection
  )
  streak <- DBI::dbGetQuery(connection, query) %>% tibble::as_tibble()

  query_template <-("
    SELECT Date FROM game_logs WHERE
      Year = {streak$Year} AND
      Team = {streak$Team} AND
      GameIndex = {streak$LoIndex}
  ")
  query <- glue::glue_sql(
    query_template,
    streak = streak,
    .con = connection
  )
  start_date <- DBI::dbGetQuery(connection, query) %>% dplyr::pull(Date) %>%
    lubridate::as_date()

  query_template <-("
    SELECT Date FROM game_logs WHERE
      Year = {streak$Year} AND
      Team = {streak$Team} AND
      GameIndex = {streak$HiIndex}
  ")
  query <- glue::glue_sql(
    query_template,
    streak = streak,
    .con = connection
  )
  end_date <- DBI::dbGetQuery(connection, query) %>% dplyr::pull(Date) %>%
    lubridate::as_date()

  streak %>% dplyr::mutate(StartDate=start_date, EndDate=end_date)
}

sql_get_division_season_games <- function(year, teams) {
  connection <- sql_get_connection()
  on.exit(DBI::dbDisconnect(connection))

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
    .con = connection
  )
  DBI::dbGetQuery(connection, query) %>% tibble::as_tibble() %>%
    dplyr::mutate(
      Date=lubridate::as_date(Date),
      CompletedOn=lubridate::as_date(CompletedOn),
      CompletionOf=lubridate::as_date(CompletionOf))
}

