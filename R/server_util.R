lzy_lines <- function(hot) {
  if (hot) {
    sql_load_hot_streaks_lines()
  } else {
    sql_load_cold_streaks_lines()
  }
}

lzy_streaks <- function(hot) {
  if (hot) {
    sql_load_hot_streaks()
  } else {
    sql_load_cold_streaks()
  }
}

lzy_lines_to_streaks <- function(hot) {
  if (hot) {
    sql_load_hot_streaks_lines_to_streaks()
  } else {
    sql_load_cold_streaks_lines_to_streaks()
  }
}

lzy_concordances <- function(hot) {
  if (hot) {
    sql_load_hot_streaks_concordances()
  } else {
    sql_load_cold_streaks_concordances()
  }
}

server_streak_summary_data <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- sql_load_game_logs()
  streaks_summary_data(lzy_game_logs, franchises, selected_streak)
}

server_build_lines <- function(years, teams, franchises, max_rank, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lines_build_lines(lzy_lines(hot), years, teams, franchises, max_rank, hot)
}

server_get_selected_streak <- function(selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- sql_load_game_logs()
  lines_get_selected_streak(
    lzy_lines_to_streaks(hot),
    lzy_streaks(hot),
    lzy_game_logs,
    selected_line_id)
}

server_lines_highlight <- function(lines, selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lines_highlight(lines,
                  lzy_concordances(hot),
                  lzy_lines_to_streaks(hot),
                  selected_line_id)
}

server_get_streak_standings <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_standings <- sql_load_standings()
  lzy_game_logs <- sql_load_game_logs()

  streaks_get_standings(lzy_standings, lzy_game_logs, franchises,
                        selected_streak)
}

server_get_streak_game_logs <- function(selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- sql_load_game_logs()
  streaks_game_log_data(lzy_game_logs, selected_streak)
}

server_build_streak_standings_graph <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_standings <- sql_load_standings()
  build_standings_graph(lzy_standings, franchises, selected_streak)
}
