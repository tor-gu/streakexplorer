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

server_streak_summary_data <- function(selected_streak) {
  lzy_franchises <- sql_load_franchises()
  lzy_game_logs <- sql_load_game_logs()
  streaks_summary_data(lzy_game_logs, lzy_franchises, selected_streak)
}

server_build_lines <- function(years, teams, franchises, max_rank, hot) {
  lines_build_lines(lzy_lines(hot), years, teams, franchises, max_rank, hot)
}

server_get_selected_streak <- function(selected_line_id, hot) {
  lzy_game_logs <- sql_load_game_logs()
  lines_get_selected_streak(
    lzy_lines_to_streaks(hot),
    lzy_streaks(hot),
    lzy_game_logs,
    selected_line_id)
}

server_lines_highlight <- function(lines, selected_line_id, hot) {
  lines_highlight(lines,
                  lzy_concordances(hot),
                  lzy_lines_to_streaks(hot),
                  selected_line_id)
}

server_get_streak_standings <- function(selected_streak) {
  lzy_standings <- sql_load_standings()
  lzy_franchises <- sql_load_franchises()
  lzy_game_logs <- sql_load_game_logs()

  streaks_get_standings(lzy_standings, lzy_game_logs, lzy_franchises,
                        selected_streak)
}

server_get_streak_game_logs <- function(selected_streak) {
  lzy_game_logs <- sql_load_game_logs()
  streaks_game_log_data(lzy_game_logs, selected_streak)
}

server_build_streak_standings_graph <- function(selected_streak) {
  lzy_standings <- sql_load_standings()
  lzy_franchises <- sql_load_franchises()
  build_standings_graph(lzy_franchises, lzy_standings, selected_streak)
}
