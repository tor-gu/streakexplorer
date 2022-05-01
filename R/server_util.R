# Utility functions to load the lazy queries ----
lzy_lines <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks_lines")
  } else {
    dplyr::tbl(se_pool, "cold_streaks_lines")
  }
}

lzy_streaks <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks")
  } else {
    dplyr::tbl(se_pool, "cold_streaks")
  }
}

lzy_lines_to_streaks <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks_lines_to_streaks")
  } else {
    dplyr::tbl(se_pool, "cold_streaks_lines_to_streaks")
  }
}

lzy_concordances <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks_concordances")
  } else {
    dplyr::tbl(se_pool, "cold_streaks_concordances")
  }
}

# Main server functions ----
server_streak_summary_data <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
  streaks_summary_data(lzy_game_logs, franchises, selected_streak)
}

server_build_lines <- function(franchises, intensity_level_range, years, teams,
                               max_rank, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  if (hot) {
    left_intensity <- intensity_level_range[[1]]
  } else {
    left_intensity <- intensity_level_range[[2]]
  }
  team_ids <- franchises_franchise_ids_to_team_ids(franchises, teams, min_year,
                                                   max_year)

  lines_build_lines(lzy_lines(hot), min_year, max_year, team_ids,
                    franchises, max_rank, left_intensity)
}

server_get_selected_streak <- function(selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
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

  # The lazy standings table will generate a bunch of warnings about the
  # decimal column, which we don't care about
  withCallingHandlers({
    lzy_standings <- dplyr::tbl(se_pool, "standings")
    lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
    streaks_get_standings(lzy_standings, lzy_game_logs, franchises,
                          selected_streak)
  }, warning = function(wrn) {
    if (stringr::str_starts(wrn$message, "Decimal MySQL")) {
      rlang::cnd_muffle(wrn)
    }
  })
}

server_get_streak_game_logs <- function(selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
  streaks_game_log_data(lzy_game_logs, selected_streak)
}

server_build_streak_standings_graph <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))

  # The standings table will generate a bunch of warnings about the
  # decimal column, which we don't care about.
  withCallingHandlers({
    lzy_standings <- dplyr::tbl(se_pool, "standings")
    plot_build_standings_graph(lzy_standings, franchises, selected_streak)
  }, warning = function(wrn) {
    if (stringr::str_starts(wrn$message, "Decimal MySQL")) {
      rlang::cnd_muffle(wrn)
    }
  })
}

server_get_max_rank <- function(franchises, years, teams, hot) {
  # Get the max rank using n=10
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  team_ids <- franchises_franchise_ids_to_team_ids(franchises, teams, min_year,
                                                   max_year)
  streaks_get_max_rank(10, min_year, max_year, team_ids, hot)
}

server_main_plot <- function(highlighted_lines, intensity_level_range,
                             max_rank, hot) {
  if (
    torgutil::tbl_is_column_value_unique(highlighted_lines, Year) &
    torgutil::tbl_is_column_value_unique(highlighted_lines, Team)
  ) {
    # For single-team plots, treat the "season" lines like the "base" lines
    highlighting <- tibble::tribble(
      ~line_type, ~color, ~width,
      "base",     highlight_colors$base, 1,
      "season",   highlight_colors$base, 1,
      "related",  highlight_colors$medium, 3,
      "identical",highlight_colors$high, 5
    )
  } else {
    highlighting <- tibble::tribble(
      ~line_type, ~color, ~width,
      "base",     highlight_colors$base, 1,
      "season",   highlight_colors$low, 3,
      "related",  highlight_colors$medium, 3,
      "identical",highlight_colors$high, 5
    )
  }
  min_intensity <- intensity_level_range[[1]]
  max_intensity <- intensity_level_range[[2]]
  plot_lines(
    highlighted_lines,
    min_intensity,
    max_intensity,
    max_rank,
    highlighting,
    reverse_x_axis = !hot
  )

}
