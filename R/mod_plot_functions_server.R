# Top level mod_plot server functions ----

plot_server_get_max_rank <- function(franchises, years, teams, hot) {
  # Get the max rank using n=10
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  team_ids <- franchises_franchise_ids_to_team_ids(franchises, teams, min_year,
                                                   max_year)
  streaks_get_max_rank(10, min_year, max_year, team_ids, hot)
}

plot_server_lines_highlight <- function(lines, selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lines_highlight(lines,
                  lzy_concordances(hot),
                  lzy_lines_to_streaks(hot),
                  selected_line_id)
}

plot_server_build_lines <- function(franchises, intensity_level_range, years, teams,
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

plot_server_get_selected_streak <- function(selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
  lines_get_selected_streak(
    lzy_lines_to_streaks(hot),
    lzy_streaks(hot),
    lzy_game_logs,
    selected_line_id)
}

plot_server_main_plot <- function(highlighted_lines, intensity_level_range,
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
