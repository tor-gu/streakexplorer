# Top level mod_plot server functions ----

plot_server_get_max_rank <- function(franchises, years, teams, hot) {
  # Get the max rank using n=10
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  team_ids <- ps_franchise_ids_to_team_ids(franchises, teams, min_year,
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
  team_ids <- ps_franchise_ids_to_team_ids(franchises, teams, min_year,
                                                   max_year)

  ps_build_lines(lzy_lines(hot), min_year, max_year, team_ids,
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

#' ps_build_lines
#'
#' Generate the table of lines for the given filter (years, teams and rank).
#' The left intensity is requires so that the full-season lines are always
#' included.
#'
#' @param lzy_lines Lazy lines table
#' @param min_year Min Year
#' @param max_year Max Year
#' @param teams Vector of teamIDs
#' @param franchises Franchises table
#' @param max_rank Max Rank
#' @param left_intensity Left-most intensity level (min for hot, max for cold)
#'
#' @return
ps_build_lines <- function(lzy_lines, min_year, max_year, teams,
                              franchises, max_rank, left_intensity) {
  # Get the team-ids
  team_ids <- ps_franchise_ids_to_team_ids(
    franchises, teams, min_year, max_year)

  # Get the left-most line elements, to add in at the end
  # The reason this is a special case is that sometimes the
  # left-most line contains a single node, and we don't want to
  # strip it out with all the other single-node lines.
  left_lines <- lzy_lines %>%
    dplyr::filter(between(Year, min_year, max_year),
                  Team %in% teams, Rank <= max_rank,
                  IntensityLevel == left_intensity) %>%
    dplyr::collect()

  # Get the lines
  lzy_lines %>%
    # Initial filter by years, teams, and ranks
    dplyr::filter(between(Year, min_year, max_year),
                  Team %in% teams, Rank <= max_rank) %>%
    # Now filter out lines that have only a single node above the cutoff
    dplyr::count(LineId) %>%
    dplyr::filter(n>1) %>%

    # Now add back in the whole lines for what remains
    dplyr::select(LineId) %>%
    dplyr::left_join(lzy_lines, by="LineId") %>%
    dplyr::filter(IntensityLevel != left_intensity) %>%
    dplyr::collect() %>%

    # Finally, add the left-most line elements
    rbind(left_lines)
}

#' ps_franchise_ids_to_team_ids
#'
#' Find all TeamIDs for the given FranchiseIDs and the year range.
#'
#' @param franchises Franchise table
#' @param franchise_ids vector of FranchiseIDs
#' @param min_year First year
#' @param max_year Final year
#'
#' @return List of TeamIDs
ps_franchise_ids_to_team_ids <- function(franchises, franchise_ids,
                                                 min_year, max_year) {
  franchises %>%
    dplyr::filter(FranchiseID %in% franchise_ids,
                  min_year <= FinalSeason | is.na(FinalSeason),
                  max_year >= FirstSeason) %>%
    dplyr::pull(TeamID)
}


