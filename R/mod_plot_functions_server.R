# Top level mod_plot server functions ----

plot_server_get_max_rank <- function(db_pool, franchises, years, teams, hot) {
  # Get the max rank using n=10
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  team_ids <- ps_franchise_ids_to_team_ids(franchises, teams, min_year,
                                           max_year)
  lzy_streaks <- lzy_streaks(db_pool, hot)
  ps_streaks_get_max_rank(lzy_streaks, 10, min_year, max_year, team_ids, hot)
}

plot_server_lines_highlight <- function(db_pool, lines, selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  ps_lines_highlight(lines,
                     lzy_concordances(db_pool, hot),
                     lzy_lines_to_streaks(db_pool, hot),
                     selected_line_id)
}

plot_server_build_lines <- function(db_pool, franchises, intensity_level_range,
                                    years, teams, max_rank, hot) {
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

  ps_build_lines(lzy_lines(db_pool, hot), min_year, max_year, team_ids,
                 franchises, max_rank, left_intensity)
}

plot_server_get_selected_streak <- function(db_pool, selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(db_pool, "game_logs")
  ps_lines_to_related_streak(
    lzy_lines_to_streaks(db_pool, hot),
    lzy_streaks(db_pool, hot),
    lzy_game_logs,
    selected_line_id)
}

plot_server_main_plot <- function(highlight_colors, highlighted_lines,
                                  intensity_level_range, max_rank, hot) {
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
  ps_plot_lines(
    highlight_colors,
    highlighted_lines,
    min_intensity,
    max_intensity,
    max_rank,
    highlighting,
    reverse_x_axis = !hot
  )
}

# Utility functions for mod_plot server ----

#' ps_build_lines
#'
#' Generate the table of lines for the given filter (years, teams and rank).
#' The left intensity is requires so that the full-season lines are always
#' included.
#'
#' @param lzy_lines Lazy lines table
#' @param min_year Min Year
#' @param max_year Max Year
#' @param team_ids Vector of teamIDs
#' @param franchises Franchises table
#' @param max_rank Max Rank
#' @param left_intensity Left-most intensity level (min for hot, max for cold)
#'
#' @return
ps_build_lines <- function(lzy_lines, min_year, max_year, team_ids,
                              franchises, max_rank, left_intensity) {
  # Get the left-most line elements, to add in at the end
  # The reason this is a special case is that sometimes the
  # left-most line contains a single node, and we don't want to
  # strip it out with all the other single-node lines.
  left_lines <- lzy_lines %>%
    dplyr::filter(between(Year, min_year, max_year),
                  Team %in% team_ids, Rank <= max_rank,
                  IntensityLevel == left_intensity) %>%
    dplyr::collect()

  # Get the lines
  lzy_lines %>%
    # Initial filter by years, teams, and ranks
    dplyr::filter(between(Year, min_year, max_year),
                  Team %in% team_ids, Rank <= max_rank) %>%
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



#' ps_lines_to_related_streak
#'
#' Given a line_id, get the related streak, as single-row table with
#' columns `Year`, `Team`, `LoIndex`,` HiIndex`, `StartDate`, and `EndDate`.
#'
#' @param lzy_lines_to_streaks Lazy lines_to_streaks table
#' @param lzy_streaks  Lazy streaks table
#' @param lzy_game_logs Lazy game_logs table
#' @param line_id LineID
#'
#' @return Streak
ps_lines_to_related_streak <- function(lzy_lines_to_streaks, lzy_streaks,
                                      lzy_game_logs, line_id) {
  if (is.null(line_id)) {
    NULL
  } else {
    lzy_lines_to_streaks %>%
      dplyr::filter(LineId==line_id) %>%
      dplyr::left_join(lzy_streaks, by="StreakId") %>%
      head(1) %>%
      dplyr::select(Year, Team, LoIndex, HiIndex) %>%
      dplyr::left_join(lzy_game_logs,
                       by=c("Year","Team","LoIndex"="GameIndex")) %>%
      dplyr::left_join(lzy_game_logs,
                       by=c("Year","Team","HiIndex"="GameIndex")) %>%
      dplyr::collect() %>%
      dplyr::mutate(StartDate = lubridate::as_date(Date.x)) %>%
      dplyr::mutate(EndDate = lubridate::as_date(Date.y)) %>%
      dplyr::select(Year, Team, LoIndex, HiIndex, StartDate, EndDate)
  }
}

#' ps_get_related_streak_ids
#'
#' Given a streak ID, returns all related streaks in the concordance table --
#' both super-streaks and sub-streaks -- including the streak itself.
#'
#' @param streak_id  Streak ID
#' @param lzy_concordances Lazy conconcrdance table
#'
#' @return vector of related streak IDs.
ps_get_related_streak_ids <- function(streak_id, lzy_concordances) {
  inner <- lzy_concordances %>%
    dplyr::filter(Inner == streak_id) %>%
    dplyr::pull(Outer)
  outer <- lzy_concordances %>%
    dplyr::filter(Outer == streak_id) %>%
    dplyr::pull(Inner)
  c(inner, outer) %>% unique()
}






