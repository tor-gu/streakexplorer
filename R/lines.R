#' lines_get_related_lines
#'
#' Given a line_id, find the line_ids associated to all related streaks.
#'
#' @param line_id LineID
#' @param lzy_lines_to_streaks  Lazy lines_to_streaks table
#' @param lzy_concordances Lazy concordances table
#'
#' @return
lines_get_related_lines <- function(line_id, lzy_lines_to_streaks,
                                    lzy_concordances) {
  related_streak_ids <- lzy_lines_to_streaks %>%
    dplyr::filter(LineId == line_id) %>%
    dplyr::pull(StreakId) %>%
    purrr::map(streaks_get_related_streak_ids, lzy_concordances) %>%
    unlist(recursive = FALSE) %>%
    unique()
  lzy_lines_to_streaks %>%
    dplyr::filter(StreakId %in% related_streak_ids) %>%
    dplyr::pull(LineId)
}


#' lines_highlight
#'
#' Given a table of `lines` and a `line_id`, add a new column, `line_type`,
#' with values:
#' * `"identical"`: Matching line_id
#' * `"related"`: Line represents a sub- or super-streak, but is not identical
#' * `"season"`: Line is from same year and team, but not a sub- or super-streak
#' * `"base"`: Unrelated line
#'
#' If `line_id` is `NULL` or omitted, all values will be `"base"`
#'
#' @param lines Table of lines
#' @param lzy_concordances Lazy concordances table
#' @param lzy_lines_to_streaks  Lazy lines_to_streaks table
#' @param line_id LineID
#'
#' @return Lines with added `line_type` column.
lines_highlight <- function(lines, lzy_concordances, lzy_lines_to_streaks,
                            line_id = NULL) {
  # Initialize with line_type == "base"
  result <- lines %>% dplyr::mutate(line_type = "base")

  # Check that we were passed a line_id
  if (!is.null(line_id)) {
    # Get the Team and Year from the first line matching the line_id
    row <- lines %>%
      dplyr::filter(LineId == line_id) %>%
      head(1)
    if (nrow(row) > 0) {
      team <- row$Team
      year <- row$Year
      # Get related line ids
      related_line_ids <- lines_get_related_lines(
        line_id, lzy_lines_to_streaks,
        lzy_concordances
      )
      result <- result %>%
        # Same Year and Team:  line_type = "season"
        dplyr::mutate(
          line_type = dplyr::if_else(Year == year & Team == team, "season",
            line_type
          ),
        ) %>%
        # Substreak or superstreak: line_type = "related"
        dplyr::mutate(
          line_type = dplyr::if_else(LineId %in% related_line_ids, "related",
            line_type
          ),
        ) %>%
        # Same streak: line_type = "identical"
        dplyr::mutate(
          line_type = dplyr::if_else(LineId == line_id, "identical", line_type),
        )
    }
  }
  result
}

#' lines_build_lines
#'
#' Generate the table of lines for the given filter (years, teams and rank)
#'
#' @param lzy_lines Lazy lines table
#' @param min_year Min Year
#' @param max_year Max Year
#' @param teams Vector of teamIDs
#' @param franchises Franchises table
#' @param max_rank Max Rank
#'
#' @return
lines_build_lines <- function(lzy_lines, min_year, max_year, teams,
                              franchises, max_rank) {
  team_ids <- franchises_franchise_ids_to_team_ids(
    franchises, teams, min_year, max_year)
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
    dplyr::collect()
}


#' lines_get_selected_streak
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
lines_get_selected_streak <- function(lzy_lines_to_streaks, lzy_streaks,
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

