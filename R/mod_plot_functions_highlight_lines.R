#' ps_lines_highlight
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
ps_lines_highlight <- function(lines, lzy_concordances, lzy_lines_to_streaks,
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
      related_line_ids <- ps_get_related_lines(
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

#' ps_get_related_lines
#'
#' Given a line_id, find the line_ids associated to all related streaks.
#'
#' @param line_id LineID
#' @param lzy_lines_to_streaks  Lazy lines_to_streaks table
#' @param lzy_concordances Lazy concordances table
#'
#' @return A vector of LineIDs
ps_get_related_lines <- function(line_id, lzy_lines_to_streaks,
                                 lzy_concordances) {
  related_streak_ids <- lzy_lines_to_streaks %>%
    dplyr::filter(LineId == line_id) %>%
    dplyr::pull(StreakId) %>%
    purrr::map(ps_get_related_streak_ids, lzy_concordances) %>%
    unlist(recursive = FALSE) %>%
    unique()
  lzy_lines_to_streaks %>%
    dplyr::filter(StreakId %in% related_streak_ids) %>%
    dplyr::pull(LineId)
}

