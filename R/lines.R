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





