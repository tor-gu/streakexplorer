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

