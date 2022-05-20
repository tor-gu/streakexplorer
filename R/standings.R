#' standings_get_final_standings
#'
#' Get the final standings for a division.  `division` should be a single-row
#' table with `Year`, `League` and `Division` (possibly `NA`).
#'
#' @param lzy_standings Lazy standings table
#' @param division Division
#'
#' @return Standings table.
standings_get_final_standings <- function(lzy_standings, division) {
  if (is.na(division$Division)) {
    lzy_division_standings <- lzy_standings %>%
      dplyr::filter(Year==local(division$Year),
                    League==local(division$League))
  } else {
    lzy_division_standings <- lzy_standings %>%
      dplyr::filter(Year==local(division$Year),
                    League==local(division$League),
                    Division==local(division$Division))
  }
  lzy_division_standings %>%
    dplyr::collect() %>%
    dplyr::filter(Date==max(Date, na.rm=TRUE))
}

#' standings_get_same_day_team_games_lzy
#'
#' Given the year and the SeasonGameId, find all games between the same two
#' teams either earlier in the same day (when `before` is `TRUE`) or later
#' in the same day (when `before` is `FALSE`).  The result is a lazy query
#' to the game_logs table.
#'
#' The result is normally empty, and is only interesting when there is a
#' double-header.
#'
#'
#' @param lzy_game_logs Lazy game logs table
#' @param year Year
#' @param season_game_id SeasonGameId
#' @param before If `TRUE`, game games earlier in the day. If `FALSE`, later
#'
#' @return Lazy query
standings_get_same_day_team_games_lzy <- function(lzy_game_logs, year,
                                              season_game_id, before=TRUE) {
  # Get the game in question. There should be two rows here, one for each
  # involved in the game.
  games <- lzy_game_logs %>%
    dplyr::filter(SeasonGameId==season_game_id, Year==year) %>%
    dplyr::collect()
  date <- games$Date[[1]]

  # Now filter the game_logs by team, date, and season game id
  if (before) {
    lzy_game_logs %>%
      dplyr::filter(Team %in% local(games$Team), Date == date,
                    SeasonGameId < season_game_id)
  } else {
    lzy_game_logs %>%
      dplyr::filter(Team %in% local(games$Team), Date == date,
                    SeasonGameId > season_game_id)
  }
}



