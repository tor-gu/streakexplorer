


#' streaks_get_standings
#'
#' Given a streak, return the division standings before the streak started
#' (`standings_before`), after the last game of the streak (`standings_after`),
#' and at the end of the season (`standings_final`). Also compute a summary
#' object (`streak_info`).
#'
#' @param lzy_standings Lazy standings table
#' @param lzy_game_logs Lazy game logs table
#' @param franchises Franchises table
#' @param streak Streak
#'
#' @return list with `streak_info`, `standings_before`, `standings_after`,
#' and `standings_final`
streaks_get_standings <- function(lzy_standings, lzy_game_logs,
                                  franchises, streak) {
  # Get the division
  division <- franchises %>%
    franchises_get_division_by_team_year(streak$Team, streak$Year)
  division_teams <- division$teams$TeamID

  # Set up a query for all games for all teams in the division for the year.
  lzy_division_season_games <-
    lzy_game_logs %>%
    dplyr::filter(Year==local(streak$Year), Team %in% division_teams)

  # Get the first game and the last game
  first_game <- lzy_division_season_games %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  GameIndex==local(streak$LoIndex)) %>%
    dplyr::collect()
  last_game <-  lzy_division_season_games %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  GameIndex==local(streak$HiIndex)) %>%
    dplyr::collect()
  # Calculate the standings before
  standings_before <-
    standings_get_by_season_game_id(lzy_standings,
                                    lzy_division_season_games,
                                    division$division,
                                    first_game$SeasonGameId,
                                    before=TRUE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)
  # Calculate the standings after
  standings_after <-
    standings_get_by_season_game_id(lzy_standings,
                                    lzy_division_season_games,
                                    division$division,
                                    last_game$SeasonGameId,
                                    before=FALSE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)
  # Look up the final standings
  standings_final <- standings_get_final_standings(lzy_standings,
                                                   division$division) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)

  # Gather summary streak info
  streak_info <- division$division %>%
    dplyr::mutate(Year=local(streak$Year),
                  Team=local(streak$Team),
                  Start=local(first_game$Date),
                  End=local(last_game$Date)) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID"))

  # Assemble the result
  list(
    streak_info=streak_info,
    standings_before=standings_before,
    standings_final=standings_final,
    standings_after=standings_after
  )
}


#' streaks_get_intensity_range
#'
#' Use the streaks table to get the the minimum and maximum intensity
#' levels in the DB.  The year is specified to make the query quicker.
#'
#' @param lzy_streaks  Lazy streaks table
#' @param year  Year to query.
#'
#' @return Vector with min and max intensity levels, e.g. `c(1,101)`
streaks_get_intensity_range <- function(lzy_streaks, year) {
  lzy_streaks %>%
    dplyr::filter(Year==year) %>%
    dplyr::distinct(IntensityLevel) %>%
    dplyr::collect() %>%
    dplyr::summarize(min_level=min(IntensityLevel),
                     max_level=max(IntensityLevel)) %>%
    purrr::transpose() %>%
    unlist()
}

