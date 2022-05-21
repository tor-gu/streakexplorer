#' ss_get_standings_for_streak
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
ss_get_standings_for_streak <- function(lzy_standings, lzy_game_logs,
                                        franchises, streak) {
  # Get the division
  division <- franchises %>%
    ss_get_division_by_team_year(streak$Team, streak$Year)
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
    ss_get_standings_by_season_game_id(lzy_standings,
                                       lzy_division_season_games,
                                       division$division,
                                       first_game$SeasonGameId,
                                       before=TRUE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)
  # Calculate the standings after
  standings_after <-
    ss_get_standings_by_season_game_id(lzy_standings,
                                       lzy_division_season_games,
                                       division$division,
                                       last_game$SeasonGameId,
                                       before=FALSE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)
  # Look up the final standings
  standings_final <- ss_get_final_standings(lzy_standings,
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

#' ss_get_standings_by_season_game_id
#'
#' Given a division and a SeasonGameId, get the standings, either at
#' the start of day (`before` is `TRUE`) or the end of day (`before` is
#' `FALSE`).
#'
#' @param lzy_standings Lazy standings table
#' @param lzy_game_logs Lazy game logs table
#' @param division division table (`Year`, `League` and `Division`)
#' @param season_game_id SeasonGameId
#' @param before If `TRUE`, use start-of-day. Otherwise, end-of-day.
#'
#' @return Standings table
ss_get_standings_by_season_game_id <- function(lzy_standings,
                                               lzy_game_logs,
                                               division,
                                               season_game_id,
                                               before = TRUE) {
  # Filter the standings based on the division
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

  # Get both instances of the game (home team and away team)
  both_games <- lzy_game_logs %>%
    dplyr::filter(SeasonGameId == season_game_id,
                  Year == local(division$Year)) %>%
    dplyr::collect()
  date <- both_games$Date[[1]]
  year <- both_games$Year[[1]]

  if (before) {
    # include all games before the date, plus any games this team
    # has played earlier in the day
    date_before <- lubridate::ymd(date) - 1
    lzy_earlier_games <- lzy_game_logs %>%
      ss_get_same_day_team_games_lzy(year, season_game_id, before = TRUE)
    lzy_division_standings %>%
      dplyr::filter(Date == date_before) %>%
      ss_update_standings_from_game_logs(lzy_earlier_games)
  } else {
    # Was this both teams' last game of the day or not?
    lzy_later_games <- lzy_game_logs %>%
      ss_get_same_day_team_games_lzy(year, season_game_id,
                                     before = FALSE)
    if (lzy_later_games %>% dplyr::collect() %>% nrow() > 0) {
      # This is not the last game of the day -- only include
      # games played before this date, the current game, and
      # any games this team played earlier in the day.
      # (Note: the 'games earlier in the day' scenario would be when there
      # are *three* games in a day and we have picked the middle game. There
      # are no examples of this in the db.)
      date_before <- lubridate::ymd(date) - 1
      lzy_earlier_games <- lzy_game_logs %>%
        ss_get_same_day_team_games_lzy(year, season_game_id,
                                       before = TRUE)
      lzy_division_standings %>%
        dplyr::filter(Date == date_before) %>%
        ss_update_standings_from_game_logs(both_games) %>%
        ss_update_standings_from_game_logs(lzy_earlier_games)
    } else {
      # This is the last game of the day -- include the whole day
      lzy_division_standings %>%
        dplyr::filter(Date == date) %>%
        dplyr::collect()
    }
  }
}

#' ss_update_standings_from_game_logs
#'
#' Add the results of the game logs to a standings table.
#'
#' This is used for handling the case when we want to add a partial days
#' games to a precalculated standings table, because of a double-header.
#'
#' @param lzy_standings  Lazy standings table
#' @param lzy_games_to_add Game logs to add
#'
#' @return Updated standings
ss_update_standings_from_game_logs <- function(lzy_standings, lzy_games_to_add) {
  standings <- lzy_standings %>% dplyr::collect()
  game_logs <- lzy_games_to_add %>% dplyr::collect() %>%
    dplyr::filter(!is.na(Result))
  if (nrow(game_logs) > 0) {
    for (i in 1:nrow(game_logs)) {
      game <- game_logs[i,]
      standings <- standings %>% dplyr::mutate(
        Wins=ifelse(Team==game$Team, Wins+(game$Result=="W"), Wins),
        Losses=ifelse(Team==game$Team, Losses+(game$Result=="L"), Losses),
        Ties=ifelse(Team==game$Team, Ties+(game$Result=="T"), Ties)
      )
    }
    # We added at least on game, so we need to recalculate the GB
    standings <- standings %>%
      dplyr::arrange(desc(Wins-Losses)) %>%
      dplyr::mutate(
        GB=(dplyr::first(Wins)-Wins + Losses-dplyr::first(Losses))/2)
  }
  standings
}

#' ss_get_final_standings
#'
#' Get the final standings for a division.  `division` should be a single-row
#' table with `Year`, `League` and `Division` (possibly `NA`).
#'
#' @param lzy_standings Lazy standings table
#' @param division Division
#'
#' @return Standings table.
ss_get_final_standings <- function(lzy_standings, division) {
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

#' ss_get_same_day_team_games_lzy
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
ss_get_same_day_team_games_lzy <- function(lzy_game_logs, year, season_game_id,
                                           before=TRUE) {
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
