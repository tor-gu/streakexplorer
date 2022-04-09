standings_update_from_game_logs <- function(standings, game_logs) {
  standings <- standings %>% dplyr::as_tibble()
  game_logs <- game_logs %>% dplyr::as_tibble() %>%
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
  }
  standings
}

standings_from_game_logs <- function(game_logs) {
  game_logs %>%
    dplyr::group_by(Team) %>%
    dplyr::summarise(
      Wins=sum(Result=="W", na.rm = TRUE),
      Losses=sum(Result=="L", na.rm = TRUE),
      Ties=sum(Result=="T", na.rm = TRUE)) %>%
    dplyr::arrange(desc(Wins-Losses)) %>%
    dplyr::mutate(
      GB=(dplyr::first(Wins)-Wins + Losses-dplyr::first(Losses))/2)
}

standings_get_by_date <- function(game_logs, date, .include=FALSE) {
  if (.include) {
    game_logs %>%
      dplyr::filter(Date <= date) %>%
      standings_from_game_logs()
  } else {
    game_logs %>%
      dplyr::filter(Date < date) %>%
      standings_from_game_logs()
  }
}

standings_get_same_day_team_games <- function(game_logs, season_game_id,
                                              .before=TRUE) {
  games <- game_logs %>% dplyr::filter(SeasonGameId==season_game_id) %>%
    dplyr::as_tibble()
  date <- games$Date[[1]]
  if (.before) {
    game_logs %>%
      dplyr::filter(Team %in% local(games$Team), Date == date,
                    SeasonGameId < season_game_id)
  } else {
    game_logs %>%
      dplyr::filter(Team %in% local(games$Team), Date == date,
                    SeasonGameId > season_game_id)
  }
}

standings_get_by_season_game_id <- function(standings,
                                              division,
                                              game_logs,
                                              season_game_id,
                                              before = TRUE) {
  division_standings <-
    standings %>% dplyr::right_join(division$division, na_matches="na")
  games <- game_logs %>% dplyr::filter(SeasonGameId == season_game_id) %>%
    dplyr::as_tibble()
  date <- games$Date[[1]]
  year <- games$Year[[1]]
  if (before) {
    # include all games before the date, plus any games this team
    # has played earlier in the day
    date_before <- lubridate::ymd(date) - 1
    earlier_games <- game_logs %>%
      standings_get_same_day_team_games(season_game_id, .before = TRUE)
    division_standings %>% dplyr::filter(Date == date_before) %>%
      standings_update_from_game_logs(earlier_games)
  } else {
    # Was this both teams' last game of the day or not?
    later_games <- game_logs %>%
      standings_get_same_day_team_games(season_game_id, .before = FALSE)
    if (later_games %>% dplyr::as_tibble() %>% nrow() > 0) {
      # This is not the last game of the day -- only include
      # games played before this date, the current game, and
      # any games this team played earlier in the day.
      # (Note: the 'games earlier in the day' scenario would be when there
      # are *three* games in a day and we have picked the middle game. There
      # are no examples of this in the db.)
      date_before <- lubridate::ymd(date) - 1
      earlier_games <- game_logs %>%
        standings_get_same_day_team_games(season_game_id, .before = TRUE)
      division_standings %>% dplyr::filter(Date == date_before) %>%
        standings_update_from_game_logs(games) %>%
        standings_update_from_game_logs(earlier_games)
    } else {
      # This is the last game of the day -- include the whole day
      division_standings %>% dplyr::filter(Date == date)
    }
  }
}


