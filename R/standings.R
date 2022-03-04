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
  games <- game_logs %>% dplyr::filter(SeasonGameId==season_game_id)
  date <- games$Date[[1]]
  if (.before) {
    game_logs %>%
      dplyr::filter(Team %in% games$Team, Date == date,
                    SeasonGameId < season_game_id)
  } else {
    game_logs %>%
      dplyr::filter(Team %in% games$Team, Date == date,
                    SeasonGameId > season_game_id)
  }
}

standings_get_by_season_game_id <- function(game_logs, season_game_id,
                                            .before=TRUE) {
  games <- game_logs %>% dplyr::filter(SeasonGameId==season_game_id)
  date <- games$Date[[1]]
  if (.before) {
    # include all games before the date, plus any games this team
    # has played earlier in the day
    standings_games <- rbind(
      game_logs %>% dplyr::filter(Date < date),
      game_logs %>%
        standings_get_same_day_team_games(season_game_id, .before=TRUE)
    )
  } else {
    # Was this the team's last game of the day or not?
    later_games <- game_logs %>%
      standings_get_same_day_team_games(season_game_id, .before=FALSE)

    if (nrow(later_games) > 0) {
      # This is not the last game of the day -- only include
      # games played before this date, the current game, and
      # any games this team played earlier in the day.
      # (Note: the 'games earlier in the day' scenario would be when there
      # are *three* games in a day and we have picked the middle game. There
      # are no examples of this in the db.)
      standings_games <- rbind(
        game_logs %>% dplyr::filter(Date < date),
        games,
        game_logs %>%
          standings_get_same_day_team_games(season_game_id, .before=TRUE)
      )
    } else {
      # This is the last game of the day -- include the whole day
      standings_games <- game_logs %>% dplyr::filter(Date <= date)
    }
  }
  standings_games %>% standings_from_game_logs()
}

