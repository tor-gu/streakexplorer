streak_summary <- function(streak_id, streaks, game_logs) {
    streak_game_log(streak_id, streaks, game_logs) %>%
    summarize(FirstGameNumber = min(GameNumber),
              FirstGameDate   = min(Date),
              LastGameNumber  = max(GameNumber),
              LastGameDate    = max(Date),
              Games           = sum(!is.na(Result)),
              Wins            = sum(Result=="W", na.rm=TRUE),
              Losses          = sum(Result=="L", na.rm=TRUE),
              Ties            = sum(Result=="T", na.rm=TRUE),
              RunsScored      = sum(RunsFor),
              RunsAllowed     = sum(RunsAgainst),
              WinningPct      = Wins/(Wins+Losses+Ties),
              PythagPct       = RunsScored^2 / (RunsScored^2 + RunsAllowed^2),
              PythagWins      = round(PythagPct * Games, digits=1),
              PythagLosses    = Games - PythagWins,
              HomeGames       = sum(AtHome==TRUE),
              AwayGames       = sum(AtHome==FALSE)
    )
}

streak_game_log <- function(streak_id, streaks, game_logs) {
  row <- streaks %>% filter(StreakId==streak_id) %>% head(1)
  game_logs %>%
    filter(Year==row$Year,
           Team==row$Team,
           GameIndex >= row$LoIndex, GameIndex <= row$HiIndex)
}

