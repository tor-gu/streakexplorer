pct_formatter <- function(pct) {
  ifelse(pct < 1,
         paste0(".", sprintf("%03d", round(1000*pct))),
         "1.000"
  )
}

streak_game_log <- function(streak_id, lines, game_logs) {
  row <- lines %>% dplyr::filter(StreakId==streak_id) %>% head(1)
  if (nrow(row) > 0) {
    game_logs %>%
      dplyr::filter(Year==row$Year,
             Team==row$Team,
             GameIndex >= row$LoIndex, GameIndex <= row$HiIndex)
  } else {
    game_logs %>% dplyr::filter(FALSE)
  }
}

streak_game_log_data <- function(streak_id, lines, game_logs) {
  game_log_data <- streak_game_log(streak_id, lines, game_logs) %>%
    mutate(`Gm#`=GameNumber,
           `Date `=glue::glue("{lubridate::month(Date)}/{lubridate::mday(Date)}"),
           Opp=ifelse(AtHome,
                      OpponentTeam,
                      glue::glue("@{OpponentTeam}")
           ),
           `W/L` = Result,
           RS = RunsFor,
           RA = RunsAgainst
    ) %>%
    select(`Gm#`:RA)

  result <- list(
    data=game_log_data,
    caption="Caption, lor"
  )
}

streak_summary_data <- function(streak_id, lines, game_logs) {
    summary_data <-
      streak_game_log(streak_id, lines, game_logs) %>%
      summarize(Team            = unique(Team),
                Year            = unique(Year),
                FirstGameNumber = min(GameNumber),
                FirstGameDate   = min(Date),
                LastGameNumber  = max(GameNumber),
                LastGameDate    = max(Date),
                Games           = sum(!is.na(Result)),
                Wins            = sum(Result=="W", na.rm=TRUE),
                Losses          = sum(Result=="L", na.rm=TRUE),
                Ties            = sum(Result=="T", na.rm=TRUE),
                RunsScored      = sum(RunsFor),
                RunsAllowed     = sum(RunsAgainst),
                WinningPct      = (Wins+Ties/2)/Games,
                PythagPct       = RunsScored^2 / (RunsScored^2 + RunsAllowed^2),
                PythagWins      = round(PythagPct * Games, digits=1),
                PythagLosses    = Games - PythagWins,
                HomeGames       = sum(AtHome==TRUE),
                AwayGames       = sum(AtHome==FALSE)
    )

    franchise_season <- franchises_by_season(SOMData::franchises,
                                             summary_data$Year) %>%
      dplyr::filter(FranchiseID == summary_data$Team)

    data <-
      summary_data %>%
      mutate(
        Start=glue::glue("{lubridate::month(FirstGameDate)}/{lubridate::mday(FirstGameDate)}"),
        End=glue::glue("{lubridate::month(LastGameDate)}/{lubridate::mday(LastGameDate)}"),
        Dates=glue::glue(ifelse(FirstGameDate==LastGameDate, "{Start}", "{Start} - {End}")),
        Record = ifelse(Ties==0,
                        glue::glue("{Wins}-{Losses}"),
                        glue::glue("{Wins}-{Losses}-{Ties}")),
        `W-L%`=pct_formatter(WinningPct),
        RS=RunsScored, RA=RunsAllowed, `Pyth%`=pct_formatter(PythagPct)
      ) %>%
      select(Dates:`Pyth%`)

    caption <- glue::glue(paste0(
      "{summary_data$Year} ",
      "{franchise_season$Location} {franchise_season$Nickname}, ",
      "Games {summary_data$FirstGameNumber}-{summary_data$LastGameNumber}"
    ))

    list(data=data, caption=caption)
}



