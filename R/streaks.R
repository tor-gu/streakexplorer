streak_get_related_streak_ids <- function(StreakId, concordances) {
  Inner <- concordances %>% filter(Inner==StreakId) %>% pull(Outer)
  Outer <- concordances %>% filter(Outer==StreakId) %>% pull(Inner)
  c(Inner,Outer) %>% unique()
}

pct_formatter <- function(pct) {
  ifelse(pct < 1,
         paste0(".", sprintf("%03d", round(1000*pct))),
         "1.000"
  )
}

streak_game_log <- function(streak_id, streaks, game_logs) {
  row <- streaks %>% dplyr::filter(StreakId==streak_id) %>% head(1)
  if (nrow(row) > 0) {
    game_logs %>%
      dplyr::filter(Year==row$Year,
             Team==row$Team,
             GameIndex >= row$LoIndex, GameIndex <= row$HiIndex)
  } else {
    game_logs %>% dplyr::filter(FALSE)
  }
}

streak_game_log_data <- function(streak_id, streaks, game_logs) {
  date_template <- "{lubridate::month(Date)}/{lubridate::mday(Date)}"
  completed_on_template <-
    paste0("Completed {lubridate::month(CompletedOn)}/",
           "{lubridate::mday(CompletedOn)}, {GameResult} ",
           "{GameRunsFor}-{GameRunsAgainst}")
  completion_of_template <-
    paste0("Final score {GameRunsFor}-{GameRunsAgainst}, began ",
           "{lubridate::month(CompletionOf)}/{lubridate::mday(CompletionOf)}")

  game_log_data <- streak_game_log(streak_id, streaks, game_logs) %>%
    mutate(GameResult=case_when(GameRunsFor >  GameRunsAgainst ~ "W",
                                 GameRunsFor <  GameRunsAgainst ~ "L",
                                 GameRunsFor == GameRunsAgainst ~ "T"),
           `Gm#`=GameNumber,
           `Date `=glue::glue(date_template),
           Opp=ifelse(AtHome,
                      OpponentTeam,
                      glue::glue("@{OpponentTeam}")
           ),
           `W/L` = Result,
           RS = RunsFor,
           RA = RunsAgainst,
           Completion=NA
    ) %>%
    dplyr::mutate(Completion=ifelse(!is.na(CompletedOn),
                                    glue::glue(completed_on_template),
                                    Completion)
    ) %>%
    dplyr::mutate(Completion=ifelse(!is.na(CompletionOf),
                                    glue::glue(completion_of_template),
                                    Completion)
    ) %>%
    select(`Gm#`:Completion)

  result <- list(
    data=game_log_data,
    caption="Game Log"
  )
}

streak_summary_data <- function(streak_id, streaks, game_logs) {
    summary_data <-
      streak_game_log(streak_id, streaks, game_logs) %>%
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



