summary_server_streak_summary_data <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
  summary_server_streaks_summary_data(lzy_game_logs, franchises,
                                      selected_streak)
}

#' summary_server_streaks_summary_data
#'
#' Build streak summary data from a streak.  The will be two data elements
#' to the return values:  `data` will be a single-row summary table suitable
#' for displaying, and `caption` will contain a string like
#' "2004 Cincinnati Reds, Games 100-104"
#'
#' @param lzy_game_logs Lazy game logs table
#' @param franchises franchise table
#' @param streak streak
#'
#' @return list with `data` and `caption`
summary_server_streaks_summary_data <- function(lzy_game_logs, franchises,
                                                streak) {
  # Get the game logs and compute some summary data.  This is not yet
  # the result we want to return.
  summary_data <-
    summary_server_streaks_get_game_log(lzy_game_logs, streak) %>%
    dplyr::summarise(
      Team            = unique(Team),
      Year            = unique(Year),
      FirstGameNumber = min(GameNumber),
      FirstGameDate   = min(Date),
      LastGameNumber  = max(GameNumber),
      LastGameDate    = max(Date),
      Games           = sum(!is.na(Result)),
      Wins            = sum(Result == "W", na.rm = TRUE),
      Losses          = sum(Result == "L", na.rm = TRUE),
      Ties            = sum(Result == "T", na.rm = TRUE),
      RunsScored      = sum(RunsFor),
      RunsAllowed     = sum(RunsAgainst),
      WinningPct      = (Wins + Ties / 2) / Games,
      PythagPct       = RunsScored^2 / (RunsScored^2 + RunsAllowed^2),
      PythagWins      = round(PythagPct * Games, digits = 1),
      PythagLosses    = Games - PythagWins,
      HomeGames       = sum(AtHome == TRUE),
      AwayGames       = sum(AtHome == FALSE)
    )

  # Look up this team's season in the franchises table.
  franchise_season <- franchises %>%
    franchises_by_season(summary_data$Year) %>%
    dplyr::filter(TeamID == local(summary_data$Team))

  # Now compute the displayable summary
  data <-
    summary_data %>%
    dplyr::mutate(
      Start = glue::glue("{lubridate::month(FirstGameDate)}/{lubridate::mday(FirstGameDate)}"),
      End = glue::glue("{lubridate::month(LastGameDate)}/{lubridate::mday(LastGameDate)}"),
      Dates = glue::glue(ifelse(FirstGameDate == LastGameDate, "{Start}", "{Start} - {End}")),
      Record = ifelse(Ties == 0,
                      glue::glue("{Wins}-{Losses}"),
                      glue::glue("{Wins}-{Losses}-{Ties}")
      ),
      `W-L%` = pct_formatter(WinningPct),
      RS = RunsScored, RA = RunsAllowed, `Pyth%` = pct_formatter(PythagPct)
    ) %>%
    dplyr::select(Dates:`Pyth%`)

  # Build the caption
  caption_pattern <- paste0(
    "{summary_data$Year} ",
    "{franchise_season$Location} {franchise_season$Nickname}, ",
    "Games {summary_data$FirstGameNumber}-{summary_data$LastGameNumber}"
  )
  caption <- glue::glue(caption_pattern)

  # Assemble the result
  list(data = data, caption = caption)
}

#' summary_server_streaks_get_game_log
#'
#' Filter a game log using a streak (using year, team, and the game index).
#' The input is lazy, and the output is in memory.
#'
#' @param lzy_game_logs Lazy game log
#' @param streak Streak
#'
#' @return filtered game log
summary_server_streaks_get_game_log <- function(lzy_game_logs, streak) {
  lzy_game_logs %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  between(GameIndex,
                          local(streak$LoIndex),
                          local(streak$HiIndex))) %>%
    dplyr::collect()
}

#' summary_server_streaks_game_log_data
#'
#' Given a streak -- a list with fields Year, Team, LoIndex and HiIndex --
#' return a list with two elements: `caption`
#' which contains a string like "Game Log", and `data`, which is a table
#' suitable for displaying as a game log
#' @param lzy_game_logs Lazy game logs
#' @param streak streak
#'
#' @return list with `data` and `caption`
summary_server_streaks_game_log_data <- function(lzy_game_logs, streak) {
  # Templates to use when computing the summary data
  date_template <- "{lubridate::month(Date)}/{lubridate::mday(Date)}"
  completed_on_template <-
    paste0(
      "Completed {lubridate::month(CompletedOn)}/",
      "{lubridate::mday(CompletedOn)}, {GameResult} ",
      "{GameRunsFor}-{GameRunsAgainst}"
    )
  completion_of_template <-
    paste0(
      "Final score {GameRunsFor}-{GameRunsAgainst}, began ",
      "{lubridate::month(CompletionOf)}/{lubridate::mday(CompletionOf)}"
    )

  # Get the game logs and compute summary data
  game_log_data <- summary_server_streaks_get_game_log(lzy_game_logs,
                                                       streak) %>%
    dplyr::mutate(
      GameResult = dplyr::case_when(
        GameRunsFor > GameRunsAgainst ~ "W",
        GameRunsFor < GameRunsAgainst ~ "L",
        GameRunsFor == GameRunsAgainst ~ "T"
      ),
      `Gm#` = GameNumber,
      Dat = glue::glue(date_template),
      Opp = ifelse(AtHome,
                   OpponentTeam,
                   glue::glue("@{OpponentTeam}")
      ),
      `W/L` = Result,
      RS = RunsFor,
      RA = RunsAgainst,
      Completion = NA
    ) %>%
    dplyr::mutate(Completion = ifelse(!is.na(CompletedOn),
                                      glue::glue(completed_on_template),
                                      Completion
    )) %>%
    dplyr::mutate(Completion = ifelse(!is.na(CompletionOf),
                                      glue::glue(completion_of_template),
                                      Completion
    )) %>%
    dplyr::select(`Gm#`:Completion) %>%
    dplyr::rename(Date=Dat)

  # Assemble the result
  list(
    data = game_log_data,
    caption = "Game Log"
  )
}

summary_server_get_streak_game_logs <- function(selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
  summary_server_streaks_game_log_data(lzy_game_logs, selected_streak)
}

summary_server_get_streak_standings <- function(franchises, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))

  # The lazy standings table will generate a bunch of warnings about the
  # decimal column, which we don't care about
  withCallingHandlers({
    lzy_standings <- dplyr::tbl(se_pool, "standings")
    lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
    streaks_get_standings(lzy_standings, lzy_game_logs, fraFnchises,
                          selected_streak)
  }, warning = function(wrn) {
    if (stringr::str_starts(wrn$message, "Decimal MySQL")) {
      rlang::cnd_muffle(wrn)
    }
  })
}

summary_server_build_streak_standings_graph <- function(franchises,
                                                        selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))

  # The standings table will generate a bunch of warnings about the
  # decimal column, which we don't care about.
  withCallingHandlers({
    lzy_standings <- dplyr::tbl(se_pool, "standings")
    plot_build_standings_graph(lzy_standings, franchises, selected_streak)
  }, warning = function(wrn) {
    if (stringr::str_starts(wrn$message, "Decimal MySQL")) {
      rlang::cnd_muffle(wrn)
    }
  })
}


