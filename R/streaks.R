#' pct_formatter
#'
#' Formats winning percentages as a string with three digits after the
#' the decimal point and no leading zero, e.g. ".542".
#' Will return "1.000" for a perfect record.
#'
#' @param pct Number to format
#'
#' @return Formatted winning percentage.
pct_formatter <- function(pct) {
  ifelse(pct < 1,
         paste0(".", sprintf("%03d", round(1000 * pct))),
         "1.000"
  )
}

#' streaks_get_related_streak_ids
#'
#' Given a streak ID, returns all related streaks in the concordance table --
#' both super-streaks and sub-streaks -- including the streak itself.
#'
#' @param streak_id  Streak ID
#' @param lzy_concordances Lazy conconcrdance table
#'
#' @return vector of related streak IDs.
streaks_get_related_streak_ids <- function(streak_id, lzy_concordances) {
  inner <- lzy_concordances %>%
    dplyr::filter(Inner == streak_id) %>%
    dplyr::pull(Outer)
  outer <- lzy_concordances %>%
    dplyr::filter(Outer == streak_id) %>%
    dplyr::pull(Inner)
  c(inner, outer) %>% unique()
}

#' streaks_game_log_data
#'
#' Given a streak -- a list with fields Year, Team, LoIndex and HiIndex --
#' return a list with with elements, return a list with two elements: `caption`
#' which contains a string like "Game Log", and `data`, which is a table
#' suitable for displaying as a game log
#' @param lzy_game_logs
#' @param streak
#'
#' @return
streaks_game_log_data <- function(lzy_game_logs, streak) {
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

  game_log_data <- streaks_get_game_log(lzy_game_logs, streak) %>%
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

  list(
    data = game_log_data,
    caption = "Game Log"
  )
}

streaks_summary_data <- function(lzy_game_logs, lzy_franchises, streak) {
  summary_data <-
    streaks_get_game_log(lzy_game_logs, streak) %>%
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

  franchise_season <- franchises_by_season_lzy(
    lzy_franchises,
    summary_data$Year
  ) %>%
    dplyr::filter(TeamID == local(summary_data$Team)) %>%
    dplyr::collect()

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

  caption_pattern <- paste0(
    "{summary_data$Year} ",
    "{franchise_season$Location} {franchise_season$Nickname}, ",
    "Games {summary_data$FirstGameNumber}-{summary_data$LastGameNumber}"
  )
  caption <- glue::glue(caption_pattern)
  list(data = data, caption = caption)
}

streaks_get_standings <- function(lzy_standings, lzy_game_logs,
                                  lzy_franchises, streak) {
  division <- lzy_franchises %>%
    franchises_get_division_by_team_year_lzy(streak$Team, streak$Year)
  division_teams <- division$lzy_teams %>% dplyr::pull(TeamID)
  lzy_division_season_games <-
    lzy_game_logs %>%
    dplyr::filter(Year==local(streak$Year), Team %in% division_teams)
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
  standings_before <-
    standings_get_by_season_game_id(lzy_standings,
                                    division,
                                    lzy_division_season_games,
                                    first_game$SeasonGameId,
                                    before=TRUE) %>%
    dplyr::left_join(division$lzy_teams, by=c("Team"="TeamID"), copy=TRUE) %>%
    dplyr::arrange(GB) %>%
    dplyr::collect()
  standings_after <-
    standings_get_by_season_game_id(lzy_standings,
                                    division,
                                    lzy_division_season_games,
                                    last_game$SeasonGameId,
                                    before=FALSE) %>%
    dplyr::left_join(division$lzy_teams, by=c("Team"="TeamID"), copy=TRUE) %>%
    dplyr::arrange(GB) %>%
    dplyr::collect()
  standings_final <- standings_from_game_logs(lzy_division_season_games) %>%
    dplyr::left_join(division$lzy_teams, by=c("Team"="TeamID"), copy=TRUE) %>%
    dplyr::arrange(GB) %>%
    dplyr::collect()
  streak_info <- division$lzy_division %>%
    dplyr::mutate(Year=local(streak$Year),
                  Team=local(streak$Team),
                  Start=local(first_game$Date),
                  End=local(last_game$Date)) %>%
    dplyr::left_join(division$lzy_teams, by=c("Team"="TeamID")) %>%
    dplyr::collect()
  list(
    streak_info=streak_info,
    standings_before=standings_before,
    standings_final=standings_final,
    standings_after=standings_after
  )
}

streaks_get_max_rank_simple <- function(lzy_streaks, min_year, max_year,
                                        teams) {
  lzy_streaks %>%
    dplyr::filter(Team %in% teams,
                  dplyr::between(Year, min_year, max_year)) %>%
    dplyr::collect() %>%
    dplyr::group_by(IntensityLevel) %>%
    dplyr::arrange(Rank) %>%
    dplyr::mutate(rn=dplyr::row_number()) %>%
    dplyr::filter(rn==10) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(max_rank=max(Rank)) %>%
    dplyr::pull(max_rank)
}

streaks_get_max_rank_by_sampling <- function(lzy_streaks, min_year, max_year,
                                             teams, levels, scaling) {
  initial_max_rank <- lzy_streaks %>%
    dplyr::filter(IntensityLevel %in% levels) %>%
    streaks_get_max_rank_simple(min_year, max_year, teams) * scaling

  lzy_streaks %>%
    dplyr::filter(Rank <= initial_max_rank) %>%
    streaks_get_max_rank_simple(min_year, max_year, teams)
}

streaks_get_max_rank <- function(min_year, max_year, teams, hot) {
  # TODO get the streaks as a param
  table <- ifelse(hot, "hot_streaks", "cold_streaks")
  lzy_streaks <- dplyr::tbl(se_pool, table)
  season_count <- (max_year - min_year + 1) * length(teams)
  if (season_count < 25)
    streaks_get_max_rank_simple(lzy_streaks, min_year, max_year, teams)
  else if (season_count < 750) {
    streaks_get_max_rank_by_sampling(lzy_streaks, min_year, max_year,
                                     teams, 1:4 * 20, 3/2)
  } else {
    streaks_get_max_rank_by_sampling(lzy_streaks, min_year, max_year,
                                     teams, 1:4 * 20, 4/3)
  }
}

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

streaks_get_game_log <- function(lzy_game_logs, streak) {
  lzy_game_logs %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  between(GameIndex,
                          local(streak$LoIndex),
                          local(streak$HiIndex))) %>%
    dplyr::collect()
}
