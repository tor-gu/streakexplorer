streak_get_related_streak_ids <- function(streak_id, concordances) {
  inner <- concordances %>%
    dplyr::filter(Inner == streak_id) %>%
    dplyr::pull(Outer)
  outer <- concordances %>%
    dplyr::filter(Outer == streak_id) %>%
    dplyr::pull(Inner)
  c(inner, outer) %>% unique()
}

pct_formatter <- function(pct) {
  ifelse(pct < 1,
    paste0(".", sprintf("%03d", round(1000 * pct))),
    "1.000"
  )
}

streak_game_log <- function(streak_id, streaks, game_logs) {
  row <- streaks %>%
    dplyr::filter(StreakId == streak_id) %>%
    head(1)
  if (nrow(row) > 0) {
    game_logs %>%
      dplyr::filter(
        Year == row$Year,
        Team == row$Team,
        GameIndex >= row$LoIndex, GameIndex <= row$HiIndex
      )
  } else {
    game_logs %>% dplyr::filter(FALSE)
  }
}

streak_game_log_data <- function(streak) {
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

  game_log_data <- sql_get_streak_game_log(streak) %>%
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
    #dplyr::mutate(Completion = dplyr::case_when(
    #  !is.na(CompletedOn) ~ glue::glue(completed_on_template),
    #  !is.na(CompletionOf) ~ glue::glue(completion_of_template),
    #  TRUE ~ Completion
    #)) %>%
    # TODO delete
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

  result <- list(
    data = game_log_data,
    caption = "Game Log"
  )
}

streak_summary_data <- function(streak, franchises) {
  summary_data <-
    sql_get_streak_game_log(streak) %>%
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

  franchise_season <- franchises_by_season(
    franchises,
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

streak_get_standings <- function(standings, game_logs, streak, franchises) {
  division <- franchises %>%
    franchises_get_division_by_team_year(streak$Team, streak$Year)
  division_teams <- division$teams %>% dplyr::pull(TeamID)
  division_season_games <-
    game_logs %>%
    dplyr::filter(Year==local(streak$Year), Team %in% division_teams)
  first_game <- division_season_games %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  GameIndex==local(streak$LoIndex)) %>%
    dplyr::as_tibble()
  last_game <-  division_season_games %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  GameIndex==local(streak$HiIndex)) %>%
    dplyr::as_tibble()
  standings_before <-
    standings_get_by_season_game_id(standings, division,
      division_season_games, first_game$SeasonGameId, before=TRUE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID"), copy=TRUE) %>%
    dplyr::arrange(GB)
  standings_after <-
    standings_get_by_season_game_id(standings, division,
      division_season_games, last_game$SeasonGameId, before=FALSE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID"), copy=TRUE) %>%
    dplyr::arrange(GB)
  standings_final <- standings_from_game_logs(division_season_games) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID"), copy=TRUE) %>%
    dplyr::arrange(GB)
  streak_info <- division$division %>%
    dplyr::mutate(Year=local(streak$Year),
                  Team=local(streak$Team),
                  Start=local(first_game$Date),
                  End=local(last_game$Date)) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID"))
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
