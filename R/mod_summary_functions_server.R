# Top level mod_summary server functions ----
summary_server_streak_summary_data <- function(db_pool, franchises,
                                               selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(db_pool, "game_logs")
  ss_streak_summary_data(lzy_game_logs, franchises,
                                      selected_streak)
}

summary_server_get_streak_game_logs <- function(db_pool, selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(db_pool, "game_logs")
  ss_streak_game_log_data(lzy_game_logs, selected_streak)
}

summary_server_get_streak_standings <- function(db_pool, franchises,
                                                selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))

  # The lazy standings table will generate a bunch of warnings about the
  # decimal column, which we don't care about
  withCallingHandlers({
    lzy_standings <- dplyr::tbl(db_pool, "standings")
    lzy_game_logs <- dplyr::tbl(db_pool, "game_logs")
    ss_get_standings_for_streak(lzy_standings, lzy_game_logs, franchises,
                          selected_streak)
  }, warning = function(wrn) {
    if (stringr::str_starts(wrn$message, "Decimal MySQL")) {
      rlang::cnd_muffle(wrn)
    }
  })
}

summary_server_build_streak_standings_graph <- function(db_pool,
                                                        highlight_colors,
                                                        franchises,
                                                        selected_streak) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))

  # The standings table will generate a bunch of warnings about the
  # decimal column, which we don't care about.
  withCallingHandlers({
    lzy_standings <- dplyr::tbl(db_pool, "standings")
    ss_build_standings_graph(highlight_colors, lzy_standings, franchises,
                             selected_streak)
  }, warning = function(wrn) {
    if (stringr::str_starts(wrn$message, "Decimal MySQL")) {
      rlang::cnd_muffle(wrn)
    }
  })
}

# Internal mod_summary server functions ----

#' ss_streak_summary_data
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
ss_streak_summary_data <- function(lzy_game_logs, franchises, streak) {
  # Get the game logs and compute some summary data.  This is not yet
  # the result we want to return.
  summary_data <-
    ss_streak_get_game_log(lzy_game_logs, streak) %>%
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
    ss_franchises_by_season(summary_data$Year) %>%
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
      `W-L%` = ss_pct_formatter(WinningPct),
      RS = RunsScored,
      RA = RunsAllowed,
      `Pyth%` = ss_pct_formatter(PythagPct)
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

#' ss_streak_get_game_log
#'
#' Filter a game log using a streak (using year, team, and the game index).
#' The input is lazy, and the output is in memory.
#'
#' @param lzy_game_logs Lazy game log
#' @param streak Streak
#'
#' @return filtered game log
ss_streak_get_game_log <- function(lzy_game_logs, streak) {
  lzy_game_logs %>%
    dplyr::filter(Year==local(streak$Year),
                  Team==local(streak$Team),
                  between(GameIndex,
                          local(streak$LoIndex),
                          local(streak$HiIndex))) %>%
    dplyr::collect()
}

#' ss_streak_game_log_data
#'
#' Given a streak -- a list with fields Year, Team, LoIndex and HiIndex --
#' return a list with two elements: `caption`
#' which contains a string like "Game Log", and `data`, which is a table
#' suitable for displaying as a game log
#' @param lzy_game_logs Lazy game logs
#' @param streak streak
#'
#' @return list with `data` and `caption`
ss_streak_game_log_data <- function(lzy_game_logs, streak) {
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
  game_log_data <- ss_streak_get_game_log(lzy_game_logs,
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


#' ss_build_standings_graph
#'
#' Given the streak info, build the standings table for the division
#' and pass it to ss_plot_standings_graph to generate a standings plot
#'
#' @param highlight_colors Colors for highlighting
#' @param lzy_standings Lazy full standings table
#' @param franchises Franchises table
#' @param streak Streak info
#'
#' @return Standings plot
ss_build_standings_graph <- function(highlight_colors, lzy_standings,
                                     franchises, streak) {
  # Get division and teams
  division_teams <- ss_get_division_by_team_year(
    franchises, streak$Team, streak$Year)

  # Filter the standings to just the division (or league, if division is NULL)
  if (is.na(division_teams$division$Division)) {
    lzy_standings <- lzy_standings %>%
      dplyr::filter(Year == local(streak$Year),
                    League == local(division_teams$division$League))
  } else {
    lzy_standings <- lzy_standings %>%
      dplyr::filter(
        Year == local(streak$Year),
        League == local(division_teams$division$League),
        Division == local(division_teams$division$Division)
      )
  }

  # Collect the result and pass it to ss_plot_standings_graph for plotting
  standings <- lzy_standings %>%
    dplyr::collect() %>%
    dplyr::mutate(Date = lubridate::ymd(Date))
  ss_plot_standings_graph(highlight_colors, standings, streak$Team,
                          streak$StartDate, streak$EndDate)
}

#' ss_pct_formatter
#'
#' Formats winning percentages as a string with three digits after the
#' the decimal point and no leading zero, e.g. ".542".
#' Will return "1.000" for a perfect record.
#'
#' @param pct Number to format
#'
#' @return Formatted winning percentage.
ss_pct_formatter <- function(pct) {
  ifelse(pct < 1,
         paste0(".", sprintf("%03d", round(1000 * pct))),
         "1.000"
  )
}

#' ss_plot_standings_graph
#'
#' Plot the standings graph, and highlight the selected team and date
#' range
#'
#' @param highlight_colors Colors for highlighting
#' @param standings Standings to plot
#' @param team TeamID to highlight
#' @param start_date Start of highlight area
#' @param end_date End of highlight area
#'
#' @return Standings plot
ss_plot_standings_graph <- function(highlight_colors, standings, team,
                                    start_date, end_date) {
  # Add GamesAbove to the standings, which will be our y-value
  standings <- standings %>% dplyr::mutate(GamesAbove = Wins - Losses)

  # Set the y-axis limits
  y_min <- min(standings$GamesAbove) - 1
  y_max <- max(standings$GamesAbove) + 1

  # Find the coordinates of the box to highlight (this will be the
  # "rect" annotation in the plot)
  date_before_start <- start_date - 1
  y_range <- standings %>%
    dplyr::filter(Date >= date_before_start, Date <= end_date, Team==team) %>%
    dplyr::pull(GamesAbove) %>% range()
  rect_y_min <- y_range[1] - 1
  rect_y_max <- y_range[2] + 1

  # Now plot the standings
  ggplot2::ggplot(mapping = ggplot2::aes(Date, Wins - Losses, group = Team)) +
    ggplot2::geom_line(data = dplyr::filter(standings, Team != team),
                       color = highlight_colors$base) +
    ggplot2::geom_line(data = dplyr::filter(standings, Team == team),
                       color = highlight_colors$high) +
    ggplot2::annotate("rect", xmin=start_date - 1, xmax=end_date,
                      ymin=rect_y_min, ymax=rect_y_max, alpha=0.1,
                      fill = highlight_colors$medium) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::scale_x_date(minor_breaks=NULL) +
    ggplot2::scale_y_continuous(breaks=0, limits=c(y_min,y_max))
}


#' ss_franchises_by_season
#'
#' Given the franchises table, find all the entries matching a specific year.
#'
#' @param franchises  Franchises table
#' @param year Year
#'
#' @return Matching rows in the franchises table
ss_franchises_by_season <- function(franchises, year) {
  franchises %>% dplyr::filter(FirstSeason <= year &
                                 (FinalSeason >= year | is.na(FinalSeason)))
}

#' ss_get_division_by_team_year
#'
#' Give a TeamID and year, find the division (`League`, `Division` and `Year`)
#' and a table of teams in the division (`TeamID`, `Location`, `Nickname`).
#'
#' @param franchises Franchises table
#' @param team teamID
#' @param year year
#'
#' @return list with `division` and `teams`
ss_get_division_by_team_year <- function(franchises, team, year) {
  season_franchises <- ss_franchises_by_season(franchises, year)
  division <- season_franchises %>%
    dplyr::filter(TeamID==team) %>%
    dplyr::select(League, Division) %>%
    dplyr::mutate(Year=year)
  teams <- season_franchises %>%
    dplyr::right_join(division, by=c("League","Division"),
                      na_matches="na") %>%
    dplyr::select(TeamID, Location, Nickname)
  list(division=division, teams=teams)
}



