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



#' streaks_get_standings
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
streaks_get_standings <- function(lzy_standings, lzy_game_logs,
                                  franchises, streak) {
  # Get the division
  division <- franchises %>%
    franchises_get_division_by_team_year(streak$Team, streak$Year)
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
    standings_get_by_season_game_id(lzy_standings,
                                    lzy_division_season_games,
                                    division$division,
                                    first_game$SeasonGameId,
                                    before=TRUE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)
  # Calculate the standings after
  standings_after <-
    standings_get_by_season_game_id(lzy_standings,
                                    lzy_division_season_games,
                                    division$division,
                                    last_game$SeasonGameId,
                                    before=FALSE) %>%
    dplyr::left_join(division$teams, by=c("Team"="TeamID")) %>%
    dplyr::select(Team, Wins,Losses, Ties, GB, Location, Nickname) %>%
    dplyr::arrange(GB)
  # Look up the final standings
  standings_final <- standings_get_final_standings(lzy_standings,
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

#' streaks_get_max_rank_simple
#'
#' Given a year range and a list of teams and a value n, find the maximum
#  of the nth highest rank over all intensity levels.
#'
#' Note: This function is inefficient when the number number of intensity
#' levels, teams and years is large.
#'
#' @param lzy_streaks Lazy streaks table (possibly with a filter)
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#'
#' @return Maximum value
streaks_get_max_rank_simple <- function(lzy_streaks, n, min_year, max_year,
                                        teams) {
  lzy_streaks %>%
    dplyr::filter(Team %in% teams,
                  dplyr::between(Year, min_year, max_year)) %>%
    dplyr::collect() %>%
    dplyr::group_by(IntensityLevel) %>%
    dplyr::arrange(Rank) %>%
    dplyr::mutate(rn=dplyr::row_number()) %>%
    dplyr::filter(rn==n) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(max_rank=max(Rank)) %>%
    dplyr::pull(max_rank)
}

#' streaks_get_max_rank_by_sampling
#'
#' Give an estimate of the rank returned by `streaks_get_max_rank_simple`
#' using this method:
#' * First: Apply the algorithm of `streaks_get_max_rank_simple` to a limited
#' set of intensity levels (e.g. `c(25,50,75)` instead of `1:101`).
#' * Second, increase the returned rank and increase it by a scaling factor
#' (e.g. `1.5`).
#' * Third, restrict the full streaks table to `Rank` values below the scaled
#' initial estimate.
#' * Finally, apply `streaks_get_max_rank_simple` to the restricted streak
#' table, this time across all intensity levels.
#'
#' Notes:
#' * This estimate will always be less than or equal to the true value.
#' * This function calls `streaks_get_max_rank_simple` twice, but each time
#' with a filter applied to the `lzy_streaks_tbl`.  It is less efficient
#' than `streaks_get_max_rank_simple` on smaller datasets, but much faster
#' on larger datasets.
#' * Increasing the scaling factor or the intensity sample space increases
#' the accuracy at the cost of speed.
#' * Smaller datasets require larger scaling factors, and larger datasets
#' require smaller scaling factors.
#'
#' @param lzy_streaks Lazy streaks table
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#' @param levels Intensity levels for the sampling, e.g. `c(25,50,75)`
#' @param scaling Scaling factor, e.g. `1.5`
#'
#' @return Estimate of maximum value
streaks_get_max_rank_by_sampling <- function(lzy_streaks, n, min_year,
                                             max_year, teams, levels,
                                             scaling) {
  # Get the max over the IntensityLevel sample space, and scale the result
  # using the scaling factor
  initial_max_rank <- lzy_streaks %>%
    dplyr::filter(IntensityLevel %in% levels) %>%
    streaks_get_max_rank_simple(n, min_year, max_year, teams) * scaling

  # Now get the max over all intensity levels, restricted by the scaled
  # sample value.
  lzy_streaks %>%
    dplyr::filter(Rank <= initial_max_rank) %>%
    streaks_get_max_rank_simple(n, min_year, max_year, teams)
}

#' streaks_get_max_rank
#'
#' Given a year range and a list of teams and a value n, find an
#' estimate of the maximum of the nth highest rank over all intensity levels.
#'
#' This function is a wrapper around `streaks_get_max_rank_simple` and
#' `streaks_get_max_rank_by_sampling` and will decide the appropriate
#' function and parameters to use based on the number of years and
#' the number of teams.
#'
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#' @param hot If `TRUE` use hot streaks, otherwise cold streaks
#'
#' @return Max value estimate
streaks_get_max_rank <- function(n, min_year, max_year, teams, hot) {
  lzy_streaks <- lzy_streaks(hot)
  season_count <- (max_year - min_year + 1) * length(teams)
  if (season_count < 25)
    streaks_get_max_rank_simple(lzy_streaks, n, min_year, max_year, teams)
  else if (season_count < 750) {
    streaks_get_max_rank_by_sampling(lzy_streaks, n, min_year, max_year,
                                     teams, 1:4 * 20, 3/2)
  } else {
    streaks_get_max_rank_by_sampling(lzy_streaks, n, min_year, max_year,
                                     teams, 1:4 * 20, 4/3)
  }
}

#' streaks_get_intensity_range
#'
#' Use the streaks table to get the the minimum and maximum intensity
#' levels in the DB.  The year is specified to make the query quicker.
#'
#' @param lzy_streaks  Lazy streaks table
#' @param year  Year to query.
#'
#' @return Vector with min and max intensity levels, e.g. `c(1,101)`
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

