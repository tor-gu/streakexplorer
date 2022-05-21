#' ps_streaks_get_max_rank
#'
#' Given a year range and a list of teams and a value n, find an
#' estimate of the maximum of the nth highest rank over all intensity levels.
#'
#' This function is a wrapper around `ps_streaks_get_max_rank_simple` and
#' `ps_streaks_get_max_rank_by_sampling` and will decide the appropriate
#' function and parameters to use based on the number of years and
#' the number of teams.
#'
#' @param lzy_streaks Lazy streaks table
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#' @param hot If `TRUE` use hot streaks, otherwise cold streaks
#'
#' @return Max value estimate
ps_streaks_get_max_rank <- function(lzy_streaks, n, min_year, max_year, teams,
                                    hot) {
  season_count <- (max_year - min_year + 1) * length(teams)
  if (season_count < 25)
    ps_streaks_get_max_rank_simple(lzy_streaks, n, min_year, max_year, teams)
  else if (season_count < 750) {
    ps_streaks_get_max_rank_by_sampling(lzy_streaks, n, min_year, max_year,
                                        teams, 1:4 * 20, 3/2)
  } else {
    ps_streaks_get_max_rank_by_sampling(lzy_streaks, n, min_year, max_year,
                                        teams, 1:4 * 20, 4/3)
  }
}

#' ps_streaks_get_max_rank_simple
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
ps_streaks_get_max_rank_simple <- function(lzy_streaks, n, min_year, max_year,
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

#' ps_streaks_get_max_rank_by_sampling
#'
#' Give an estimate of the rank returned by `ps_streaks_get_max_rank_simple`
#' using this method:
#' * First: Apply the algorithm of `ps_streaks_get_max_rank_simple` to a limited
#' set of intensity levels (e.g. `c(25,50,75)` instead of `1:101`).
#' * Second, increase the returned rank and increase it by a scaling factor
#' (e.g. `1.5`).
#' * Third, restrict the full streaks table to `Rank` values below the scaled
#' initial estimate.
#' * Finally, apply `ps_streaks_get_max_rank_simple` to the restricted streak
#' table, this time across all intensity levels.
#'
#' Notes:
#' * This estimate will always be less than or equal to the true value.
#' * This function calls `ps_streaks_get_max_rank_simple` twice, but each time
#' with a filter applied to the `lzy_streaks_tbl`.  It is less efficient
#' than `ps_streaks_get_max_rank_simple` on smaller datasets, but much faster
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
ps_streaks_get_max_rank_by_sampling <- function(lzy_streaks, n, min_year,
                                                max_year, teams, levels,
                                                scaling) {
  # Get the max over the IntensityLevel sample space, and scale the result
  # using the scaling factor
  initial_max_rank <- lzy_streaks %>%
    dplyr::filter(IntensityLevel %in% levels) %>%
    ps_streaks_get_max_rank_simple(n, min_year, max_year, teams) * scaling

  # Now get the max over all intensity levels, restricted by the scaled
  # sample value.
  lzy_streaks %>%
    dplyr::filter(Rank <= initial_max_rank) %>%
    ps_streaks_get_max_rank_simple(n, min_year, max_year, teams)
}
