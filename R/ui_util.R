

#' ui_filter_by_league_division
#'
#' Filter franchises table by league and division.  `league_division` should
#' be a list with `league` and `division`.
#'
#' @param franchises Franchises table
#' @param league_division League and division
#'
#' @return Filtered franchises table
ui_filter_by_league_division <- function(franchises, league_division) {
  if (is.na(league_division$division)) {
    franchises %>% dplyr::filter(
      League == league_division$league,
      is.na(Division))
  } else {
    franchises %>% dplyr::filter(
      League == league_division$league,
      Division == league_division$division
    )
  }
}

#' ui_filter_by_league_divisions
#'
#' Filter franchises table by a list of leagues and divisions.
#' `league_divisions` should be a list of lists, each with `league` and
#' `division`.
#'
#' @param franchises Franchises table
#' @param league_divisions List of lists
#'
#' @return Filtered franchises table
ui_filter_by_league_divisions <- function(franchises, league_divisions) {
  purrr::map(
    league_divisions, ~ ui_filter_by_league_division(franchises, .)
  ) %>%
    data.table::rbindlist() %>%
    unique()
}



#' ui_generate_team_selection
#'
#' Given a (filtered) franchises table, generate a named list of teams, with
#' the distinct FranchiseIDs as values.  The name of each value will be a
#' combination of all nicknames associated to the FranchiseID -- e.g.
#' "Orioles/Browns"="BAL".  For each franchise, arrange it so the the nicknames
#' are arranged from most recent to least recent.
#'
#' @param franchises Franchises table
#'
#' @return Named list of franchise IDs.
ui_generate_team_selection <- function(franchises) {
  franchises %>%
    dplyr::arrange(desc(FirstSeason)) %>%
    dplyr::select(FranchiseID, Nickname) %>%
    unique() %>%
    dplyr::group_by(FranchiseID) %>%
    dplyr::summarise(Nicknames = stringr::str_c(Nickname, collapse = "/")) %>%
    torgutil::tbl_as_named_list(FranchiseID, Nicknames)
}

#' ui_build_teams_choices
#'
#' Given the franchises table, and a years and league/division filter,
#' generate a named list of FranchiseIDs, where the names of the
#' franchises is based on the teams Nicknames, like "Red Sox" or
#' "Rays/Devil Rays".
#'
#' @param franchises  Franchises table
#' @param years  Years filter
#' @param league_divisions List of lists with `league` and `division`
#'
#' @return Named list for the UI
ui_build_teams_choices <- function(franchises, years, league_divisions) {
  result <- franchises %>%
    filter_ui_filter_by_years(years) %>%
    ui_filter_by_league_divisions(league_divisions) %>%
    filter_ui_truncate_years(years) %>%
    ui_generate_team_selection()
  result
}

#' ui_get_updated_division_selection
#'
#' This function is for updating a the user-selected divisions after the
#' a new filter has been applied to the available choices. The rule is:
#' * If the 'All' checkbox is selected, the new selection should be all
#' available divisions
#' * If 'All' is not checked, but some previously selected choices have
#' disappeared, the new new selection should be all available divisions
#' * Otherwise, the already selected choices should be retained.
#'
#' @param division_choices Currently available choices (as named list)
#' @param input_divisions Currently selected (vector of values)
#' @param input_divisions_all The value of the 'All' checkbox
#'
#' @return Updated selection
ui_get_updated_division_selection <- function(division_choices, input_divisions,
                                              input_divisions_all) {
  # If it makes sense, keep previous division selections
  if (input_divisions_all) {
    unlist(division_choices)
  } else if (all(input_divisions %in% unlist(division_choices))) {
    input_divisions
  } else {
    # Some previously selected divisions don't exist anymore --
    # default back to all choices in this case.
    unlist(division_choices)
  }
}

#' ui_get_updated_teams_selection
#'
#' This function is for updating a the user-selected teams after the
#' a new filter has been applied to the available choices. The rule is:
#' * If the 'All' checkbox is selected, the new selection should be all
#' available teams.
#' * If 'All' is not checked, but some previously selected choices have
#' disappeared, the new new selection should be all available teams.
#' * Otherwise, the already selected choices should be retained.
#'
#' @param teams_choices Currently available choices (as named list)
#' @param input_teams Currently selected (vector of values)
#' @param input_teams_all The value of the 'All' checkbox
#'
#' @return Updated selection
ui_get_updated_teams_selection <- function(teams_choices, input_teams,
                                           input_teams_all) {
  if (input_teams_all) {
    # We want all teams
    selected <- unlist(teams_choices)
  }
  else if (all(input_teams %in% unlist(teams_choices))) {
    # If it makes sense, keep all previously selected teams
    selected <- input_teams
  } else {
    # Some previously selected teams are not available with the current
    # options, so default back to all teams.
    selected <- unlist(teams_choices)
  }
}

