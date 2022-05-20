



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

