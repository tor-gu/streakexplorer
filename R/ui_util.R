




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

