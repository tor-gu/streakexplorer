% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_filter_functions_ui.R
\name{filter_ui_get_updated_teams_selection}
\alias{filter_ui_get_updated_teams_selection}
\title{filter_ui_get_updated_teams_selection}
\usage{
filter_ui_get_updated_teams_selection(
  teams_choices,
  input_teams,
  input_teams_all
)
}
\arguments{
\item{teams_choices}{Currently available choices (as named list)}

\item{input_teams}{Currently selected (vector of values)}

\item{input_teams_all}{The value of the 'All' checkbox}
}
\value{
Updated selection
}
\description{
This function is for updating a the user-selected teams after the
a new filter has been applied to the available choices. The rule is:
\itemize{
\item If the 'All' checkbox is selected, the new selection should be all
available teams.
\item If 'All' is not checked, but some previously selected choices have
disappeared, the new new selection should be all available teams.
\item Otherwise, the already selected choices should be retained.
}
}
