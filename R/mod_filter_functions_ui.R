#' filter_ui_division_choice_values_as_league_and_division_list
#'
#' Convert a vector of strings of the form "XXX_YYY" into a list of lists of the
#' form `list(league="XXX", division="YYY")`.  If "YYY" is "None", then
#' `division` will be `NA`.
#'
#' @param choice_values Vector of strings of the from "XXX_YYY"
#'
#' @return List of lists with `league` and `division`
filter_ui_division_choice_values_as_league_and_division_list <-
  function(choice_values) {
    purrr::map(choice_values,
               filter_ui_division_choice_value_as_league_and_division)
  }

#' filter_ui_division_choice_value_as_league_and_division
#'
#' Convert a string of the form "XXX_YYY" into a list of the form
#' `list(league="XXX", division="YYY")`.  If "YYY" is "None", then `division`
#' will be `NA`.
#'
#' @param choice_value  String of the form "XXX_YYY"
#'
#' @return list with `league` and `division`
filter_ui_division_choice_value_as_league_and_division <-
  function(choice_value) {
    split_value <-
      as.list(stringr::str_split(choice_value, "_", simplify = TRUE))
    names(split_value) <- c("league", "division")
    split_value$division <- ifelse(split_value$division == "None", NA,
                                   split_value$division)
    split_value
  }
