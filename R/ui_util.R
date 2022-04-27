#' ui_filter_by_year
#'
#' Filter the franchises table by year.
#'
#' @param franchises  Franchises table
#' @param year Year
#'
#' @return Filtered franchises table
ui_filter_by_year <- function(franchises, year) {
  franchises %>% dplyr::filter(
    FirstSeason <= year & (FinalSeason >= year | is.na(FinalSeason))
  )
}

#' ui_filter_by_years
#'
#' Filter the franchises table by a set of years. Return franchises matching
#' any year in this list.
#'
#' @param franchises  Franchises table
#' @param years vector of years.
#'
#' @return Filtered franchises table
ui_filter_by_years <- function(franchises, years) {
  purrr::map(years, function(year) ui_filter_by_year(franchises, year)) %>%
    data.table::rbindlist() %>%
    unique()
}

#' ui_filter_by_league
#'
#' Filter the franchises table by leagues
#'
#' @param franchises Franchises table
#' @param leagues vector of leagues
#'
#' @return Filtered franchises table
ui_filter_by_league <- function(franchises, leagues) {
  franchises %>% dplyr::filter(League %in% leagues)
}


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

ui_filter_by_league_divisions <- function(franchises, league_divisions) {
  purrr::map(
    league_divisions, ~ ui_filter_by_league_division(franchises, .)
  ) %>%
    data.table::rbindlist() %>%
    unique()
}

ui_truncate_years <- function(franchises, years) {
  franchises %>% dplyr::mutate(
    FirstSeason = pmax(FirstSeason, min(years)),
    FinalSeason = pmin(max(years), FinalSeason, na.rm = TRUE)
  )
}

ui_get_divisions <- function(franchises) {
  franchises %>%
    dplyr::group_by(League, Division) %>%
    dplyr::summarise(
      FirstSeason = min(FirstSeason), FinalSeason = max(FinalSeason),
      .groups = "drop"
    )
}

ui_division_as_choice_value <- function(league, division) {
  if (is.na(division)) {
    glue::glue("{league}_None")
  } else {
    glue::glue("{league}_{division}")
  }
}

ui_division_choice_value_as_league_and_division <- function(choice_value) {
  split_value <- as.list(stringr::str_split(choice_value, "_", simplify = TRUE))
  names(split_value) <- c("league", "division")
  split_value$division <- ifelse(split_value$division == "None", NA,
    split_value$division
  )
  split_value
}

ui_division_choice_values_as_league_and_division_list <- function(choice_values) {
  purrr::map(choice_values, ui_division_choice_value_as_league_and_division)
}

ui_division_as_choice_label <- function(league, division, first_year = NULL,
                                     final_year = NULL,
                                     min_first_year = NULL,
                                     max_final_year = NULL) {
  if ((is.null(first_year) & is.null(final_year)) ||
    (first_year == min_first_year & final_year == max_final_year)) {
    year_part <- ""
  } else {
    year_part <- glue::glue(" ({first_year}-{final_year})", .null = "")
  }
  if (is.na(division)) {
    division_part <- "No Division"
  } else {
    division_part <- division
  }
  glue::glue("{league} {division_part}{year_part}")
}

ui_generate_league_division_selection <- function(division_table, league) {
  league_divisions <- division_table %>%
    dplyr::filter(League == league)
  choices <- purrr::pmap(
    league_divisions,
    function(League, Division, ...) {
      ui_division_as_choice_value(League, Division)
    }
  )
  if (torgutil::tbl_is_column_value_unique(league_divisions, FirstSeason) &&
    torgutil::tbl_is_column_value_unique(league_divisions, FinalSeason)) {
    names(choices) <- purrr::pmap(
      league_divisions, function(League, Division, FirstSeason, FinalSeason) {
        ui_division_as_choice_label(League, Division)
      }
    )
  } else {
    min_first_season <- min(division_table$FirstSeason)
    max_final_season <- max(division_table$FinalSeason)
    names(choices) <- purrr::pmap(
      league_divisions, function(League, Division, FirstSeason, FinalSeason) {
        ui_division_as_choice_label(
          League, Division, FirstSeason, FinalSeason,
          min_first_season, max_final_season
        )
      }
    )
  }
  choices
}

ui_generate_division_selection <- function(division_table) {
  leagues <- division_table %>%
    dplyr::pull(League) %>%
    unique()
  # We need the treat the single-league case differently than the multi-league
  # case: With a single league, we just return a list of choices.  But if there
  # are multiple leagues, we want to return a list-of-lists, so that we have
  # divisions.
  if (length(leagues) == 1) {
    ui_generate_league_division_selection(division_table, leagues)
  } else {
    result <- purrr::map(
      leagues,
      ~ ui_generate_league_division_selection(division_table, .)
    )
    names(result) <- glue::glue("{leagues} Divisions")
    result
  }
}

ui_generate_team_selection <- function(team_table) {
  team_table %>%
    dplyr::arrange(desc(FirstSeason)) %>%
    dplyr::select(FranchiseID, Nickname) %>%
    unique() %>%
    dplyr::group_by(FranchiseID) %>%
    dplyr::summarise(Nicknames = stringr::str_c(Nickname, collapse = "/")) %>%
    torgutil::tbl_as_named_list(FranchiseID, Nicknames)
}

ui_build_divisions_choices <- function(franchises, years, leagues) {
  franchises %>%
    ui_filter_by_years(years) %>%
    ui_filter_by_league(leagues) %>%
    ui_truncate_years(years) %>%
    ui_get_divisions() %>%
    ui_generate_division_selection()
}

ui_build_teams_choices <- function(franchises, years, league_divisions) {
  result <- franchises %>%
    ui_filter_by_years(years) %>%
    ui_filter_by_league_divisions(league_divisions) %>%
    ui_truncate_years(years) %>%
    ui_generate_team_selection()
  result
}

ui_get_updated_division_selection <- function(division_choices, input_divisions,
                                           input_divisions_all) {
  # If it makes sense, keep previous division selections
  if (input_divisions_all) {
    unlist(division_choices)
  } else if (all(input_divisions %in% unlist(division_choices))) {
    input_divisions
  } else {
    # Some previously selected divsions don't exist anymore --
    # default back to all choices in this case.
    unlist(division_choices)
  }
}

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

