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

#' ui_truncate_years
#'
#' Given a franchises table and a set of year, modify rows of the table so that
#' the `FirstSeason` and `FinalSeason` are 'truncated' by the min and max of the
#' years.  For example, if a row has `FinalSeason` `NA` or 2000, and the max
#' value in `years` is 1990, change the `FinalSeason` to 1990.
#'
#' The assumption is that the `franchises` table has already been filtered by
#' the `years` list.
#'
#' @param franchises  Franchises table
#' @param years vector of years.
#'
#' @return Modified franchises table
ui_truncate_years <- function(franchises, years) {
  franchises %>% dplyr::mutate(
    FirstSeason = pmax(FirstSeason, min(years)),
    FinalSeason = pmin(max(years), FinalSeason, na.rm = TRUE)
  )
}

#' ui_get_divisions
#'
#' Given a franchises table (which as already been truncated to to a
#' range of years), return all the league/division combos, as well
#' as the first and final seasons.
#'
#' @param franchises Franchise table
#'
#' @return Table with `League`, `Division`, `FirstSeason`, `FinalSeason`
ui_get_divisions <- function(franchises) {
  franchises %>%
    dplyr::group_by(League, Division) %>%
    dplyr::summarise(
      FirstSeason = min(FirstSeason), FinalSeason = max(FinalSeason),
      .groups = "drop"
    )
}

#' ui_division_as_choice_value
#'
#' Build a string like "AL_West" or "NL_None"
#'
#' @param league League
#' @param division Division, or `NA`
#'
#' @return String of the form League_Division
ui_division_as_choice_value <- function(league, division) {
  if (is.na(division)) {
    glue::glue("{league}_None")
  } else {
    glue::glue("{league}_{division}")
  }
}

#' ui_division_choice_value_as_league_and_division
#'
#' Convert a string of the form "XXX_YYY" into a list of the form
#' `list(league="XXX", division="YYY")`.  If "YYY" is "None", then `division`
#' will be `NA`.
#'
#' @param choice_value  String of the form "XXX_YYY"
#'
#' @return list with `league` and `division`
ui_division_choice_value_as_league_and_division <- function(choice_value) {
  split_value <- as.list(stringr::str_split(choice_value, "_", simplify = TRUE))
  names(split_value) <- c("league", "division")
  split_value$division <- ifelse(split_value$division == "None", NA,
    split_value$division
  )
  split_value
}

#' ui_division_choice_values_as_league_and_division_list
#'
#' Convert a vector of strings of the form "XXX_YYY" into a list of lists of the
#' form `list(league="XXX", division="YYY")`.  If "YYY" is "None", then
#' `division` will be `NA`.
#'
#' @param choice_values Vector of strings of the from "XXX_YYY"
#'
#' @return List of lists with `league` and `division`
ui_division_choice_values_as_league_and_division_list <- function(choice_values) {
  purrr::map(choice_values, ui_division_choice_value_as_league_and_division)
}

#' ui_division_as_choice_label
#'
#' Given a league and division and, optionally, first and final years, as well
#' as a year range, produce strings like this:
#' * "AL East"
#' * "AL East (1990-1995)
#' * "AL No Division"
#' * "AL No Division (1990-1995)"
#' If the first and final year is contained entirely within the year range,
#' then just the league and division will be returned.  But if the min
#' or max year extends beyond the year range, then a range of years will
#' be appended.
#' @param league League
#' @param division Division (or `NA`)
#' @param first_year First year the division is active
#' @param final_year Final year the division is active
#' @param min_first_year First year of the year range
#' @param max_final_year Final year of the year range
#'
#' @return League/Division/Year string
ui_division_as_choice_label <- function(league, division, first_year = NULL,
                                        final_year = NULL,
                                        min_first_year = NULL,
                                        max_final_year = NULL) {
  # The 'year part' of the return value is only included if
  # the first or final year extend outside of the year-range.
  if ((is.null(first_year) & is.null(final_year)) ||
    (first_year == min_first_year & final_year == max_final_year)) {
    year_part <- ""
  } else {
    year_part <- glue::glue(" ({first_year}-{final_year})", .null = "")
  }

  # If the division is NA, use the string "No Division"
  if (is.na(division)) {
    division_part <- "No Division"
  } else {
    division_part <- division
  }

  # Assemble the result
  glue::glue("{league} {division_part}{year_part}")
}

#' ui_generate_league_division_selection
#'
#' Given a division table and a league return a named list of choices, which
#' may look something like this:
#' ```
#' list("AL East (1969-1985)"="AL_East",
#'      "AL West (1980-2021)"="AL_West")
#' ```
#'
#' @param division_table Division table
#' @param league League to filter by
#'
#' @return Named list
ui_generate_league_division_selection <- function(division_table, league) {
  # Filter to just the league we care arout
  league_divisions <- division_table %>%
    dplyr::filter(League == league)

  # The choice values are handled by ui_division_as_choice_value
  choices <- purrr::pmap(
    league_divisions,
    function(League, Division, ...) {
      ui_division_as_choice_value(League, Division)
    }
  )

  # Add the choice names
  if (torgutil::tbl_is_column_value_unique(league_divisions, FirstSeason) &&
    torgutil::tbl_is_column_value_unique(league_divisions, FinalSeason)) {
    # All the year ranges are the same, so we don't need to include those
    names(choices) <- purrr::pmap(
      league_divisions, function(League, Division, FirstSeason, FinalSeason) {
        ui_division_as_choice_label(League, Division)
      }
    )
  } else {
    # There is some variation, so we need to include year ranges in the names
    # (e.g. "AL East (1969-2021)")
    min_first_season <- min(league_divisions$FirstSeason)
    max_final_season <- max(league_divisions$FinalSeason)
    names(choices) <- purrr::pmap(
      league_divisions, function(League, Division, FirstSeason, FinalSeason) {
        ui_division_as_choice_label(
          League, Division, FirstSeason, FinalSeason,
          min_first_season, max_final_season
        )
      }
    )
  }

  # Return the result
  choices
}

#' ui_generate_division_selection
#'
#' Given a division table, generate a choice list for the division selection
#' UI.  If all the divisions are from the same league, there will be an
#' element for each division. If both leagues are represented, then there will
#' be two sublists, labeled "AL Divisions" and "NL Divisions".  (This will
#' generate separators in the UI)
#'
#' @param division_table Division table
#'
#' @return List of choices for the UI
ui_generate_division_selection <- function(division_table) {
  # Get the leagues
  leagues <- division_table %>%
    dplyr::pull(League) %>%
    unique()
  # We need the treat the single-league case differently than the multi-league
  # case: With a single league, we just return a list of choices.  But if there
  # are multiple leagues, we want to return a list-of-lists, so that we have
  # divisions.
  if (length(leagues) == 1) {
    # Single league
    ui_generate_league_division_selection(division_table, leagues)
  } else {
    # Both leagues
    result <- purrr::map(
      leagues,
      ~ ui_generate_league_division_selection(division_table, .)
    )
    # Add the separator labels
    names(result) <- glue::glue("{leagues} Divisions")
    result
  }
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

