filter_by_year <- function(franchises, year) {
  franchises %>% dplyr::filter(
    FirstSeason <= year & (FinalSeason >= year | is.na(FinalSeason))
  )
}

filter_by_years <- function(franchises, years, truncate_years = TRUE) {
  purrr::map(years, function(year) filter_by_year(franchises, year)) %>%
    data.table::rbindlist() %>%
    tibble::as_tibble() %>%
    unique()
}

filter_by_league <- function(franchises, leagues) {
  franchises %>% dplyr::filter(League %in% leagues)
}


filter_by_league_division <- function(franchises, league_division) {
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

filter_by_league_divisions <- function(franchises, league_divisions) {
  # TODO use formula
  purrr::map(
    league_divisions,
    function(ld) filter_by_league_division(franchises, ld)
  ) %>%
    data.table::rbindlist() %>%
    tibble::as_tibble() %>%
    unique()
}

truncate_years <- function(franchises, years) {
  franchises %>% dplyr::mutate(
    FirstSeason = pmax(FirstSeason, min(years)),
    FinalSeason = pmin(max(years), FinalSeason, na.rm = TRUE)
  )
}

get_divisions <- function(franchises) {
  franchises %>%
    dplyr::group_by(League, Division) %>%
    dplyr::summarise(
      FirstSeason = min(FirstSeason), FinalSeason = max(FinalSeason),
      .groups = "drop"
    )
}

get_teams <- function(franchises) {
  franchises %>%
    dplyr::group_by(FranchiseID) %>%
    dplyr::summarise(
      FirstSeason = min(FirstSeason), FinalSeason = max(FinalSeason),
      .groups = "drop"
    )
}

division_as_choice_value <- function(league, division) {
  if (is.na(division)) {
    glue::glue("{league}_None")
  } else {
    glue::glue("{league}_{division}")
  }
}

division_choice_value_as_league_and_division <- function(choice_value) {
  split_value <- as.list(stringr::str_split(choice_value, "_", simplify = TRUE))
  names(split_value) <- c("league", "division")
  split_value$division <- ifelse(split_value$division == "None", NA,
    split_value$division
  )
  split_value
}

division_choice_values_as_league_and_division_list <- function(choice_values) {
  purrr::map(choice_values, ~ division_choice_value_as_league_and_division(.))
}

division_as_choice_label <- function(league, division, first_year = NULL,
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

generate_league_division_selection <- function(division_table, league) {
  league_divisions <- division_table %>%
    dplyr::filter(League == league)
  choices <- purrr::pmap(
    league_divisions,
    function(League, Division, ...) {
      division_as_choice_value(League, Division)
    }
  )
  if (torgutil::tbl_is_column_value_unique(league_divisions, FirstSeason) &&
    torgutil::tbl_is_column_value_unique(league_divisions, FinalSeason)) {
    names(choices) <- purrr::pmap(
      league_divisions, function(League, Division, FirstSeason, FinalSeason) {
        division_as_choice_label(League, Division)
      }
    )
  } else {
    min_first_season <- min(division_table$FirstSeason)
    max_final_season <- max(division_table$FinalSeason)
    names(choices) <- purrr::pmap(
      league_divisions, function(League, Division, FirstSeason, FinalSeason) {
        division_as_choice_label(
          League, Division, FirstSeason, FinalSeason,
          min_first_season, max_final_season
        )
      }
    )
  }
  choices
}

generate_division_selection <- function(division_table) {
  leagues <- division_table %>%
    dplyr::pull(League) %>%
    unique()
  # We need the treat the single-league case differently than the multi-league
  # case: With a single league, we just return a list of choices.  But if there
  # are multiple leagues, we want to return a list-of-lists, so that we have
  # divisions.
  if (length(leagues) == 1) {
    generate_league_division_selection(division_table, leagues)
  } else {
    result <- purrr::map(
      leagues,
      ~ generate_league_division_selection(
        division_table,
        .
      )
    )
    names(result) <- glue::glue("{leagues} Divisions")
    result
  }
}

generate_team_selection <- function(team_table) {
  selection_table <- team_table %>%
    dplyr::arrange(desc(FirstSeason)) %>%
    dplyr::select(FranchiseID, Nickname) %>%
    unique() %>%
    dplyr::group_by(FranchiseID) %>%
    dplyr::summarise(Nicknames = stringr::str_c(Nickname, collapse = "/")) %>%
    torgutil::tbl_as_named_list(FranchiseID, Nicknames)
}

build_divisions_choices <- function(franchises, years, leagues) {
  franchises %>%
    filter_by_years(years) %>%
    filter_by_league(leagues) %>%
    truncate_years(years) %>%
    get_divisions() %>%
    generate_division_selection()
}

build_teams_choices <- function(franchises, years, league_divisions) {
  franchises %>%
    filter_by_years(years) %>%
    filter_by_league_divisions(league_divisions) %>%
    truncate_years(years) %>%
    generate_team_selection()
}
