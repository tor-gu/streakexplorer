franchises_by_season <- function(franchises, year) {
  franchises %>% dplyr::filter(FirstSeason <= year &
    (FinalSeason >= year | is.na(FinalSeason)))
}

franchises_get_division_by_team_year <- function(franchises, team, year) {
  season_franchises <- franchises_by_season(franchises, year)
  division <- season_franchises %>%
    dplyr::filter(TeamID==team) %>%
    dplyr::select(League, Division)
  teams <- season_franchises %>%
    dplyr::right_join(division) %>%
    dplyr::select(TeamID, Location, Nickname)
  list(division=division, teams=teams)
}

# TODO maybe delete
franchises_by_seasons <- function(franchises, years) {
  purrr::map(years, function(year) franchises_by_season(franchises, year)) %>%
    data.table::rbindlist() %>%
    tibble::as_tibble() %>%
    unique()
}

