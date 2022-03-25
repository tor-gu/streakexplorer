franchises_by_season <- function(franchises, year) {
  franchises %>% dplyr::filter(FirstSeason <= year &
    (FinalSeason >= year | is.na(FinalSeason)))
}

franchises_get_division_by_team_year <- function(franchises, team, year) {
  season_franchises <- franchises_by_season(franchises, year)
  division <- season_franchises %>%
    dplyr::filter(TeamID==team) %>%
    dplyr::select(League, Division) %>%
    dplyr::mutate(Year=year)
  teams <- season_franchises %>%
    dplyr::right_join(division) %>%
    dplyr::select(TeamID, Location, Nickname)
  list(division=division, teams=teams)
}

