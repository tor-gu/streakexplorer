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
    dplyr::right_join(division, by=c("League","Division")) %>%
    dplyr::select(TeamID, Location, Nickname)
  list(division=division, teams=teams)
}

franchises_franchise_ids_to_team_ids <- function(franchises, franchise_ids,
                                                 years) {
  franchises %>%
    dplyr::filter(FranchiseID %in% franchise_ids,
                  years[1] <= FinalSeason | is.na(FinalSeason),
                  years[2] >= FirstSeason) %>%
    dplyr::pull(TeamID)
}

