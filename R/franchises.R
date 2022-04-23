# TODO converting these to use a non-lazy table.
franchises_by_season_lzy <- function(lzy_franchises, year) {
  lzy_franchises %>% dplyr::filter(FirstSeason <= year &
    (FinalSeason >= year | is.na(FinalSeason)))
}

franchises_get_division_by_team_year_lzy <- function(lzy_franchises, team, year) {
  lzy_season_franchises <- franchises_by_season_lzy(lzy_franchises, year)
  lzy_division <- lzy_season_franchises %>%
    dplyr::filter(TeamID==team) %>%
    dplyr::select(League, Division) %>%
    dplyr::mutate(Year=year)
  lzy_teams <- lzy_season_franchises %>%
    dplyr::right_join(lzy_division, by=c("League","Division"),
                      na_matches="na") %>%
    dplyr::select(TeamID, Location, Nickname)
  list(lzy_division=lzy_division, lzy_teams=lzy_teams)
}

franchises_franchise_ids_to_team_ids <- function(franchises, franchise_ids,
                                                 years) {
  first_year <- years[[1]]
  final_year <- years[[2]]
  franchises %>%
    dplyr::filter(FranchiseID %in% franchise_ids,
                  first_year <= FinalSeason | is.na(FinalSeason),
                  final_year >= FirstSeason) %>%
    dplyr::pull(TeamID)
}

