#' franchises_by_season
#'
#' Given the franchises table, find all the entries matching a specific year.
#'
#' @param franchises  Franchises table
#' @param year Year
#'
#' @return Matching rows in the franchises table
franchises_by_season <- function(franchises, year) {
  franchises %>% dplyr::filter(FirstSeason <= year &
    (FinalSeason >= year | is.na(FinalSeason)))
}

#' franchises_get_division_by_team_year
#'
#' Give a TeamID and year, find the division (`League`, `Division` and `Year`)
#' and a table of teams in the division (`TeamID`, `Location`, `Nickname`).
#'
#' @param franchises Franchises table
#' @param team teamID
#' @param year year
#'
#' @return list with `division` and `teams`
franchises_get_division_by_team_year <- function(franchises, team, year) {
  season_franchises <- franchises_by_season(franchises, year)
  division <- season_franchises %>%
    dplyr::filter(TeamID==team) %>%
    dplyr::select(League, Division) %>%
    dplyr::mutate(Year=year)
  teams <- season_franchises %>%
    dplyr::right_join(division, by=c("League","Division"),
                      na_matches="na") %>%
    dplyr::select(TeamID, Location, Nickname)
  list(division=division, teams=teams)
}

#' franchises_franchise_ids_to_team_ids
#'
#' Find all TeamIDs for the given FranchiseIDs and the year range.
#'
#' @param franchises Franchise table
#' @param franchise_ids vector of FranchiseIDs
#' @param min_year First year
#' @param max_year Final year
#'
#' @return List of TeamIDs
franchises_franchise_ids_to_team_ids <- function(franchises, franchise_ids,
                                                 min_year, max_year) {
  franchises %>%
    dplyr::filter(FranchiseID %in% franchise_ids,
                  min_year <= FinalSeason | is.na(FinalSeason),
                  max_year >= FirstSeason) %>%
    dplyr::pull(TeamID)
}

