franchises_by_season <- function(franchises, year) {
  franchises %>% dplyr::filter(FirstSeason <= year &
                                 (FinalSeason >= year | is.na(FinalSeason)))
}

franchises_by_seasons <- function(franchises, years) {
  purrr::map(years, function(year) franchises_by_season(franchises, year)) %>%
    data.table::rbindlist() %>% tibble::as_tibble() %>% unique()
}

franchise_season_name <- function(franchise_ids, years) {
  purrr::cross2(years, franchise_ids) %>%
    purrr::map_dfr(~franchises %>%
                     dplyr::filter(.x[[1]] >= FirstSeason,
                                   .x[[1]] <= FinalSeason | is.na(FinalSeason),
                                   .x[[2]] == FranchiseID) %>%
                     dplyr::mutate(Year=.x[[1]])) %>%
    dplyr::select(FranchiseID, Year, TeamID, Location, Nickname)
}
