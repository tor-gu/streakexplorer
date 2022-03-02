franchises_by_season <- function(franchises, year) {
  franchises %>% dplyr::filter(FirstSeason <= year &
                                 (FinalSeason >= year | is.na(FinalSeason)))
}

# TODO maybe delete
franchises_by_seasons <- function(franchises, years) {
  purrr::map(years, function(year) franchises_by_season(franchises, year)) %>%
    data.table::rbindlist() %>% tibble::as_tibble() %>% unique()
}

