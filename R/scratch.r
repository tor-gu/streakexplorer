


selected_years <- 1950:1990
selected_leagues <- "AL"
league_divisions <-
  list(
    list(league="AL", division=NA),
    list(league="AL", division="West")
  )

franchises_ANA <- SOMData::franchises %>%
  filter_by_years(selected_years) %>%
  filter_by_league(selected_leagues) %>%
  truncate_years(selected_years) %>%
  filter_by_league_divisions(league_divisions) %>%
  filter(FranchiseID=="ANA")


SOMData::franchises %>%
  filter_by_years(selected_years) %>%
  filter_by_league(selected_leagues) %>%
  truncate_years(selected_years) %>%
  filter_by_league_divisions(league_divisions) %>%
  filter(FranchiseID=="MIN")

names <- franchises_ANA %>%
  arrange(FirstSeason) %>%
  mutate(FullName=glue::glue("{Location} {Nickname}")) %>%
  pull(FullName) %>% unique() %>%
  stringr::str_c(collapse = "/")


choice_values <- c("AL_West", "AL_None")

division_choice_value_as_league_and_division <- function(choice_value) {
  split_value <- as.list(str_split(choice_value, "_", simplify = TRUE))
  names(split_value) <- c("league", "division")
  split_value$division <- ifelse(split_value$division=="None", NA, split_value$division)
  split_value
}

division_choice_values_as_league_and_division_list <- function(choice_values) {
  purrr::map(choice_values, ~division_choice_value_as_league_and_division(.))
}

selected_years <- 1950:2020
selected_divisions <- list(list(league="AL", division=NA), list(league="AL", division="East"))
selection_table <- SOMData::franchises %>%
  filter_by_years(selected_years) %>%
  filter_by_league_divisions(selected_divisions) %>%
  truncate_years(selected_years) %>%
  arrange(desc(FirstSeason)) %>%
  select(FranchiseID, Nickname) %>%
  unique() %>%
  group_by(FranchiseID) %>%
  summarize(Nicknames=str_c(Nickname, collapse="/"))

choices <- as.list(selection_table$FranchiseID)
names(choices) <- as.list(selection_table$Nicknames)
choices
as.list(selection_table)

selection_table %>% pull(FranchiseID)

tbl_as_named_list <- function(tbl, value_col, name_col) {
  result <- tbl %>% pull({{value_col}}) %>% as.list()
  names(result) <- tbl %>% pull({{name_col}}) %>% as.list()
  result
}
tbl_as_named_list(selection_table, FranchiseID, Nicknames)
