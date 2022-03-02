library(tidyr)
library(dplyr)

# TODO unused
som_add_adj_score <- function(streaks, prop, top=TRUE) {
  if (top) {
    slicer <- slice_max
  } else {
    slicer <- slice_min
  }
  streaks %>% group_by(Level) %>%
    slicer(Score, prop=prop) %>%
    mutate(mean=mean(Score), sd=sd(Score)) %>%
    mutate(AdjScore=(Score-mean)/sd) %>%
    select(-sd, -mean)
}

# TODO unused
som_make_level_table <- function(levels) {
  tibble(Level=levels) %>% mutate(Next=lead(Level)) %>% filter(!is.na(Next))
}

# TODO unused
som_make_segments <- function(streaks, concordances, level_table) {
  streaks %>%
    full_join(streaks, by=character()) %>%
    inner_join(level_tbl, by=c("Level.x"="Level", "Level.y"="Next")) %>%
    inner_join(concordances, by=c("StreakId.x"="Outer", "StreakId.y"="Inner"))
}

# TODO unused
som_get_related_streaks_by_team <- function(streaks, Id) {
  Team <- streaks %>% filter(Id==.env$Id) %>% pull(Team)
  streaks %>% filter(Team==.env$Team)
}

# TODO unused
som_get_related_streaks_by_team_season <- function(streaks, Id) {
  Team <- streaks %>% filter(Id==.env$Id) %>% pull(Team)
  Year <- streaks %>% filter(Id==.env$Id) %>% pull(Year)
  streaks %>% filter(Team==.env$Team & Year==.env$Year)
}

som_get_related_streak_ids <- function(concordances, StreakId) {
  Inner <- concordances %>% filter(Inner==StreakId) %>% pull(Outer)
  Outer <- concordances %>% filter(Outer==StreakId) %>% pull(Inner)
  c(Inner,Outer) %>% unique()
}

# TODO unused
som_get_related_streaks <- function(streaks, concordances, Id) {
  StreakId <- top_streaks %>% filter(Id==.env$Id) %>% pull(StreakId)
  related_streak_ids <- som_get_related_streak_ids(concordances, StreakId)
  streaks %>% filter(StreakId %in% related_streak_ids)
}

# TODO unused
som_get_identical_streaks <- function(streaks, Id) {
  StreakId <- top_streaks %>% filter(Id==.env$Id) %>% pull(StreakId)
  streaks %>% filter(StreakId == .env$StreakId)
}

# TODO unused
som_add_adj_level <- function(streaks, levels) {
 streaks %>% mutate(
   AdjLevel = purrr::map_int(Level, ~which(levels == .x))
 )
}



