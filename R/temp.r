library(SOMData)
streaks <- SOMData::hot_streaks
concordances <- SOMData::hot_streaks_concordances
streaks
concordances

streaks %>% group_by(Level)


scores <- SOMData::hot_streaks %>%
  group_by(Level) %>%
  slice_max(Score, prop=.01) %>%
  mutate(mean=mean(Score), sd=sd(Score)) %>%
  mutate(AdjScore=(Score-mean)/sd) %>% slice_max(Score, n=10)

som_add_adj_score <- function(streaks, prop, top=TRUE) {
  streaks %>% group_by(Level) %>%
    slice_max(Score, prop=prop) %>%
    mutate(mean=mean(Score), sd=sd(Score)) %>%
    mutate(AdjScore=(Score-mean)/sd) %>%
    select(-sd, -mean)
}
adj_streaks <- streaks %>% som_add_adj_score(prop=.5)
str <- adj_streaks %>% slice_max(order_by=AdjScore, n=10)
str %>% full_join(str, by=character()) %>% right_join(concordances, by=c("Id.x"="Inner","Id.y"="Outer"))

str %>% full_join(str, by=character()) %>%
  filter(StreakId.x==62710 & StreakId.y==62710) %>%
  mutate(diff = Level.y-Level.x) %>%
  select(Id.x, StreakId.x, Level.x, Score.x,Id.y, StreakId.y, Level.y, Score.y, diff)

str <- str %>%  mutate(Level=as_factor(Level))
str %>% full_join(str, by=character())

str %>% mutate()
lvls <- as_factor(0:99/100, ordered=TRUE)
lvls[[3]]+1
library(forcats)
fct_shift(lvls)

lvls <- factor(0:99/100, ordered=TRUE)

level_tbl <- tibble(Level=1:99/100) %>% mutate(Next=lead(Level)) %>% filter(!is.na(Next))
str <- adj_streaks %>% slice_max(order_by=AdjScore, n=10) %>% select(-(Year:Ties))
str %>%
  full_join(str, by=character()) %>%
  right_join(level_tbl, by=c("Level.x"="Level", "Level.y"="Next")) %>%
  inner_join(concordances, by=c("StreakId.x"="Outer", "StreakId.y"="Inner")) %>%
  ggplot() + geom_segment(aes(x=Level.x, y=AdjScore.x, xend=Level.y, yend=AdjScore.y, color=StreakId.x))


str %>%
  full_join(str, by=character()) %>%
  inner_join(concordances, by=c("StreakId.x"="Outer", "StreakId.y"="Inner")) %>%
  filter(StreakId.x == StreakId.y)

som_make_level_table <- function(levels) {
  tibble(Level=levels) %>% mutate(Next=lead(Level)) %>% filter(!is.na(Next))
}

som_make_segments <- function(streaks, concordances, level_table) {
  streaks %>%
    full_join(streaks, by=character()) %>%
    inner_join(level_table, by=c("Level.x"="Level", "Level.y"="Next")) %>%
    inner_join(concordances, by=c("StreakId.x"="Outer", "StreakId.y"="Inner"))
}

level_table <- som_make_level_table(1:99/100)
streaks <- SOMData::hot_streaks
concordances <- SOMData::hot_streaks_concordances
adj_streaks <- streaks %>% som_add_adj_score(prop=.5, top=TRUE)
top_streaks <- adj_streaks %>% slice_max(order_by=AdjScore, n=10)

som_get_related_streaks_by_team <- function(streaks, Id) {
  Team <- streaks %>% filter(Id==.env$Id) %>% pull(Team)
  streaks %>% filter(Team==.env$Team)
}

som_get_related_streaks_by_team_season <- function(streaks, Id) {
  Team <- streaks %>% filter(Id==.env$Id) %>% pull(Team)
  Year <- streaks %>% filter(Id==.env$Id) %>% pull(Year)
  streaks %>% filter(Team==.env$Team & Year==.env$Year)
}

som_get_related_streaks <- function(streaks, Id) {

}
library(tidyverse)
level_table <- som_make_level_table(1:99/100)
streaks <- SOMData::hot_streaks
concordances <- SOMData::hot_streaks_concordances
adj_streaks <- streaks %>% som_add_adj_score(prop=.5, top=TRUE)
top_streaks <- adj_streaks %>% slice_max(order_by=AdjScore, n=10)

Id <- 476666
top_streaks %>% som_get_related_streaks_by_team_season(Id)
StreakId <- top_streaks %>% filter(Id==.env$Id) %>% pull(StreakId)
concordances %>% filter(Inner==StreakId | Outer==StreakId) %>% pull(Outer) %>% unique()
concordances %>% filter(Inner==StreakId | Outer==StreakId) %>% pull(Inner) %>% unique()

som_get_related_streak_ids <- function(concordances, StreakId) {
  Inner <- concordances %>% filter(Inner==StreakId) %>% pull(Outer)
  Outer <- concordances %>% filter(Outer==StreakId) %>% pull(Inner)
  c(Inner,Outer) %>% unique()
}
som_get_related_streaks <- function(streaks, concordances, Id) {
  StreakId <- top_streaks %>% filter(Id==.env$Id) %>% pull(StreakId)
  related_streak_ids <- som_get_related_streak_ids(concordances, StreakId)
  streaks %>% filter(StreakId %in% related_streak_ids)
}
som_get_identical_streaks <- function(streaks, Id) {
  StreakId <- top_streaks %>% filter(Id==.env$Id) %>% pull(StreakId)
  streaks %>% filter(StreakId == .env$StreakId)
}

som_get_related_streak_ids(concordances, 43)
top_streaks %>% som_get_related_streaks(concordances, 43288)
top_streaks %>% som_get_identical_streaks( 43288)

top_streaks <- ungroup(top_streaks)




streaks <- SOMData::hot_streaks
concordances <- SOMData::hot_streaks_concordances
adj_streaks <- streaks %>% som_add_adj_score(prop=.5, top=TRUE) %>%
  filter(Team %in% c("BOS", "NYA") & Year > 1999)
top_streaks <- adj_streaks %>% slice_max(order_by=AdjScore, n=10) %>% ungroup()
Id <- top_streaks$Id[[3]]
Id <- NULL
plot_make_data(top_streaks, concordances, Id) %>% plot_make_plot()
pd <- plot_make_data(top_streaks, concordances, Id)
pd$points$Relation
pd$segments$Relation


SOMData::game_logs %>% pull(Team) %>% unique()

