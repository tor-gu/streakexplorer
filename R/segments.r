seg_make_level_table <- function(levels) {
  tibble(Level=levels) %>% mutate(Next=lead(Level)) %>% filter(!is.na(Next))
}

seg_make_segments <- function(streaks, concordances, levels) {
  level_table <- seg_make_level_table(levels)
  streaks %>%
    full_join(streaks, by=character()) %>%
    inner_join(level_table, by=c("Level.x"="Level", "Level.y"="Next")) %>%
    inner_join(concordances, by=c("StreakId.x"="Outer", "StreakId.y"="Inner"))
}

seg_add_relations <- function(segments, streaks_with_relations) {
  streak_relations <- streaks_with_relations %>% select(Id,Relation)
  segments %>%
    left_join(streak_relations, by=c("Id.x"="Id")) %>%
    rename(Relation.x=Relation) %>%
    left_join(streak_relations, by=c("Id.y"="Id")) %>%
    rename(Relation.y=Relation) %>%
    mutate(Relation=pmax(Relation.x, Relation.y)) %>%
    select(-Relation.x, -Relation.y)
}
