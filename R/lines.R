numeric_right_segment_plus1 <- function(all_levels, my_levels) {
  lb <- all_levels %>%
    magrittr::extract(. <= max(my_levels)) %>%
    setdiff(my_levels) %>% max()
  list(lb=lb, ub=max(my_levels))
}

lines_split_streak <- function(streaks, concordances, levels, streak_id) {
  streak_levels <- streaks %>%
    filter(StreakId == streak_id) %>%
    pull(Level)
  level_bounds <- numeric_right_segment_plus1(levels, streak_levels)
  streak_line <- streaks %>%
    semi_join(concordances %>% filter(Inner==streak_id),
              by=c("StreakId"="Outer")) %>%
    filter(between(Level, level_bounds$lb, level_bounds$ub))
  remainder <- streaks %>%
    filter(StreakId != streak_id | Level <= level_bounds$lb)
  list(remainder=remainder, streak_line=streak_line)
}

lines_split_top_streak <- function(streak_lines, concordances, levels) {
  current_remainder <- streak_lines$remainder
  streak_id <- current_remainder %>%
    slice_max(Level, n=1, with_ties=FALSE) %>%
    pull(StreakId)
  new_split <- lines_split_streak(current_remainder, concordances, levels,
                                 streak_id)
  list(remainder = new_split$remainder,
       lines = rlist::list.append(streak_lines$lines, new_split$streak_line))
}

lines_split_all <- function(streaks, concordances, levels) {
  streak_lines <- list(remainder=streaks, lines=list())
  while (nrow(streak_lines$remainder) > 0) {
    streak_lines <- lines_split_top_streak(streak_lines, concordances, levels)
  }
  streak_lines$lines
}

lines_bind <- function(lines) {
  purrr::reduce(
    purrr::imap(lines, ~ .x %>% mutate(LineIdx=.y)),
    rbind)
}
