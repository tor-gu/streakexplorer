numeric_right_segment_plus1 <- function(all_levels, my_levels) {
  lb <- all_levels %>%
    magrittr::extract(. <= max(my_levels)) %>%
    setdiff(my_levels) %>% max()
  list(lb=lb, ub=max(my_levels))
}

lines_split_streak <- function(streaks, concordances, levels, streak_id) {
  #message(glue::glue("lines split streak {streak_id}"))
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
  #message("lines_split_top_streak")
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
  message("Calling lines_split_all")
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


lines_update_text <- function(lines, game_logs) {
  # TODO build up glue string with paste, and then use a single mutate
  message("Calling lines_update_text")
  lines %>%   left_join(
    SOMData::game_logs %>% select(Year, Team, GameIndex,
                                  StartGameNumber=GameNumber, StartDate=Date),
    by=c("Year","Team","LoIndex"="GameIndex")) %>%
  left_join(
    SOMData::game_logs %>% select(Year, Team, GameIndex,
                                  EndGameNumber=GameNumber, EndDate=Date),
    by=c("Year","Team","HiIndex"="GameIndex")) %>%
  mutate(text=glue::glue("{Team} {Year}")) %>%
  mutate(text=ifelse(Ties==0,
                     glue::glue("{text}<br>{Wins}-{Losses}"),
                     glue::glue("{Team} {Year}<br>{Wins}-{Losses}-{Ties}")
    )
  ) %>%
  mutate(text=glue::glue("{text}<br>Rank {Rank}")) %>%
  mutate(text=glue::glue(
    "{text}<br>{format.Date(StartDate, '%m/%d')} - {format.Date(EndDate, '%m/%d')}"))
}

lines_highlight <- function(lines, streaks, concordances, id=NULL) {
  message(glue::glue("lines_highlight"))
  result <- lines %>%
    mutate(
      line_colored="a",
      line_width=1,
      line_type="base"
    )

  if ( !is.null(id) ) {
    row <- streaks %>% filter(Id==id)
    streak_id <- row %>% pull(StreakId)
    team <- row %>% pull(Team)
    year <- row %>% pull(Year)
    related_streak_ids <- SOMData::get_related_streaks(
      streaks, concordances, id) %>% pull(StreakId) %>% unique()

    message(glue::glue("lines_highlight {row} {streak_id} {team} {year} "))
    result <- result %>%
      mutate(
        line_colored=ifelse(Year==year & Team==team, "b", line_colored),
        line_type=ifelse(Year==year & Team==team, "season", line_type),
      ) %>%
      mutate(
        line_type=ifelse(StreakId %in% related_streak_ids, "related", line_type),
      ) %>%
      mutate(
        line_colored=ifelse(StreakId == streak_id, "c", line_colored),
        line_type=ifelse(StreakId == streak_id, "identical", line_type),
      )
  }
  result
}

lines_plot <- function(lines, max_rank) {
  base <- lines %>%
    group_by(LineIdx) %>%
    plotly::highlight_key(~StreakId)
  colors <- c("black", "purple", "red")
  line_types <- c(base="dot", season="dash", related="solid", identical="solid")
  plotly::plot_ly(source="lines_plot", base,
          x=~AdjLevel, y=~Rank, hoverinfo="text") %>%
    plotly::add_lines(alpha=0.7,
              line=list(shape="spline"),
              text=~text,
              color=~line_colored, colors=colors,
              linetype=~line_type, linetypes=line_types) %>%
    plotly::highlight(on="plotly_hover", off="plotly_doubleclick") %>%
    plotly::layout(xaxis=list(title="Level", showticklabels=FALSE,
                              zeroline=FALSE)) %>%
    #plotly::layout(yaxis=list(title="Rank", autorange="reversed",
    #                          zeroline=FALSE, tick0=1, dtick=max_rank-1)) %>%
    plotly::layout(yaxis=list(title="Rank", range=c(max_rank+1,0),
                              zeroline=FALSE, tick0=1, dtick=max_rank-1)) %>%
    plotly::layout(showlegend=FALSE) %>%
    plotly::config(displayModeBar=FALSE)
}

add_descenders <- function(initial_rank_filter, initial_filter) {
  filtered_left <- initial_filter %>%
    mutate(PrevAdjLevel=AdjLevel - 1) %>%
    semi_join(initial_rank_filter, by=c("StreakId", "PrevAdjLevel"="AdjLevel")) %>%
    select(-PrevAdjLevel)

  filtered_right <- initial_filter %>%
    mutate(NextAdjLevel=AdjLevel + 1) %>%
    semi_join(initial_rank_filter, by=c("StreakId", "NextAdjLevel"="AdjLevel")) %>%
    select(-NextAdjLevel)

  rbind(initial_rank_filter, filtered_left, filtered_right) %>% unique()
}
