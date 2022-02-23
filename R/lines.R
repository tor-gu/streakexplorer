numeric_right_segment_plus1 <- function(all_levels, my_levels) {
  lb <- all_levels %>%
    magrittr::extract(. <= max(my_levels)) %>%
    setdiff(my_levels) %>% max()
  list(lb=lb, ub=max(my_levels))
}

numeric_left_segment_minus1 <- function(all_levels, my_levels) {
  ub <- all_levels %>%
    magrittr::extract(. >= min(my_levels)) %>%
    setdiff(my_levels) %>% min()
  list(lb=min(my_levels), ub=ub)
}

lines_split_streak <- function(streaks, concordances, levels, streak_id,
                               top=TRUE) {
  #message(glue::glue("lines split streak {streak_id}"))
  streak_levels <- streaks %>%
    filter(StreakId == streak_id) %>%
    pull(Level)
  level_bounds <-
    if (top)
      numeric_right_segment_plus1(levels, streak_levels)
    else
      numeric_left_segment_minus1(levels, streak_levels)
  streak_line <- streaks %>%
    semi_join(concordances %>% filter(Inner==streak_id),
              by=c("StreakId"="Outer")) %>%
    filter(between(Level, level_bounds$lb, level_bounds$ub))
  remainder <-
    if (top)
      streaks %>% filter(StreakId != streak_id | Level <= level_bounds$lb)
    else
      streaks %>% filter(StreakId != streak_id | Level >= level_bounds$ub)

  list(remainder=remainder, streak_line=streak_line)
}

lines_split_top_streak <- function(streak_lines, concordances, levels,
                                   top=TRUE) {
  #message("lines_split_top_streak")
  current_remainder <- streak_lines$remainder
  slicer <- if (top) slice_max else slice_min
  streak_id <- current_remainder %>%
    slicer(Level, n=1, with_ties=FALSE) %>%
    pull(StreakId)
  new_split <- lines_split_streak(current_remainder, concordances, levels,
                                 streak_id, top)
  list(remainder = new_split$remainder,
       lines = rlist::list.append(streak_lines$lines, new_split$streak_line))
}

lines_split_all <- function(streaks, concordances, levels, top=TRUE) {
  message("Calling lines_split_all")
  streak_lines <- list(remainder=streaks, lines=list())
  while (nrow(streak_lines$remainder) > 0) {
    streak_lines <- lines_split_top_streak(streak_lines, concordances,
                                           levels, top)
  }
  streak_lines$lines
}

lines_bind <- function(lines) {
  purrr::reduce(
    purrr::imap(lines, ~ .x %>% mutate(LineIdx=.y)),
    rbind)
}

get_related_lines <- function(line_id, lines_to_streaks, concordances) {
  related_streak_ids <- lines_to_streaks %>%
    dplyr::filter(LineIdx==line_id) %>%
    dplyr::pull(StreakId) %>%
    purrr::map(~som_get_related_streak_ids(concordances, .)) %>%
    unlist(recursive = FALSE) %>% unique()
  lines_to_streaks %>%
    dplyr::filter(StreakId %in% related_streak_ids) %>%
    pull(LineIdx)
}


lines_highlight <- function(lines, concordances, lines_to_streaks,
                            id=NULL) {
  message(glue::glue("lines_highlight"))
  result <- lines %>%
    dplyr::mutate(
      line_colored="a",
      line_width=1,
      line_type="base"
    )

  if ( !is.null(id) ) {
    row <- lines %>% filter(LineIdx==id) %>% head(1)
    if ( nrow(row) > 0 ) {
      team <- row %>% pull(Team)
      year <- row %>% pull(Year)
      related_line_ids <- get_related_lines(id, lines_to_streaks, concordances)
      result <- result %>%
        dplyr::mutate(
          line_colored=dplyr::if_else(Year==year & Team==team, "b",
                                      line_colored),
          line_type=dplyr::if_else(Year==year & Team==team, "season",
                                   line_type),
        ) %>%
        dplyr::mutate(
          line_type=dplyr::if_else(LineIdx %in% related_line_ids, "related",
                                   line_type),
        ) %>%
        dplyr::mutate(
          line_colored=dplyr::if_else(LineIdx == id, "c", line_colored),
          line_type=dplyr::if_else(LineIdx == id, "identical", line_type),
        )
    }
  }
  result
}

lines_remove_nubs <- function(lines) {
  # This is a cleanup function to be applied after a Rank filter has been
  # applied.  The idea is to remove the inital element of a line when
  # that element is from a different streak, and there is a gap between the
  # initial element and the next one (because the intermediate ones were
  # removed in a Rank filter)
  lines %>%
    dplyr::arrange(LineIdx, AdjLevel) %>%
    dplyr::filter(dplyr::lead(LineIdx) != LineIdx |
                    dplyr::lead(StreakId) == StreakId |
                    dplyr::lead(AdjLevel) - AdjLevel == 1 |
                    is.na(dplyr::lead(AdjLevel)))
}

lines_plot <- function(lines, max_rank, reverse_x_axis=FALSE) {
  base <- lines %>%
    group_by(LineIdx) %>%
    #plotly::highlight_key(~StreakId)
    plotly::highlight_key(~LineIdx)
  x_range <- range(lines$AdjLevel)
  x_axis_range <- if(reverse_x_axis) rev(x_range) else x_range
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
    plotly::layout(xaxis=list(title="Level", range=x_axis_range,
                              showticklabels=FALSE, zeroline=FALSE)) %>%
    plotly::layout(yaxis=list(title="Rank", range=c(max_rank+1,0),
                              zeroline=FALSE, tick0=1, dtick=max_rank-1)) %>%
    plotly::layout(showlegend=FALSE) %>%
    plotly::config(displayModeBar=FALSE)
}

add_descenders <- function(initial_rank_filter, initial_filter) {
  filtered_left <- initial_filter %>%
    mutate(PrevAdjLevel=AdjLevel - 1) %>%
    semi_join(initial_rank_filter,
              by=c("StreakId", "PrevAdjLevel"="AdjLevel")) %>%
    select(-PrevAdjLevel)

  filtered_right <- initial_filter %>%
    mutate(NextAdjLevel=AdjLevel + 1) %>%
    semi_join(initial_rank_filter,
              by=c("StreakId", "NextAdjLevel"="AdjLevel")) %>%
    select(-NextAdjLevel)

  rbind(initial_rank_filter, filtered_left, filtered_right) %>% unique()
}
