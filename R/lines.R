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
      line_colored=1,
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
          line_colored=dplyr::if_else(Year==year & Team==team, 2,
                                      line_colored),
          line_type=dplyr::if_else(Year==year & Team==team, "season",
                                   line_type),
        ) %>%
        dplyr::mutate(
          line_type=dplyr::if_else(LineIdx %in% related_line_ids, "related",
                                   line_type),
          line_width=dplyr::if_else(LineIdx %in% related_line_ids, 3, 1)
        ) %>%
        dplyr::mutate(
          line_colored=dplyr::if_else(LineIdx == id, 3, line_colored),
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

lines_add_lines_maybe <- function(p, lines=NULL, ...) {
  if (!is.null(lines) && nrow(lines) > 0) {
    data <- plotly::highlight_key(lines, ~LineIdx, group="line-highlight")
    plotly::add_lines(p, data=data, ...)
  } else {
    p
  }
}

lines_plot <- function(lines, max_rank, reverse_x_axis=FALSE) {
  #base <- lines %>%
  #  group_by(LineIdx) %>%
  #  plotly::highlight_key(~LineIdx)
  x_range <- range(lines$AdjLevel)
  x_axis_range <- if(reverse_x_axis) rev(x_range) else x_range
  x_range_len <- x_range[2] - x_range[1]
  x_axis_ticks <- c(x_range[1] + .05*x_range_len,
                    mean(x_range),
                    x_range[1] + .95*x_range_len)
  x_axis_tick_text <-
    if (reverse_x_axis)
      c("Pure losing\nstreak",
        "\U27F5 Streak intensity \U27F6",
        "Full\nseason")
    else
      c("Full\nseason",
        "\U27F5 Streak intensity \U27F6",
        "Pure winning\nstreak")
  y_axis_ticks <- c(1,
                    mean(c(1, max_rank)),
                    max_rank)
  y_axis_tick_text <- c("# 1",
                        "Streak\nrank",
                        paste("# ", max_rank))

  colors <- c(`1` = "black", '2' = "purple", '3' = "red")
  line_types <- c(base="dot", season="dash", related="solid", identical="solid")
  split_lines <- lines %>%
    split(lines$line_type) %>%
    purrr::map(dplyr::group_by, LineIdx)
  plotly::plot_ly(source="lines_plot",
          x=~AdjLevel, y=~Rank, hoverinfo="text") %>%
    #plotly::add_lines(data=tibble(AdjLevel=integer(),Rank=integer(), line_colored=integer())) %>%
    lines_add_lines_maybe(
      lines=split_lines$base,
      alpha=0.7,
      line=list(shape="spline", width=1),
      text=~text,
      #linetype=~line_type, linetypes=line_types,
      color=~factor(line_colored), colors=colors
      ) %>%
    lines_add_lines_maybe(
      lines = split_lines$season,
      alpha=0.7,
      line=list(shape="spline", width=1),
      text=~text,
      #linetype=~line_type, linetypes=line_types,
      color=~factor(line_colored), colors=colors
    ) %>%
    lines_add_lines_maybe(
      lines = rbind(split_lines$related, split_lines$identical),
      alpha=0.7,
      line=list(shape="spline", width=3),
      text=~text,
      #linetype=~line_type, linetypes=line_types,
      color=~factor(line_colored), colors=colors
    ) %>%
    plotly::highlight(on="plotly_hover", off="plotly_doubleclick",
                      opacityDim=.6, color="red") %>%
    plotly::layout(xaxis=list(range=x_axis_range,
                              showgrid=FALSE,
                              showline=TRUE,
                              showticklabels=TRUE,
                              tickangle=0,
                              tickfont=list(color="purple",
                                            family="Arial",
                                            size=15),
                              tickmode="array",
                              ticktext=x_axis_tick_text,
                              tickvals=x_axis_ticks,
                              title=list(text=""),
                              zeroline=FALSE)) %>%
    plotly::layout(yaxis=list(range=c(max_rank+1,0),
                              showgrid=FALSE,
                              showline=TRUE,
                              showticklabels=TRUE,
                              tickangle=0,
                              tickfont=list(color="purple",
                                            family="Arial",
                                            size=15),
                              ticktext=y_axis_tick_text,
                              tickvals=y_axis_ticks,
                              title=list(text=""),
                              zeroline=FALSE)) %>%
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
