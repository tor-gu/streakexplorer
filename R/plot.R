
plot_add_lines_maybe <- function(p, lines = NULL, ...) {
  if (!is.null(lines) && nrow(lines) > 0) {
    data <- plotly::highlight_key(lines, ~LineId, group = "line-highlight")
    plotly::add_lines(p, data = data, ...)
  } else {
    p
  }
}

plot_lines <- function(lines, intensity_level_range, max_rank,
                       reverse_x_axis = FALSE) {
  x_range <- intensity_level_range
  x_axis_range <- if (reverse_x_axis) rev(x_range) else x_range
  x_range_len <- x_range[2] - x_range[1]
  x_axis_ticks <- c(
    x_range[1] + .05 * x_range_len,
    mean(x_range),
    x_range[1] + .95 * x_range_len
  )
  x_axis_tick_text <-
    if (reverse_x_axis) {
      c(
        "Pure losing\nstreak",
        "\U27F5 Streak intensity \U27F6",
        "Full\nseason"
      )
    } else {
      c(
        "Full\nseason",
        "\U27F5 Streak intensity \U27F6",
        "Pure winning\nstreak"
      )
    }
  y_axis_ticks <- c(
    1,
    mean(c(1, max_rank)),
    max_rank
  )
  y_axis_tick_text <- c(
    "# 1",
    "Streak\nrank",
    paste("# ", max_rank)
  )

  colors <- c(`1` = "black", "2" = "purple", "3" = "red")
  line_types <- c(
    base = "dot", season = "dash", related = "solid",
    identical = "solid"
  )
  split_lines <- lines %>%
    split(lines$line_type) %>%
    purrr::map(dplyr::group_by, LineId)
  plotly::plot_ly(
    source = "lines_plot",
    x = ~IntensityLevel, y = ~Rank, hoverinfo = "text"
  ) %>%
    plot_add_lines_maybe(
      lines = split_lines$base,
      alpha = 0.7,
      line = list(shape = "spline", width = 1),
      text = ~HoverText,
      color = ~ factor(line_colored), colors = colors
    ) %>%
    plot_add_lines_maybe(
      lines = split_lines$season,
      alpha = 0.7,
      line = list(shape = "spline", width = 3),
      text = ~HoverText,
      color = ~ factor(line_colored), colors = colors
    ) %>%
    plot_add_lines_maybe(
      lines = rbind(split_lines$related, split_lines$identical),
      alpha = 0.7,
      line = list(shape = "spline", width = 5),
      text = ~HoverText,
      color = ~ factor(line_colored), colors = colors
    ) %>%
    plotly::highlight(
      on = "plotly_hover", off = "plotly_doubleclick",
      opacityDim = .6, color = "red"
    ) %>%
    plotly::layout(xaxis = list(
      fixedrange = TRUE,
      range = x_axis_range,
      showgrid = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      tickangle = 0,
      tickfont = list(
        color = "purple",
        family = "Arial",
        size = 15
      ),
      tickmode = "array",
      ticktext = x_axis_tick_text,
      tickvals = x_axis_ticks,
      title = list(text = ""),
      zeroline = FALSE
    )) %>%
    plotly::layout(yaxis = list(
      fixedrange = TRUE,
      range = c(max_rank + 1, 0),
      showgrid = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      tickangle = 0,
      tickfont = list(
        color = "purple",
        family = "Arial",
        size = 15
      ),
      ticktext = y_axis_tick_text,
      tickvals = y_axis_ticks,
      title = list(text = ""),
      zeroline = FALSE
    )) %>%
    plotly::layout(showlegend = FALSE) %>%
    plotly::config(displayModeBar = FALSE)
}

plot_standings_graph <- function(standings, team, start_date, end_date) {
  standings <- standings %>% dplyr::mutate(GamesAbove=Wins-Losses)
  y_min <- min(standings$GamesAbove) - 1
  y_max <- max(standings$GamesAbove) + 1
  y_range <- standings %>%
    dplyr::filter(Date >= start_date - 1, Date <= end_date, Team==team) %>%
    dplyr::pull(GamesAbove) %>% range()
  rect_y_min <- y_range[1] - 1
  rect_y_max <- y_range[2] + 1

  ggplot2::ggplot(mapping = ggplot2::aes(Date, Wins - Losses, group = Team)) +
    ggplot2::geom_line(data = dplyr::filter(standings, Team != team)) +
    ggplot2::geom_line(data = dplyr::filter(standings, Team == team),
              color = "red") +
    ggplot2::annotate("rect", xmin=start_date - 1, xmax=end_date,
             ymin=rect_y_min, ymax=rect_y_max, alpha=0.2) +
    ggplot2::xlab(NULL) +
    #ggplot2::ylab("Games over .500") +
    ggplot2::ylab(NULL) +
    #ggplot2::labs(title="Standings graph") +
    ggplot2::scale_x_date(minor_breaks=NULL) +
    #ggplot2::scale_x_date(breaks=NULL) +
    ggplot2::scale_y_continuous(breaks=0, limits=c(y_min,y_max))
}

build_standings_graph <- function(franchises, standings, streak) {
  division_teams <- franchises_get_division_by_team_year(
    franchises, streak$Team, streak$Year)
  standings <- standings %>%
    dplyr::filter(Year==streak$Year) %>%
    dplyr::right_join(division_teams$division)
  plot_standings_graph(standings, streak$Team, streak$StartDate,
                       streak$EndDate)
}
