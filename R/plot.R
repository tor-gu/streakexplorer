plot_add_lines_maybe <- function(p, lines = NULL, ...) {
  if (!is.null(lines) && nrow(lines) > 0) {
    data <- plotly::highlight_key(lines, ~LineId, group = "line-highlight")
    plotly::add_lines(p, data = data, ...)
  } else {
    p
  }
}

plot_lines <- function(lines, max_rank, reverse_x_axis = FALSE) {
  x_range <- range(lines$IntensityLevel)
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
  line_types <- c(base = "dot", season = "dash", related = "solid",
                  identical = "solid")
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
      # linetype=~line_type, linetypes=line_types,
      color = ~ factor(line_colored), colors = colors
    ) %>%
    plot_add_lines_maybe(
      lines = split_lines$season,
      alpha = 0.7,
      line = list(shape = "spline", width = 1),
      text = ~HoverText,
      # linetype=~line_type, linetypes=line_types,
      color = ~ factor(line_colored), colors = colors
    ) %>%
    plot_add_lines_maybe(
      lines = rbind(split_lines$related, split_lines$identical),
      alpha = 0.7,
      line = list(shape = "spline", width = 3),
      text = ~HoverText,
      # linetype=~line_type, linetypes=line_types,
      color = ~ factor(line_colored), colors = colors
    ) %>%
    plotly::highlight(
      on = "plotly_hover", off = "plotly_doubleclick",
      opacityDim = .6, color = "red"
    ) %>%
    plotly::layout(xaxis = list(
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
