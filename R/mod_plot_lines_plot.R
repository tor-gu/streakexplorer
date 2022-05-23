#' ps_plot_lines
#'
#' Build the plotly plot.  The lines should have already been
#' passed through lines_highlight
#'
#' @param highlight_colors Colors for highlighting
#' @param lines Lines to plot
#' @param min_intensity Min intensity level
#' @param max_intensity Max intensity level
#' @param max_rank Max rank
#' @param highlighting Highlighting info (line colors and widths)
#' @param reverse_x_axis If TRUE, reverse the x-axis
#'
#' @return plotly::plot_ly object
ps_plot_lines <- function(highlight_colors, lines, min_intensity, max_intensity,
                          max_rank, highlighting, reverse_x_axis = FALSE) {
  # Set up the x-axis
  x_range <- c(min_intensity, max_intensity)
  x_axis_range <- if (reverse_x_axis) rev(x_range) else x_range
  x_range_len <- max_intensity - min_intensity
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

  # Set up the y-axis
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

  # Get the colors and line widths from the highlighting table
  colors <- tbl_as_named_list(highlighting, color, line_type) %>%
    unlist()
  widths <- tbl_as_named_list(highlighting, width, line_type)

  # Split the lines by line_type (set in lines_highlight) and group by
  # LineId within each
  split_lines <- lines %>%
    split(lines$line_type) %>%
    purrr::map(dplyr::group_by, LineId)

  # Now build the plot
  plotly::plot_ly(
    source = "lines_plot",
    x = ~IntensityLevel, y = ~Rank, hoverinfo = "text"
  ) %>%
    # Add the "base" lines
    ps_plot_add_lines_maybe(
      lines = split_lines$base,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$base),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Add the "season" lines
    ps_plot_add_lines_maybe(
      lines = split_lines$season,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$season),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Add the "related" lines
    ps_plot_add_lines_maybe(
      lines = split_lines$related,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$related),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Add the "identical" lines
    ps_plot_add_lines_maybe(
      lines = split_lines$identical,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$identical),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Set hover action
    plotly::highlight(
      on = "plotly_hover", off = "plotly_doubleclick",
      opacityDim = .6, color = colors[["identical"]]
    ) %>%
    # Add in the x-axis
    plotly::layout(xaxis = list(
      fixedrange = TRUE,
      range = x_axis_range,
      showgrid = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      tickangle = 0,
      tickfont = list(
        color = colors[["base"]],
        family = "Arial",
        size = 15
      ),
      tickmode = "array",
      ticktext = x_axis_tick_text,
      tickvals = x_axis_ticks,
      title = list(text = ""),
      zeroline = FALSE
    )) %>%
    # Add in the y-axis
    plotly::layout(yaxis = list(
      fixedrange = TRUE,
      range = c(max_rank + 1, 0),
      showgrid = FALSE,
      showline = TRUE,
      showticklabels = TRUE,
      tickangle = 0,
      tickfont = list(
        color = colors[["base"]],
        family = "Arial",
        size = 15
      ),
      ticktext = y_axis_tick_text,
      tickvals = y_axis_ticks,
      title = list(text = ""),
      zeroline = FALSE
    )) %>%
    # Finish up and return the plotly object
    plotly::layout(showlegend = FALSE,
                   paper_bgcolor = highlight_colors$background,
                   plot_bgcolor  = highlight_colors$background) %>%
    plotly::config(displayModeBar = FALSE)
}


#' ps_plot_add_lines_maybe
#'
#' Given plot_ly object and a set of table of lines, add the lines to
#' the object. Act as a passthrough if the lines table is omitted or
#' empty.
#'
#' @param p plot_ly object
#' @param lines Lines to add
#' @param ... Additional params for plotly::add_lines
#'
#' @return modified plot_ly object
ps_plot_add_lines_maybe <- function(p, lines = NULL, ...) {
  if (!is.null(lines) && nrow(lines) > 0) {
    data <- plotly::highlight_key(lines, ~LineId, group = "line-highlight")
    plotly::add_lines(p, data = data, ...)
  } else {
    p
  }
}

