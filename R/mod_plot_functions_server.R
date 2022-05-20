# Top level mod_plot server functions ----

plot_server_get_max_rank <- function(franchises, years, teams, hot) {
  # Get the max rank using n=10
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  team_ids <- ps_franchise_ids_to_team_ids(franchises, teams, min_year,
                                           max_year)
  ps_streaks_get_max_rank(10, min_year, max_year, team_ids, hot)
}

plot_server_lines_highlight <- function(lines, selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  ps_lines_highlight(lines,
                     lzy_concordances(hot),
                     lzy_lines_to_streaks(hot),
                     selected_line_id)
}

plot_server_build_lines <- function(franchises, intensity_level_range, years, teams,
                               max_rank, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  if (hot) {
    left_intensity <- intensity_level_range[[1]]
  } else {
    left_intensity <- intensity_level_range[[2]]
  }
  team_ids <- ps_franchise_ids_to_team_ids(franchises, teams, min_year,
                                           max_year)

  ps_build_lines(lzy_lines(hot), min_year, max_year, team_ids,
                 franchises, max_rank, left_intensity)
}

plot_server_get_selected_streak <- function(selected_line_id, hot) {
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  lzy_game_logs <- dplyr::tbl(se_pool, "game_logs")
  ps_lines_to_related_streak(
    lzy_lines_to_streaks(hot),
    lzy_streaks(hot),
    lzy_game_logs,
    selected_line_id)
}

plot_server_main_plot <- function(highlighted_lines, intensity_level_range,
                             max_rank, hot) {
  if (
    torgutil::tbl_is_column_value_unique(highlighted_lines, Year) &
    torgutil::tbl_is_column_value_unique(highlighted_lines, Team)
  ) {
    # For single-team plots, treat the "season" lines like the "base" lines
    highlighting <- tibble::tribble(
      ~line_type, ~color, ~width,
      "base",     highlight_colors$base, 1,
      "season",   highlight_colors$base, 1,
      "related",  highlight_colors$medium, 3,
      "identical",highlight_colors$high, 5
    )
  } else {
    highlighting <- tibble::tribble(
      ~line_type, ~color, ~width,
      "base",     highlight_colors$base, 1,
      "season",   highlight_colors$low, 3,
      "related",  highlight_colors$medium, 3,
      "identical",highlight_colors$high, 5
    )
  }
  min_intensity <- intensity_level_range[[1]]
  max_intensity <- intensity_level_range[[2]]
  ps_plot_lines(
    highlighted_lines,
    min_intensity,
    max_intensity,
    max_rank,
    highlighting,
    reverse_x_axis = !hot
  )
}

# Utility functions for mod_plot server ----

#' ps_build_lines
#'
#' Generate the table of lines for the given filter (years, teams and rank).
#' The left intensity is requires so that the full-season lines are always
#' included.
#'
#' @param lzy_lines Lazy lines table
#' @param min_year Min Year
#' @param max_year Max Year
#' @param teams Vector of teamIDs
#' @param franchises Franchises table
#' @param max_rank Max Rank
#' @param left_intensity Left-most intensity level (min for hot, max for cold)
#'
#' @return
ps_build_lines <- function(lzy_lines, min_year, max_year, teams,
                              franchises, max_rank, left_intensity) {
  # Get the team-ids
  team_ids <- ps_franchise_ids_to_team_ids(
    franchises, teams, min_year, max_year)

  # Get the left-most line elements, to add in at the end
  # The reason this is a special case is that sometimes the
  # left-most line contains a single node, and we don't want to
  # strip it out with all the other single-node lines.
  left_lines <- lzy_lines %>%
    dplyr::filter(between(Year, min_year, max_year),
                  Team %in% teams, Rank <= max_rank,
                  IntensityLevel == left_intensity) %>%
    dplyr::collect()

  # Get the lines
  lzy_lines %>%
    # Initial filter by years, teams, and ranks
    dplyr::filter(between(Year, min_year, max_year),
                  Team %in% teams, Rank <= max_rank) %>%
    # Now filter out lines that have only a single node above the cutoff
    dplyr::count(LineId) %>%
    dplyr::filter(n>1) %>%

    # Now add back in the whole lines for what remains
    dplyr::select(LineId) %>%
    dplyr::left_join(lzy_lines, by="LineId") %>%
    dplyr::filter(IntensityLevel != left_intensity) %>%
    dplyr::collect() %>%

    # Finally, add the left-most line elements
    rbind(left_lines)
}

#' ps_franchise_ids_to_team_ids
#'
#' Find all TeamIDs for the given FranchiseIDs and the year range.
#'
#' @param franchises Franchise table
#' @param franchise_ids vector of FranchiseIDs
#' @param min_year First year
#' @param max_year Final year
#'
#' @return List of TeamIDs
ps_franchise_ids_to_team_ids <- function(franchises, franchise_ids,
                                                 min_year, max_year) {
  franchises %>%
    dplyr::filter(FranchiseID %in% franchise_ids,
                  min_year <= FinalSeason | is.na(FinalSeason),
                  max_year >= FirstSeason) %>%
    dplyr::pull(TeamID)
}

#' ps_streaks_get_max_rank
#'
#' Given a year range and a list of teams and a value n, find an
#' estimate of the maximum of the nth highest rank over all intensity levels.
#'
#' This function is a wrapper around `ps_streaks_get_max_rank_simple` and
#' `ps_streaks_get_max_rank_by_sampling` and will decide the appropriate
#' function and parameters to use based on the number of years and
#' the number of teams.
#'
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#' @param hot If `TRUE` use hot streaks, otherwise cold streaks
#'
#' @return Max value estimate
ps_streaks_get_max_rank <- function(n, min_year, max_year, teams, hot) {
  lzy_streaks <- lzy_streaks(hot)
  season_count <- (max_year - min_year + 1) * length(teams)
  if (season_count < 25)
    ps_streaks_get_max_rank_simple(lzy_streaks, n, min_year, max_year, teams)
  else if (season_count < 750) {
    ps_streaks_get_max_rank_by_sampling(lzy_streaks, n, min_year, max_year,
                                     teams, 1:4 * 20, 3/2)
  } else {
    ps_streaks_get_max_rank_by_sampling(lzy_streaks, n, min_year, max_year,
                                     teams, 1:4 * 20, 4/3)
  }
}

#' ps_streaks_get_max_rank_simple
#'
#' Given a year range and a list of teams and a value n, find the maximum
#  of the nth highest rank over all intensity levels.
#'
#' Note: This function is inefficient when the number number of intensity
#' levels, teams and years is large.
#'
#' @param lzy_streaks Lazy streaks table (possibly with a filter)
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#'
#' @return Maximum value
ps_streaks_get_max_rank_simple <- function(lzy_streaks, n, min_year, max_year,
                                           teams) {
  lzy_streaks %>%
    dplyr::filter(Team %in% teams,
                  dplyr::between(Year, min_year, max_year)) %>%
    dplyr::collect() %>%
    dplyr::group_by(IntensityLevel) %>%
    dplyr::arrange(Rank) %>%
    dplyr::mutate(rn=dplyr::row_number()) %>%
    dplyr::filter(rn==n) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(max_rank=max(Rank)) %>%
    dplyr::pull(max_rank)
}

#' ps_streaks_get_max_rank_by_sampling
#'
#' Give an estimate of the rank returned by `ps_streaks_get_max_rank_simple`
#' using this method:
#' * First: Apply the algorithm of `ps_streaks_get_max_rank_simple` to a limited
#' set of intensity levels (e.g. `c(25,50,75)` instead of `1:101`).
#' * Second, increase the returned rank and increase it by a scaling factor
#' (e.g. `1.5`).
#' * Third, restrict the full streaks table to `Rank` values below the scaled
#' initial estimate.
#' * Finally, apply `ps_streaks_get_max_rank_simple` to the restricted streak
#' table, this time across all intensity levels.
#'
#' Notes:
#' * This estimate will always be less than or equal to the true value.
#' * This function calls `ps_streaks_get_max_rank_simple` twice, but each time
#' with a filter applied to the `lzy_streaks_tbl`.  It is less efficient
#' than `ps_streaks_get_max_rank_simple` on smaller datasets, but much faster
#' on larger datasets.
#' * Increasing the scaling factor or the intensity sample space increases
#' the accuracy at the cost of speed.
#' * Smaller datasets require larger scaling factors, and larger datasets
#' require smaller scaling factors.
#'
#' @param lzy_streaks Lazy streaks table
#' @param n Function will maximize value of `n`th highest rank
#' @param min_year Minimum year for filter
#' @param max_year Maximum year for filter
#' @param teams Vector of team IDs for filter.
#' @param levels Intensity levels for the sampling, e.g. `c(25,50,75)`
#' @param scaling Scaling factor, e.g. `1.5`
#'
#' @return Estimate of maximum value
ps_streaks_get_max_rank_by_sampling <- function(lzy_streaks, n, min_year,
                                             max_year, teams, levels,
                                             scaling) {
  # Get the max over the IntensityLevel sample space, and scale the result
  # using the scaling factor
  initial_max_rank <- lzy_streaks %>%
    dplyr::filter(IntensityLevel %in% levels) %>%
    ps_streaks_get_max_rank_simple(n, min_year, max_year, teams) * scaling

  # Now get the max over all intensity levels, restricted by the scaled
  # sample value.
  lzy_streaks %>%
    dplyr::filter(Rank <= initial_max_rank) %>%
    ps_streaks_get_max_rank_simple(n, min_year, max_year, teams)
}

#' ps_lines_highlight
#'
#' Given a table of `lines` and a `line_id`, add a new column, `line_type`,
#' with values:
#' * `"identical"`: Matching line_id
#' * `"related"`: Line represents a sub- or super-streak, but is not identical
#' * `"season"`: Line is from same year and team, but not a sub- or super-streak
#' * `"base"`: Unrelated line
#'
#' If `line_id` is `NULL` or omitted, all values will be `"base"`
#'
#' @param lines Table of lines
#' @param lzy_concordances Lazy concordances table
#' @param lzy_lines_to_streaks  Lazy lines_to_streaks table
#' @param line_id LineID
#'
#' @return Lines with added `line_type` column.
ps_lines_highlight <- function(lines, lzy_concordances, lzy_lines_to_streaks,
                               line_id = NULL) {
  # Initialize with line_type == "base"
  result <- lines %>% dplyr::mutate(line_type = "base")

  # Check that we were passed a line_id
  if (!is.null(line_id)) {
    # Get the Team and Year from the first line matching the line_id
    row <- lines %>%
      dplyr::filter(LineId == line_id) %>%
      head(1)
    if (nrow(row) > 0) {
      team <- row$Team
      year <- row$Year
      # Get related line ids
      related_line_ids <- lines_get_related_lines(
        line_id, lzy_lines_to_streaks,
        lzy_concordances
      )
      result <- result %>%
        # Same Year and Team:  line_type = "season"
        dplyr::mutate(
          line_type = dplyr::if_else(Year == year & Team == team, "season",
                                     line_type
          ),
        ) %>%
        # Substreak or superstreak: line_type = "related"
        dplyr::mutate(
          line_type = dplyr::if_else(LineId %in% related_line_ids, "related",
                                     line_type
          ),
        ) %>%
        # Same streak: line_type = "identical"
        dplyr::mutate(
          line_type = dplyr::if_else(LineId == line_id, "identical", line_type),
        )
    }
  }
  result
}

#' ps_lines_to_related_streak
#'
#' Given a line_id, get the related streak, as single-row table with
#' columns `Year`, `Team`, `LoIndex`,` HiIndex`, `StartDate`, and `EndDate`.
#'
#' @param lzy_lines_to_streaks Lazy lines_to_streaks table
#' @param lzy_streaks  Lazy streaks table
#' @param lzy_game_logs Lazy game_logs table
#' @param line_id LineID
#'
#' @return Streak
ps_lines_to_related_streak <- function(lzy_lines_to_streaks, lzy_streaks,
                                      lzy_game_logs, line_id) {
  if (is.null(line_id)) {
    NULL
  } else {
    lzy_lines_to_streaks %>%
      dplyr::filter(LineId==line_id) %>%
      dplyr::left_join(lzy_streaks, by="StreakId") %>%
      head(1) %>%
      dplyr::select(Year, Team, LoIndex, HiIndex) %>%
      dplyr::left_join(lzy_game_logs,
                       by=c("Year","Team","LoIndex"="GameIndex")) %>%
      dplyr::left_join(lzy_game_logs,
                       by=c("Year","Team","HiIndex"="GameIndex")) %>%
      dplyr::collect() %>%
      dplyr::mutate(StartDate = lubridate::as_date(Date.x)) %>%
      dplyr::mutate(EndDate = lubridate::as_date(Date.y)) %>%
      dplyr::select(Year, Team, LoIndex, HiIndex, StartDate, EndDate)
  }
}

#' ps_plot_lines
#'
#' Build the plotly plot.  The lines should have already been
#' passed through lines_highlight
#'
#' @param lines Lines to plot
#' @param min_intensity Min intensity level
#' @param max_intensity Max intensity level
#' @param max_rank Max rank
#' @param highlighting Highlighting info (line colors and widths)
#' @param reverse_x_axis If TRUE, reverse the x-axis
#'
#' @return plotly::plot_ly object
ps_plot_lines <- function(lines, min_intensity, max_intensity, max_rank,
                       highlighting, reverse_x_axis = FALSE) {
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
  colors <- torgutil::tbl_as_named_list(highlighting, color, line_type) %>%
    unlist()
  widths <- torgutil::tbl_as_named_list(highlighting, width, line_type)

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
    plot_add_lines_maybe(
      lines = split_lines$base,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$base),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Add the "season" lines
    plot_add_lines_maybe(
      lines = split_lines$season,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$season),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Add the "related" lines
    plot_add_lines_maybe(
      lines = split_lines$related,
      alpha = 0.7,
      line = list(shape = "spline", width = widths$related),
      text = ~HoverText,
      color = ~ factor(line_type), colors = colors
    ) %>%
    # Add the "identical" lines
    plot_add_lines_maybe(
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
