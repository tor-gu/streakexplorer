lines_get_related_lines <- function(line_id, lines_to_streaks, concordances) {
  related_streak_ids <- lines_to_streaks %>%
    dplyr::filter(LineId == line_id) %>%
    dplyr::pull(StreakId) %>%
    purrr::map(streak_get_related_streak_ids, concordances) %>%
    unlist(recursive = FALSE) %>%
    unique()
  lines_to_streaks %>%
    dplyr::filter(StreakId %in% related_streak_ids) %>%
    dplyr::pull(LineId)
}


lines_highlight <- function(lines, concordances, lines_to_streaks,
                            id = NULL) {
  message(glue::glue("lines_highlight"))
  result <- lines %>%
    dplyr::mutate(
      line_colored = 1,
      line_type = "base"
    )

  if (!is.null(id)) {
    row <- lines %>%
      dplyr::filter(LineId == id) %>%
      head(1)
    if (nrow(row) > 0) {
      team <- row %>% dplyr::pull(Team)
      year <- row %>% dplyr::pull(Year)
      related_line_ids <- lines_get_related_lines(
        id, lines_to_streaks,
        concordances
      )
      result <- result %>%
        dplyr::mutate(
          line_colored = dplyr::if_else(Year == year & Team == team, 2,
            line_colored
          ),
          line_type = dplyr::if_else(Year == year & Team == team, "season",
            line_type
          ),
        ) %>%
        dplyr::mutate(
          line_type = dplyr::if_else(LineId %in% related_line_ids, "related",
            line_type
          ),
        ) %>%
        dplyr::mutate(
          line_colored = dplyr::if_else(LineId == id, 3, line_colored),
          line_type = dplyr::if_else(LineId == id, "identical", line_type),
        )
    }
  }
  result
}

lines_remove_branch_descenders <- function(lines, max_rank, hot) {
  if (hot) {
    lines <- lines %>%
      dplyr::arrange(LineId, IntensityLevel)
  } else {
    lines <- lines %>%
      dplyr::arrange(LineId, desc(IntensityLevel))
  }
  lines %>%
    torgutil::filter_out(
      dplyr::lead(LineId) == LineId,
      dplyr::lead(StreakId) != StreakId,
      dplyr::lead(Rank) > max_rank
    )
}

lines_build_lines <- function(years, teams, franchises, max_rank, hot) {
  team_ids <- franchises_franchise_ids_to_team_ids(
    franchises, teams, years)
  lines <- sql_get_lines(years[[1]], years[[2]], team_ids, hot, max_rank) %>%
    lines_remove_branch_descenders(max_rank, hot)
}

lines_get_selected_streak_id <- function(lines_to_streaks, line_id) {
  if (is.null(line_id)) {
    NULL
  } else {
    lines_to_streaks %>%
      dplyr::filter(LineId == line_id) %>%
      dplyr::pull(StreakId)
  }
}

