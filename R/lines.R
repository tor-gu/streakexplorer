get_related_lines <- function(line_id, lines_to_streaks, concordances) {
  related_streak_ids <- lines_to_streaks %>%
    dplyr::filter(LineId==line_id) %>%
    dplyr::pull(StreakId) %>%
    purrr::map(~som_get_related_streak_ids(concordances, .)) %>%
    unlist(recursive = FALSE) %>% unique()
  lines_to_streaks %>%
    dplyr::filter(StreakId %in% related_streak_ids) %>%
    pull(LineId)
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
    row <- lines %>% filter(LineId==id) %>% head(1)
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
          line_type=dplyr::if_else(LineId %in% related_line_ids, "related",
                                   line_type),
          line_width=dplyr::if_else(LineId %in% related_line_ids, 3, 1)
        ) %>%
        dplyr::mutate(
          line_colored=dplyr::if_else(LineId == id, 3, line_colored),
          line_type=dplyr::if_else(LineId == id, "identical", line_type),
        )
    }
  }
  result
}

lines_remove_nubs <- function(lines) {
  # This is a cleanup function to be applied after a Rank filter has been
  # applied.  The idea is to remove the initial element of a line when
  # that element is from a different streak, and there is a gap between the
  # initial element and the next one (because the intermediate ones were
  # removed in a Rank filter)
  lines %>%
    dplyr::arrange(LineId, IntensityLevel) %>%
    dplyr::filter(dplyr::lead(LineId) != LineId |
                    dplyr::lead(StreakId) == StreakId |
                    dplyr::lead(IntensityLevel) - IntensityLevel == 1 |
                    is.na(dplyr::lead(IntensityLevel)))
}


add_descenders <- function(initial_rank_filter, initial_filter) {
  filtered_left <- initial_filter %>%
    mutate(PrevIntensityLevel=IntensityLevel - 1) %>%
    semi_join(initial_rank_filter,
              by=c("StreakId", "PrevIntensityLevel"="IntensityLevel")) %>%
    select(-PrevAdjLevel)

  filtered_right <- initial_filter %>%
    mutate(NextIntensityLevel=IntensityLevel + 1) %>%
    semi_join(initial_rank_filter,
              by=c("StreakId", "NextIntensityLevel"="IntensityLevel")) %>%
    select(-NextIntensityLevel)

  rbind(initial_rank_filter, filtered_left, filtered_right) %>% unique()
}
