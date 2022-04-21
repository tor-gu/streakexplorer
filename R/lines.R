lines_get_related_lines <- function(line_id, lzy_lines_to_streaks,
                                    lzy_concordances) {
  related_streak_ids <- lzy_lines_to_streaks %>%
    dplyr::filter(LineId == line_id) %>%
    dplyr::pull(StreakId) %>%
    purrr::map(streaks_get_related_streak_id, lzy_concordances) %>%
    unlist(recursive = FALSE) %>%
    unique()
  lzy_lines_to_streaks %>%
    dplyr::filter(StreakId %in% related_streak_ids) %>%
    dplyr::pull(LineId)
}


lines_highlight <- function(lines, lzy_concordances, lzy_lines_to_streaks,
                            id = NULL) {
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
        id, lzy_lines_to_streaks,
        lzy_concordances
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

lines_build_lines <- function(lzy_lines, years, teams, franchises, max_rank, hot) {
  team_ids <- franchises_franchise_ids_to_team_ids(
    franchises, teams, years)
  min_year <- years[[1]]
  max_year <- years[[2]]
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
    dplyr::collect() %>%
    # Remove any "descenders"
    lines_remove_branch_descenders(max_rank, hot)
}


lines_get_selected_streak <- function(lzy_lines_to_streaks, lzy_streaks,
                                      lzy_game_logs, line_id) {
  if (is.null(line_id)) {
    NULL
  } else {
    # We will only need a few fields from the game logs.
    lzy_game_logs <- lzy_game_logs %>%
      dplyr::select(Year,Team,GameIndex,Date)
    # Now build the query
    lzy_lines_to_streaks %>%
      dplyr::filter(LineId==line_id) %>%
      dplyr::left_join(lzy_streaks, by="StreakId") %>%
      dplyr::select(Year, Team, LoIndex, HiIndex) %>%
      dplyr::left_join(lzy_game_logs,
                       by=c("Year","Team","LoIndex"="GameIndex")) %>%
      dplyr::left_join(lzy_game_logs,
                       by=c("Year","Team","HiIndex"="GameIndex")) %>%
      head(1) %>%
      dplyr::collect() %>%
      dplyr::mutate(StartDate = lubridate::as_date(Date.x)) %>%
      dplyr::mutate(EndDate = lubridate::as_date(Date.y)) %>%
      dplyr::select(Year, Team, LoIndex, HiIndex, StartDate, EndDate)
  }
}

