# We always deal with DT objects through proxies.
# Each DT has a DT_xxx_init function and a DT_xxx_update function.

# DT init functions ----
summary_ui_DT_standings_init <- function(highlight_colors) {
  dummy_data <- tibble::tibble(
    Team=character(0),
    W=integer(0),
    L=integer(0),
    GB=double(0),
    Highlight=logical(0)
  )
  # Note the weird indexing here.  columnDefs uses zero-based
  # indexing, but with the rowname as column zero, and the
  # 'regular' columns indexed starting at 1. But when we set
  # rownames to FALSE, the regular columns are zero-indexed.
  hidden_columns = which(names(dummy_data) == "Highlight") - 1
  DT::datatable(
    dummy_data,
    rownames = FALSE,
    options = list(
      columnDefs = list(list(visible=FALSE, targets=hidden_columns)),
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ),
    selection="none"
  ) %>%
    DT::formatStyle("Highlight",
                    target="row",
                    backgroundColor=DT::styleEqual(TRUE,
                                                   highlight_colors$high)
    )
}

summary_ui_DT_streak_summary_init <- function() {
  dummy_table <- tibble::tibble(
    Dates=character(0),
    Record=character(0),
    `W-L%`=character(0),
    RS=integer(0),
    RA=integer(0),
    `Pyth%`=character(0)
  )
  DT::datatable(
    dummy_table,
    caption = NULL,
    rownames = FALSE,
    options = list(
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ),
    selection="none"
  )
}

summary_ui_DT_game_log_init <- function() {
  dummy_table <- tibble::tibble(
    `Gm#` = integer(0),
    Date = character(0),
    Opp = character(0),
    `W/L` = character(0),
    RS = integer(0),
    RA = integer(0),
    Completion = character(0)
  )
  DT::datatable(
    dummy_table,
    caption = NULL,
    rownames = FALSE,
    options(
      ordering = FALSE, searching = FALSE, pageLength = 15,
      pagingType = "simple",
      lengthChange = FALSE
    ),
    selection="none"
  )
}



# DT update functions ----
summary_ui_DT_standings_update <- function(proxy, streak_info, standings) {
  nickname <- streak_info %>%
    dplyr::pull(Nickname)
  standings <- standings %>%
    dplyr::mutate(Highlight=(Nickname==nickname)) %>%
    dplyr::select(Team=Nickname, W=Wins, L=Losses, GB, Highlight)

  DT::replaceData(proxy, standings, resetPaging = FALSE,
                  rownames = FALSE
  )
}

summary_ui_DT_game_log_update <- function(proxy, game_log_data) {
  DT::replaceData(proxy,
                  game_log_data,
                  resetPaging = TRUE,
                  rownames = FALSE)
}

summary_ui_DT_streak_summary_update <- function(proxy, streaks_summary_data) {
  DT::replaceData(
    proxy,
    streaks_summary_data,
    resetPaging = FALSE,
    rownames = FALSE
  )
}

