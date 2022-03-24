standings_DT_init <- function() {
  dummy_data <- tibble::tibble(
    Team=character(0),
    Wins=integer(0),
    Losses=integer(0),
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
                    backgroundColor=DT::styleEqual(TRUE, "red")
    )
}

streak_summary_DT_init <- function() {
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

game_log_DT_init <- function() {
  dummy_table <- tibble::tibble(
    `Gm#` = integer(0),
    Date = character(0),
    Opp = "X",
    `W/L` = character(0),
    RS = integer(0),
    RA = integer(0),
    Completion = logical(0)
  )
  DT::datatable(
    dummy_table,
    caption = NULL,
    rownames = FALSE,
    options(
      ordering = FALSE, searching = FALSE, pageLength = 15,
      #paging = nrow(game_log$data) > 15,
      pagingType = "simple",
      lengthChange = FALSE
    ),
    selection="none"
  )
}

standings_DT_update <- function(proxy, streak_info, standings) {
  standings <- standings %>%
    dplyr::mutate(Highlight=(Nickname==streak_info$Nickname)) %>%
    dplyr::select(Team=Nickname, W=Wins, L=Losses, GB, Highlight)

  DT::replaceData(proxy, standings, resetPaging = FALSE,
                  rownames = FALSE
  )
}

