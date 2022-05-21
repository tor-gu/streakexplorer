# Utility functions to load the lazy queries ----
lzy_lines <- function(db_pool, hot) {
  if (hot) {
    dplyr::tbl(db_pool, "hot_streaks_lines")
  } else {
    dplyr::tbl(db_pool, "cold_streaks_lines")
  }
}

lzy_streaks <- function(db_pool, hot) {
  if (hot) {
    dplyr::tbl(db_pool, "hot_streaks")
  } else {
    dplyr::tbl(db_pool, "cold_streaks")
  }
}

lzy_lines_to_streaks <- function(db_pool, hot) {
  if (hot) {
    dplyr::tbl(db_pool, "hot_streaks_lines_to_streaks")
  } else {
    dplyr::tbl(db_pool, "cold_streaks_lines_to_streaks")
  }
}

lzy_concordances <- function(db_pool, hot) {
  if (hot) {
    dplyr::tbl(db_pool, "hot_streaks_concordances")
  } else {
    dplyr::tbl(db_pool, "cold_streaks_concordances")
  }
}






