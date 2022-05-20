# Utility functions to load the lazy queries ----
lzy_lines <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks_lines")
  } else {
    dplyr::tbl(se_pool, "cold_streaks_lines")
  }
}

lzy_streaks <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks")
  } else {
    dplyr::tbl(se_pool, "cold_streaks")
  }
}

lzy_lines_to_streaks <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks_lines_to_streaks")
  } else {
    dplyr::tbl(se_pool, "cold_streaks_lines_to_streaks")
  }
}

lzy_concordances <- function(hot) {
  if (hot) {
    dplyr::tbl(se_pool, "hot_streaks_concordances")
  } else {
    dplyr::tbl(se_pool, "cold_streaks_concordances")
  }
}

# Main server functions ----






