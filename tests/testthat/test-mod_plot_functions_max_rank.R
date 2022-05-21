test_that("ps_streaks_get_max_rank_simple basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    teams <- c("BOS","CIN")
    min_year <- 1948
    max_year <- 1949
    actual <- ps_streaks_get_max_rank_simple(lzy_streaks, 3, min_year, max_year,
                                             teams)
    dbDisconnect(mock_conn)
  })
  expect_equal(actual, 1985)
})

test_that("ps_streaks_get_max_rank_by_sampling basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    teams <- c("BOS","CIN")
    min_year <- 1948
    max_year <- 1949
    actual <- ps_streaks_get_max_rank_by_sampling(lzy_streaks, 3, min_year,
                                                  max_year, teams, c(20,80), 1.3)
    dbDisconnect(mock_conn)
  })
  expect_equal(actual, 1784)
})

