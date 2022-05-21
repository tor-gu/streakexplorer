

test_that("app_get_intensity_range basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    actual <- app_get_intensity_range(lzy_streaks, 1948)
    dbDisconnect(mock_conn)
  })
  names(actual) <- NULL
  expect_equal(actual, c(1,101))
})

