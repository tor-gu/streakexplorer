test_that("lines_get_related_lines works", {
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      37,
    37,     37,
    37,     100,
    1,      100,
    2,      2,
  )
  lines_to_streaks <- tibble::tribble(
    ~LineId, ~StreakId,
    1001,     1,
    1037,     37,
    1100,     100,
    1002,     2,
  )
  line_id <- 1037

  actual <- lines_get_related_lines(line_id, lines_to_streaks, concordances)
  expected <- c(1001, 1037, 1100)
  expect_equal(actual, expected)
})

test_that("lines_get_related_lines handles multiple lines per streak", {
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      37,
    37,     37,
    37,     100,
    1,      100,
    2,      2,
  )
  lines_to_streaks <- tibble::tribble(
    ~LineId, ~StreakId,
    1001,     1,
    1037,     37,
    1100,     100,
    1002,     2,
  )
  line_id <- 1037

  actual <- lines_get_related_lines(line_id, lines_to_streaks, concordances)
  expected <- c(1001, 1037, 1100)
  expect_equal(actual, expected)
})



test_that("lines_get_selected_streak basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_lines_to_streaks <- dplyr::tbl(mock_conn, "hot_streaks_lines_to_streaks")
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    line_id <- 37
    actual <- lines_get_selected_streak(lzy_lines_to_streaks, lzy_streaks,
                                        lzy_game_logs, line_id)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tibble(
    Year=1997, Team="ANA", LoIndex=10, HiIndex=158,
    StartDate=lubridate::ymd("1997-04-13"),
    EndDate=lubridate::ymd("1997-09-24")
  )
  expect_equal(actual, expected)
})


