test_that("streaks_get_related_streak_id works with an inner-most streak", {
  with_mock_db({
    conn <- dbConnect(RMariaDB::MariaDB(), dbname="streak_explorer_data")
    lzy_concordances <- dplyr::tbl(conn, "hot_streaks_concordances")
    actual <- streaks_get_related_streak_ids(37, lzy_concordances)
    dbDisconnect(conn)
  })
  expected <- c(1,2,3,5,11,14,18,22,25,30,37)
  expect_equal(actual, expected)
})

test_that("streaks_get_related_streak_id works with an outer-most streak", {
  with_mock_db({
    conn <- dbConnect(RMariaDB::MariaDB(), dbname="streak_explorer_data")
    lzy_concordances <- dplyr::tbl(conn, "hot_streaks_concordances")
    actual <- streaks_get_related_streak_ids(115, lzy_concordances)
    dbDisconnect(conn)
  })
  expected <- 115:147
  expect_equal(actual, expected)
})
test_that("streaks_get_related_streak_id works with an internal streak", {
  with_mock_db({
    conn <- dbConnect(RMariaDB::MariaDB(), dbname="streak_explorer_data")
    lzy_concordances <- dplyr::tbl(conn, "hot_streaks_concordances")
    actual <- streaks_get_related_streak_ids(73, lzy_concordances)
    dbDisconnect(conn)
  })
  expected <- c(38,39,40,42,48,53,65,68,73,74,75)
  expect_equal(actual, expected)
})

test_that("streaks_game_log_data works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- streaks_game_log_data(lzy_game_logs, streak)
    dbDisconnect(mock_conn)
  })
  expected_caption <- "Game Log"
  expected_data <- tibble::tribble(
    ~`Gm#`, ~Date, ~Opp, ~`W/L`, ~RS, ~RA, ~Completion,
    100,    "7/26","@SLN", "L",  6,   9,   NA,
    101,    "7/27","@SLN", "L",  0,   6,   NA,
    102,    "7/28","@SLN", "L",  10,  11,  NA,
    103,    "7/30","@HOU", NA,   3,   1,   "Completed 7/31, W 3-2",
    103,    "7/31","@HOU", "W",  0,   1,   "Final score 3-2, began 7/30",
    104,    "7/31","@HOU", "L",  0,   8,   NA,
  )
  expect_equal(actual$caption, expected_caption)
  expect_equal(actual$data, expected_data)
})

