test_that("ss_streaks_summary_data works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- ss_streaks_summary_data(lzy_game_logs, franchises,
                                                  streak)
    dbDisconnect(mock_conn)
  })
  expected_caption <- "2004 Cincinnati Reds, Games 100-104"
  expected_data <- tibble::tibble(
    Dates="7/26 - 7/31",
    Record="1-4",
    `W-L%`=".200",
    RS=19,
    RA=36,
    `Pyth%`=".218"
  )
  expect_equal(actual$caption, expected_caption)
  expect_equal(actual$data, expected_data)
})

test_that("ss_streaks_get_game_log basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    streak_game_log <- ss_streaks_get_game_log(lzy_game_logs,
                                                           streak)
    dbDisconnect(mock_conn)
  })
  actual_ids <- streak_game_log %>% dplyr::pull(Id)
  expected_ids <- 208768:208773
  expect_equal(actual_ids, expected_ids)
})

test_that("ss_streaks_game_log_data works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- ss_streaks_game_log_data(lzy_game_logs, streak)
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


