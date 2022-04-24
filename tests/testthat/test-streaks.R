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


test_that("streaks_summary_data works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- streaks_summary_data(lzy_game_logs, franchises, streak)
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


test_that("streaks_get_standings works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- streaks_get_standings(lzy_standings, lzy_game_logs, franchises, streak)
    dbDisconnect(mock_conn)
  })
  expected_streak_info <- tibble::tibble(
    League="NL",
    Division="Central",
    Year=2004,
    Team="CIN",
    Start="2004-07-26",
    End="2004-07-31",
    Location="Cincinnati",
    Nickname="Reds"
  )
  expected_standings_before <- tibble::tribble(
    ~Team, ~Wins, ~Losses, ~Ties, ~GB, ~Location,    ~Nickname,
    "SLN", 62,    36,      0,     0,   "St. Louis",  "Cardinals",
    "CHN", 52,    46,      0,    10,   "Chicago",    "Cubs",
    "CIN", 50,    49,      0,  12.5,   "Cincinnati", "Reds",
    "HOU", 49,    49,      0,    13,   "Houston",    "Astros",
    "MIL", 48,    49,      0,  13.5,   "Milwaukee",  "Brewers",
    "PIT", 47,    49,      0,    14,   "Pittsburgh", "Pirates"
  )
  expected_standings_after <- tibble::tribble(
    ~Team, ~Wins, ~Losses, ~Ties, ~GB, ~Location,    ~Nickname,
    "SLN", 66,    37,      0,     0,   "St. Louis",  "Cardinals",
    "CHN", 56,    48,      0,  10.5,   "Chicago",    "Cubs",
    "HOU", 52,    52,      0,  14.5,   "Houston",    "Astros",
    "CIN", 51,    53,      0,  15.5,   "Cincinnati", "Reds",
    "MIL", 50,    53,      0,    16,   "Milwaukee",  "Brewers",
    "PIT", 49,    53,      0,  16.5,   "Pittsburgh", "Pirates"
  )
  expected_standings_final <- tibble::tribble(
    ~Team, ~Wins, ~Losses, ~Ties, ~GB, ~Location,    ~Nickname,
    "SLN", 105,    57,  0,     0,   "St. Louis",  "Cardinals",
    "HOU", 92,     70,  0,    13,   "Houston",    "Astros",
    "CHN", 89,     73,  0,    16,   "Chicago",    "Cubs",
    "CIN", 76,     86,  0,    29,   "Cincinnati", "Reds",
    "PIT", 72,     89,  0,  32.5,   "Pittsburgh", "Pirates",
    "MIL", 67,     94,  0,  37.5,   "Milwaukee",  "Brewers",
  )
  expect_equal(actual$streak_info, expected_streak_info)
  expect_equal(actual$standings_before, expected_standings_before)
  expect_equal(actual$standings_after,  expected_standings_after)
  expect_equal(actual$standings_final,  expected_standings_final)
})
