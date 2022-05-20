

test_that("streaks_get_standings works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- streaks_get_standings(lzy_standings, lzy_game_logs, franchises,
                                    streak)
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

test_that("streaks_get_intensity_range basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    actual <- streaks_get_intensity_range(lzy_streaks, 1948)
    dbDisconnect(mock_conn)
  })
  names(actual) <- NULL
  expect_equal(actual, c(1,101))
})

