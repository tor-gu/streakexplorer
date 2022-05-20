test_that("ss_streak_summary_data works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- ss_streak_summary_data(lzy_game_logs, franchises,
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

test_that("ss_streak_get_game_log basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    streak_game_log <- ss_streak_get_game_log(lzy_game_logs,
                                                           streak)
    dbDisconnect(mock_conn)
  })
  actual_ids <- streak_game_log %>% dplyr::pull(Id)
  expected_ids <- 208768:208773
  expect_equal(actual_ids, expected_ids)
})

test_that("ss_streak_game_log_data works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- ss_streak_game_log_data(lzy_game_logs, streak)
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

test_that("ss_get_standings_for_streak works with 'completions'", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    streak <- list(Year=2004, Team="CIN", LoIndex=100, HiIndex=105)
    actual <- ss_get_standings_for_streak(lzy_standings, lzy_game_logs, franchises,
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

test_that("ss_get_standings_by_season_game_id handles games before", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    division <- list(Year=1990, League="AL", Division="East")
    actual <- ss_get_standings_by_season_game_id(lzy_standings, lzy_game_logs,
                                                 division, 123, TRUE)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tribble(
    ~Year, ~League, ~Division, ~Date,       ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    1990,  "AL",    "East",    "1990-04-19","TOR", 6,     4,       0,     0,
    1990,  "AL",    "East",    "1990-04-19","BOS", 5,     4,       0,     0.5,
    1990,  "AL",    "East",    "1990-04-19","NYA", 4,     3,       0,     0.5,
    1990,  "AL",    "East",    "1990-04-19","MIL", 4,     4,       0,     1,
    1990,  "AL",    "East",    "1990-04-19","BAL", 4,     5,       0,     1.5,
    1990,  "AL",    "East",    "1990-04-19","CLE", 3,     5,       0,     2,
    1990,  "AL",    "East",    "1990-04-19","DET", 4,     6,       0,     2
  )
  expect_equal(actual, expected)
})

test_that("ss_get_standings_by_season_game_id handles games after", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    division <- list(Year=1990, League="AL", Division="East")
    actual <- ss_get_standings_by_season_game_id(lzy_standings, lzy_game_logs,
                                                 division, 123, FALSE)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tribble(
    ~Year, ~League, ~Division, ~Date,       ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    1990,  "AL",    "East",    "1990-04-20","TOR", 7,     4,       0,     0,
    1990,  "AL",    "East",    "1990-04-20","MIL", 5,     4,       0,     1,
    1990,  "AL",    "East",    "1990-04-20","BAL", 5,     5,       0,     1.5,
    1990,  "AL",    "East",    "1990-04-20","BOS", 5,     5,       0,     1.5,
    1990,  "AL",    "East",    "1990-04-20","NYA", 4,     4,       0,     1.5,
    1990,  "AL",    "East",    "1990-04-20","CLE", 3,     5,       0,     2.5,
    1990,  "AL",    "East",    "1990-04-20","DET", 4,     7,       0,     3
  )
  expect_equal(actual, expected)
})

test_that("ss_get_standings_by_season_game_id game 1 of double header", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    division <- list(Year=1948, League="AL", Division=NA)
    standings_before <- ss_get_standings_by_season_game_id(lzy_standings,
                                                           lzy_game_logs,
                                                           division, 1, TRUE) %>%
      dplyr::select(Date:GB)
    standings_after <- ss_get_standings_by_season_game_id(lzy_standings,
                                                          lzy_game_logs,
                                                          division, 1, FALSE) %>%
      dplyr::select(Date:GB)
    dbDisconnect(mock_conn)
  })
  # In 1948, games 1 and 2 were a doubleheader between BOS and PHA
  expected_standings_before <- tibble::tribble(
    ~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    "1948-04-18", "SLA", 0,     0,       0,     0,
    "1948-04-18", "BOS", 0,     0,       0,     0,
    "1948-04-18", "CHA", 0,     0,       0,     0,
    "1948-04-18", "CLE", 0,     0,       0,     0,
    "1948-04-18", "DET", 0,     0,       0,     0,
    "1948-04-18", "WS1", 0,     0,       0,     0,
    "1948-04-18", "NYA", 0,     0,       0,     0,
    "1948-04-18", "PHA", 0,     0,       0,     0,
  )
  expected_standings_after <- tibble::tribble(
    ~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    "1948-04-18", "PHA", 1,     0,       0,     0,
    "1948-04-18", "SLA", 0,     0,       0,     0.5,
    "1948-04-18", "CHA", 0,     0,       0,     0.5,
    "1948-04-18", "CLE", 0,     0,       0,     0.5,
    "1948-04-18", "DET", 0,     0,       0,     0.5,
    "1948-04-18", "WS1", 0,     0,       0,     0.5,
    "1948-04-18", "NYA", 0,     0,       0,     0.5,
    "1948-04-18", "BOS", 0,     1,       0,     1,
  )
  expect_equal(standings_before, expected_standings_before)
  expect_equal(standings_after, expected_standings_after)
})

test_that("ss_get_standings_by_season_game_id game 2 of double header", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    division <- list(Year=1948, League="AL", Division=NA)
    standings_before <- ss_get_standings_by_season_game_id(lzy_standings,
                                                           lzy_game_logs,
                                                           division, 2, TRUE) %>%
      dplyr::select(Date:GB)
    standings_after <- ss_get_standings_by_season_game_id(lzy_standings,
                                                          lzy_game_logs,
                                                          division, 2, FALSE) %>%
      dplyr::select(Date:GB)
    dbDisconnect(mock_conn)
  })
  # In 1948, games 1 and 2 were a doubleheader between BOS and PHA
  expected_standings_before <- tibble::tribble(
    ~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    "1948-04-18", "PHA", 1,     0,       0,     0,
    "1948-04-18", "SLA", 0,     0,       0,     0.5,
    "1948-04-18", "CHA", 0,     0,       0,     0.5,
    "1948-04-18", "CLE", 0,     0,       0,     0.5,
    "1948-04-18", "DET", 0,     0,       0,     0.5,
    "1948-04-18", "WS1", 0,     0,       0,     0.5,
    "1948-04-18", "NYA", 0,     0,       0,     0.5,
    "1948-04-18", "BOS", 0,     1,       0,     1,
  )
  expected_standings_after <- tibble::tribble(
    ~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    "1948-04-19", "PHA", 2,     0,       0,     0,
    "1948-04-19", "NYA", 1,     0,       0,     0.5,
    "1948-04-19", "SLA", 0,     0,       0,     1,
    "1948-04-19", "CHA", 0,     0,       0,     1,
    "1948-04-19", "CLE", 0,     0,       0,     1,
    "1948-04-19", "DET", 0,     0,       0,     1,
    "1948-04-19", "WS1", 0,     1,       0,     1.5,
    "1948-04-19", "BOS", 0,     2,       0,     2,
  )
  expect_equal(standings_before, expected_standings_before)
  expect_equal(standings_after, expected_standings_after)
})

