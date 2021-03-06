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

test_that("ss_update_standings_from_game_logs empty update", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")  %>%
      dplyr::filter(Year==1948, League=="NL", Date=="1948-08-25")
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs") %>%
      dplyr::filter(FALSE)
    actual <- ss_update_standings_from_game_logs(lzy_standings, lzy_game_logs) %>%
      dplyr::select(-Division)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tribble(
    ~Year, ~League,~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    1948,  "NL",   "1948-08-25", "BSN", 68,    49,      1,     0,
    1948,  "NL",   "1948-08-25", "BRO", 61,    51,      1,     4.5,
    1948,  "NL",   "1948-08-25", "SLN", 63,    53,      1,     4.5,
    1948,  "NL",   "1948-08-25", "NY1", 60,    53,      1,     6,
    1948,  "NL",   "1948-08-25", "PIT", 58,    52,      2,     6.5,
    1948,  "NL",   "1948-08-25", "PHI", 52,    63,      1,     15,
    1948,  "NL",   "1948-08-25", "CIN", 50,    67,      0,     18,
    1948,  "NL",   "1948-08-25", "CHN", 46,    70,      1,     21.5,

  )
  expect_equal(actual, expected)
})

test_that("ss_update_standings_from_game_logs update with incomplete games", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")  %>%
      dplyr::filter(Year==1948, League=="NL", Date=="1948-08-25")
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs") %>%
      dplyr::filter(Year==1948, Date=="1948-08-25",
                    Team %in% c("PIT","BRO","BSN","SLN"))
    actual <- ss_update_standings_from_game_logs(lzy_standings, lzy_game_logs) %>%
      dplyr::select(-Division)
    dbDisconnect(mock_conn)
  })
  # PIT v BRO was incomplete.
  # BSN beat SLN, so those standings will be affected
  expected <- tibble::tribble(
    ~Year, ~League,~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    1948,  "NL",   "1948-08-25", "BSN", 69,    49,      1,     0,
    1948,  "NL",   "1948-08-25", "BRO", 61,    51,      1,     5,
    1948,  "NL",   "1948-08-25", "SLN", 63,    54,      1,     5.5,
    1948,  "NL",   "1948-08-25", "NY1", 60,    53,      1,     6.5,
    1948,  "NL",   "1948-08-25", "PIT", 58,    52,      2,     7,
    1948,  "NL",   "1948-08-25", "PHI", 52,    63,      1,     15.5,
    1948,  "NL",   "1948-08-25", "CIN", 50,    67,      0,     18.5,
    1948,  "NL",   "1948-08-25", "CHN", 46,    70,      1,     22,
  )
  expect_equal(actual, expected)
})

test_that("ss_get_final_standings handles no-division standings", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    division <- franchises %>%
      ss_get_division_by_team_year("BOS", 1950)
    actual <- ss_get_final_standings(lzy_standings,
                                     division$division) %>%
      dplyr::select(-Division)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tribble(
    ~Year, ~League,~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    1950,  "AL",   "1950-10-01", "NYA", 98,    56,      1,     0,
    1950,  "AL",   "1950-10-01", "DET", 95,    59,      3,     3,
    1950,  "AL",   "1950-10-01", "BOS", 94,    60,      0,     4,
    1950,  "AL",   "1950-10-01", "CLE", 92,    62,      1,     6,
    1950,  "AL",   "1950-10-01", "WS1", 67,    87,      1,     31,
    1950,  "AL",   "1950-10-01", "CHA", 60,    94,      2,     38,
    1950,  "AL",   "1950-10-01", "SLA", 58,    96,      0,     40,
    1950,  "AL",   "1950-10-01", "PHA", 52,    102,     0,     46,
  )
  expect_equal(actual, expected)
})

test_that("ss_get_final_standings handles division standings", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    division <- franchises %>%
      ss_get_division_by_team_year("BOS", 1990)
    actual <- ss_get_final_standings(lzy_standings,
                                     division$division) %>%
      dplyr::select(-Division)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tribble(
    ~Year, ~League,~Date,        ~Team, ~Wins, ~Losses, ~Ties, ~GB,
    1990,  "AL",   "1990-10-03", "BOS", 88,    74,      0,     0,
    1990,  "AL",   "1990-10-03", "TOR", 86,    76,      0,     2,
    1990,  "AL",   "1990-10-03", "DET", 79,    83,      0,     9,
    1990,  "AL",   "1990-10-03", "CLE", 77,    85,      0,     11,
    1990,  "AL",   "1990-10-03", "BAL", 76,    85,      0,     11.5,
    1990,  "AL",   "1990-10-03", "MIL", 74,    88,      0,     14,
    1990,  "AL",   "1990-10-03", "NYA", 67,    95,      0,     21,
  )
  expect_equal(actual, expected)
})

test_that("ss_get_same_day_team_games_lzy handles games before", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    games_before_1 <- ss_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 1, before=TRUE) %>%
      dplyr::collect()
    games_before_2 <- ss_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 2, before=TRUE) %>%
      dplyr::collect()
    dbDisconnect(mock_conn)
  })
  # In 1948, games 1 and 2 were a doubleheader between BOS and PHA
  expect_equal(nrow(games_before_1), 0)
  expect_equal(games_before_2$SeasonGameId, c(1,1))
})

test_that("ss_get_same_day_team_games_lzy handles games after", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    games_before_1 <- ss_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 1, before=FALSE) %>%
      dplyr::collect()
    games_before_2 <- ss_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 2, before=FALSE) %>%
      dplyr::collect()
    dbDisconnect(mock_conn)
  })
  # In 1948, games 1 and 2 were a doubleheader between BOS and PHA
  expect_equal(games_before_1$SeasonGameId, c(2,2))
  expect_equal(nrow(games_before_2), 0)
})
