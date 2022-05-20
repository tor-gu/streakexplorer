
test_that("standings_get_final_standings handles no-division standings", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    division <- franchises %>%
      franchises_get_division_by_team_year("BOS", 1950)
    actual <- standings_get_final_standings(lzy_standings,
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

test_that("standings_get_final_standings handles division standings", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    division <- franchises %>%
      franchises_get_division_by_team_year("BOS", 1990)
    actual <- standings_get_final_standings(lzy_standings,
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

test_that("standings_get_same_day_team_games_lzy handles games before", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    games_before_1 <- standings_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 1, before=TRUE) %>%
      dplyr::collect()
    games_before_2 <- standings_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 2, before=TRUE) %>%
      dplyr::collect()
    dbDisconnect(mock_conn)
  })
  # In 1948, games 1 and 2 were a doubleheader between BOS and PHA
  expect_equal(nrow(games_before_1), 0)
  expect_equal(games_before_2$SeasonGameId, c(1,1))
})

test_that("standings_get_same_day_team_games_lzy handles games after", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    games_before_1 <- standings_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 1, before=FALSE) %>%
      dplyr::collect()
    games_before_2 <- standings_get_same_day_team_games_lzy(
      lzy_game_logs, 1948, 2, before=FALSE) %>%
      dplyr::collect()
    dbDisconnect(mock_conn)
  })
  # In 1948, games 1 and 2 were a doubleheader between BOS and PHA
  expect_equal(games_before_1$SeasonGameId, c(2,2))
  expect_equal(nrow(games_before_2), 0)
})

