

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

