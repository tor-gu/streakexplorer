test_that("standings_update_from_game_logs empty update", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")  %>%
      dplyr::filter(Year==1948, League=="NL", Date=="1948-08-25")
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs") %>%
      dplyr::filter(FALSE)
    actual <- standings_update_from_game_logs(lzy_standings, lzy_game_logs) %>%
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

test_that("standings_update_from_game_logs update with incomplete games", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_standings <- dplyr::tbl(mock_conn, "standings")  %>%
      dplyr::filter(Year==1948, League=="NL", Date=="1948-08-25")
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs") %>%
        dplyr::filter(Year==1948, Date=="1948-08-25",
                      Team %in% c("PIT","BRO","BSN","SLN"))
    actual <- standings_update_from_game_logs(lzy_standings, lzy_game_logs) %>%
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
