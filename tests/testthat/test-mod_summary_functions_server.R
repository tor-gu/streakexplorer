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


test_that("ss_franchises_by_season handles basic scenarios", {
  franchises <- tibble::tribble(
    ~Id, ~FirstSeason, ~FinalSeason,
    1,   0,            1,
    2,   0,            37,
    3,   0,            100,
    4,   0,            NA,
    5,  37,            37,
    6,  37,            100,
    7,  37,            NA,
    8,  100,           100,
    0,  100,           NA
  )

  actual <- ss_franchises_by_season(franchises, 37)
  expected <- franchises[c(2, 3, 4, 5, 6, 7), ]
  expect_equal(actual, expected)
})

test_that("ss_get_division_by_team_year handles division", {
  franchises <- tibble::tribble(
    ~TeamID, ~League, ~Division, ~Location, ~Nickname, ~FirstSeason, ~FinalSeason,
    "AA1",   "AL",    "EAST",    "AA1 Loc", "AA1 Team", 1,           100,
    "AA2",   "AL",    "EAST",    "AA2 Loc", "AA2 Team", 1,           NA,
    "BB1",   "AL",    "WEST",    "BB1 Loc", "BB1 Team", 1,           100,
    "BB2",   "AL",    NA,        "BB2 Loc", "BB2 Team", 1,           100,
    "BB3",   "NL",    "EAST",    "BB3 Loc", "BB3 Team", 1,           100,
    "BB4",   "NL",    "EAST",    "BB4 Loc", "BB4 Team", 1,           100,
    "BB5",   "AL",    "EAST",    "BB5 Loc", "BB5 Team", 1,           36,
    "BB6",   "AL",    "EAST",    "BB6 Loc", "BB6 Team", 38,          100,
  )
  actual <- ss_get_division_by_team_year(franchises, "AA1", 37)
  expected <- list(
    division=tibble::tibble(League="AL", Division="EAST", Year=37 ),
    teams=tibble::tribble(
      ~TeamID, ~Location, ~Nickname,
      "AA1",   "AA1 Loc", "AA1 Team",
      "AA2",   "AA2 Loc", "AA2 Team",
    )
  )
  expect_equal(actual, expected)
})


test_that("ss_get_division_by_team_year handles NA division", {
  franchises <- tibble::tribble(
    ~TeamID, ~League, ~Division, ~Location, ~Nickname, ~FirstSeason, ~FinalSeason,
    "AA1",   "AL",    NA,        "AA1 Loc", "AA1 Team", 1,           100,
    "AA2",   "AL",    NA,        "AA2 Loc", "AA2 Team", 1,           NA,
    "BB1",   "AL",    "WEST",    "BB1 Loc", "BB1 Team", 1,           100,
    "BB2",   "AL",    "EAST",    "BB2 Loc", "BB2 Team", 1,           100,
    "BB3",   "NL",    "EAST",    "BB3 Loc", "BB3 Team", 1,           100,
    "BB4",   "NL",    "EAST",    "BB4 Loc", "BB4 Team", 1,           100,
    "BB5",   "AL",    "EAST",    "BB5 Loc", "BB5 Team", 1,           36,
    "BB6",   "AL",    "EAST",    "BB6 Loc", "BB6 Team", 38,          100,
  )
  actual <- ss_get_division_by_team_year(franchises, "AA1", 37)
  expected <- list(
    division=tibble::tibble(League="AL", Division=NA_character_, Year=37 ),
    teams=tibble::tribble(
      ~TeamID, ~Location, ~Nickname,
      "AA1",   "AA1 Loc", "AA1 Team",
      "AA2",   "AA2 Loc", "AA2 Team",
    )
  )
  expect_equal(actual, expected)
})

