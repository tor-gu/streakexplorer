test_that("ps_franchise_ids_to_team_ids basic test", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~TeamID, ~FirstSeason, ~FinalSeason,
    "AAA",        "AA1",   1,            10,
    "AAA",        "AA2",   11,           20,
    "AAA",        "AA3",   21,           30,
    "AAA",        "AA4",   31,           NA,
    "BBB",        "BB1",   1,            25,
    "BBB",        "BB2",   26,           NA,
    "CCC",        "CC1",   1,            22,
    "CCC",        "CC2",   23,           33,
    "CCC",        "CC3",   34,           NA,
  )
  franchise_ids <- c("AAA","CCC")
  min_year <- 22
  max_year <- 33
  actual <- ps_franchise_ids_to_team_ids(franchises, franchise_ids,
                                         min_year, max_year)
  expected <-  c("AA3","AA4","CC1","CC2")
  expect_equal(actual, expected)
})

test_that("ps_streaks_get_max_rank_simple basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    teams <- c("BOS","CIN")
    min_year <- 1948
    max_year <- 1949
    actual <- ps_streaks_get_max_rank_simple(lzy_streaks, 3, min_year, max_year,
                                             teams)
    dbDisconnect(mock_conn)
  })
  expect_equal(actual, 1985)
})

test_that("ps_streaks_get_max_rank_by_sampling basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    teams <- c("BOS","CIN")
    min_year <- 1948
    max_year <- 1949
    actual <- ps_streaks_get_max_rank_by_sampling(lzy_streaks, 3, min_year,
                                                  max_year, teams, c(20,80), 1.3)
    dbDisconnect(mock_conn)
  })
  expect_equal(actual, 1784)
})

test_that("ps_lines_highlight basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_concordances <- dplyr::tbl(mock_conn, "hot_streaks_concordances")
    lzy_lines_to_streaks <- dplyr::tbl(mock_conn,
                                       "hot_streaks_lines_to_streaks")
    lzy_lines <- dplyr::tbl(mock_conn, "hot_streaks_lines")
    lines <- lzy_lines %>%
      dplyr::filter(Year %in% 1948:1949, Rank <= 80) %>%
      dplyr::filter(IntensityLevel %in% c(70,80,90,100)) %>%
      dplyr::collect()
    line_id <- 6298
    lines <- ps_lines_highlight(lines, lzy_concordances, lzy_lines_to_streaks,
                                line_id)
    dbDisconnect(mock_conn)
  })
  actual <- lines %>% dplyr::select(Year, Team, IntensityLevel, line_type)
  expected <- tibble::tribble(
    ~Year, ~Team, ~IntensityLevel, ~line_type,
    1948,  "BOS", 90,              "identical",
    1948,  "BOS", 100,             "identical",
    1948,  "BOS", 80,              "season",
    1948,  "BOS", 70,              "related",
    1949,  "BOS", 100,             "base",
    1949,  "BOS", 70,              "base",
    1949,  "BOS", 80,              "base",
  )
  expect_equal(actual, expected)
})

test_that("ps_build_lines basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_lines <- dplyr::tbl(mock_conn, "hot_streaks_lines")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    lines <- ps_build_lines(lzy_lines, 1948, 1948, c("BOS","CIN"),
                            franchises, 80, 1)
    dbDisconnect(mock_conn)
  })
  expect_equal(nrow(lines), 107)
  expect_equal(unique(lines$LineId),
               c(6298, 6311, 6312, 6317, 6320, 6324, 6325, 6327))
})

test_that("ps_build_lines detects left-most line with hot streaks", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_lines <- dplyr::tbl(mock_conn, "hot_streaks_lines")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    lines <- ps_build_lines(lzy_lines, 1954, 1954, "NYA",
                            franchises, 80, 1)
    dbDisconnect(mock_conn)
  })
  expect_equal(nrow(lines), 109)
  expect_equal(unique(lines$LineId),
               c(39806, 39822, 39825, 39826, 39834, 39838, 39840, 39842, 39843))
})

test_that("ps_build_lines detects left-most line with cold streaks", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_lines <- dplyr::tbl(mock_conn, "cold_streaks_lines")
    franchises <- dplyr::tbl(mock_conn, "franchises") %>% dplyr::collect()
    lines <- ps_build_lines(lzy_lines, 1949, 1949, "WS1",
                            franchises, 80, 101)
    dbDisconnect(mock_conn)
  })
  expect_equal(nrow(lines), 114)
  expect_equal(unique(lines$LineId),
               c(66267, 66268, 66277, 66285, 66296, 66298, 66305, 66307, 66309,
                 66310))
})

test_that("ps_lines_to_related_streak basic test", {
  with_mock_db({
    mock_conn <- suppressWarnings(
      dbConnect(RMySQL::MySQL(), dbname="streak_explorer_data")
    )
    lzy_lines_to_streaks <- dplyr::tbl(mock_conn, "hot_streaks_lines_to_streaks")
    lzy_streaks <- dplyr::tbl(mock_conn, "hot_streaks")
    lzy_game_logs <- dplyr::tbl(mock_conn, "game_logs")
    line_id <- 37
    actual <- ps_lines_to_related_streak(lzy_lines_to_streaks, lzy_streaks,
                                         lzy_game_logs, line_id)
    dbDisconnect(mock_conn)
  })
  expected <- tibble::tibble(
    Year=1997, Team="ANA", LoIndex=10, HiIndex=158,
    StartDate=lubridate::ymd("1997-04-13"),
    EndDate=lubridate::ymd("1997-09-24")
  )
  expect_equal(actual, expected)
})



