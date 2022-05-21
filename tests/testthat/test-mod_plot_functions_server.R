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

test_that("ps_get_related_streak_ids works with an inner-most streak", {
  with_mock_db({
    conn <- dbConnect(RMariaDB::MariaDB(), dbname="streak_explorer_data")
    lzy_concordances <- dplyr::tbl(conn, "hot_streaks_concordances")
    actual <- ps_get_related_streak_ids(37, lzy_concordances)
    dbDisconnect(conn)
  })
  expected <- c(1,2,3,5,11,14,18,22,25,30,37)
  expect_equal(actual, expected)
})

test_that("ps_get_related_streak_ids works with an outer-most streak", {
  with_mock_db({
    conn <- dbConnect(RMariaDB::MariaDB(), dbname="streak_explorer_data")
    lzy_concordances <- dplyr::tbl(conn, "hot_streaks_concordances")
    actual <- ps_get_related_streak_ids(115, lzy_concordances)
    dbDisconnect(conn)
  })
  expected <- 115:147
  expect_equal(actual, expected)
})
test_that("ps_get_related_streak_ids works with an internal streak", {
  with_mock_db({
    conn <- dbConnect(RMariaDB::MariaDB(), dbname="streak_explorer_data")
    lzy_concordances <- dplyr::tbl(conn, "hot_streaks_concordances")
    actual <- ps_get_related_streak_ids(73, lzy_concordances)
    dbDisconnect(conn)
  })
  expected <- c(38,39,40,42,48,53,65,68,73,74,75)
  expect_equal(actual, expected)
})




