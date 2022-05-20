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


