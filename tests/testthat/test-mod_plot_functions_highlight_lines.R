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


test_that("ps_get_related_lines works", {
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      37,
    37,     37,
    37,     100,
    1,      100,
    2,      2,
  )
  lines_to_streaks <- tibble::tribble(
    ~LineId, ~StreakId,
    1001,     1,
    1037,     37,
    1100,     100,
    1002,     2,
  )
  line_id <- 1037

  actual <- ps_get_related_lines(line_id, lines_to_streaks, concordances)
  expected <- c(1001, 1037, 1100)
  expect_equal(actual, expected)
})

test_that("ps_get_related_lines handles multiple lines per streak", {
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      37,
    37,     37,
    37,     100,
    1,      100,
    2,      2,
  )
  lines_to_streaks <- tibble::tribble(
    ~LineId, ~StreakId,
    1001,     1,
    1037,     37,
    1100,     100,
    1002,     2,
  )
  line_id <- 1037

  actual <- ps_get_related_lines(line_id, lines_to_streaks, concordances)
  expected <- c(1001, 1037, 1100)
  expect_equal(actual, expected)
})

