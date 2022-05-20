test_that("lines_get_related_lines works", {
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

  actual <- lines_get_related_lines(line_id, lines_to_streaks, concordances)
  expected <- c(1001, 1037, 1100)
  expect_equal(actual, expected)
})

test_that("lines_get_related_lines handles multiple lines per streak", {
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

  actual <- lines_get_related_lines(line_id, lines_to_streaks, concordances)
  expected <- c(1001, 1037, 1100)
  expect_equal(actual, expected)
})



