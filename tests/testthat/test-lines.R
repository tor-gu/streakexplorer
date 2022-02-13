test_that("numeric_right_segment_plus1 handles simple case", {
  all_levels <- 1:20
  my_levels <- 7:13
  actual <- numeric_right_segment_plus1(all_levels, my_levels)
  expected <- list(lb=6, ub=13)
  expect_equal(actual, expected)
})

test_that("numeric_right_segment_plus1 handles internal gaps", {
  all_levels <- 1:20
  my_levels <- c(3:8,12:16)
  actual <- numeric_right_segment_plus1(all_levels, my_levels)
  expected <- list(lb=11, ub=16)
  expect_equal(actual, expected)
})

test_that("numeric_right_segment_plus1 handles right-justified segment", {
  all_levels <- 1:20
  my_levels <- c(12:20)
  actual <- numeric_right_segment_plus1(all_levels, my_levels)
  expected <- list(lb=11, ub=20)
  expect_equal(actual, expected)
})

test_that("numeric_right_segment_plus1 handles left-justified segment", {
  all_levels <- 1:20
  my_levels <- c(1:10)
  actual <- numeric_right_segment_plus1(all_levels, my_levels)
  expected <- list(lb=-Inf, ub=10)
  expect_equal(actual, expected)
})

test_that("numeric_right_segment_plus1 handles unsorted levels", {
  all_levels <- c(11:20,1:10)
  my_levels <- c(11:15,3:8)
  actual <- numeric_right_segment_plus1(all_levels, my_levels)
  expected <- list(lb=10, ub=15)
  expect_equal(actual, expected)
})

test_that("lines_split_streak handles simple case", {
  levels <- 1:20
  streaks <- tibble::tibble(
    StreakId=c(1,1,2),
    Level=c(1,2,3)
  )
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      1,
    1,      2,
    2,      2
  )
  actual <- lines_split_streak(streaks, concordances, levels, 2)
  expected <- list(
    remainder = streaks %>% filter(Level != 3),
    streak_line = streaks %>% filter(Level != 1)
  )
  expect_equal(actual, expected)
})

test_that("lines_split_streak handles Y branch", {
  levels <- 1:20
  streaks <- tibble::tribble(
    ~StreakId, ~Level,
    1,         1,
    2,         2,
    3,         2,
    2,         3,
    3,         3
  )
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      1,
    1,      2,
    1,      3,
    2,      2,
    3,      3
  )
  expected_2 <- list(
    remainder = streaks %>% filter(StreakId != 2),
    streak_line = streaks %>% filter(StreakId != 3)
  )
  expected_3 <- list(
    remainder = streaks %>% filter(StreakId != 3),
    streak_line = streaks %>% filter(StreakId != 2)
  )
  actual_2 <- lines_split_streak(streaks, concordances, levels, 2)
  actual_3 <- lines_split_streak(streaks, concordances, levels, 3)
  expect_equal(actual_2, expected_2)
  expect_equal(actual_3, expected_3)
})

test_that("lines_split_top_streak strips off both sides of Y branch", {
  levels <- 1:20
  streaks <- tibble::tribble(
    ~StreakId, ~Level,
    1,         1,
    2,         2,
    3,         2,
    2,         3,
    3,         3
  )
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      1,
    1,      2,
    1,      3,
    2,      2,
    3,      3
  )
  streak_lines <- list(remainder=streaks, lines=list())
  actual_1 <- lines_split_top_streak(streak_lines, concordances, levels)
  expected_1 <- list(
    remainder=streaks %>% filter(StreakId != 2),
    lines=list(streaks %>% filter(StreakId != 3))
  )
  expect_equal(actual_1, expected_1)
  actual_2 <- lines_split_top_streak(actual_1, concordances, levels)
  expected_2 <- list(
    remainder=streaks %>% filter(StreakId==1),
    lines=list(
      streaks %>% filter(StreakId != 3),
      streaks %>% filter(StreakId != 2)
    )
  )
  expect_equal(actual_2, expected_2)
})


test_that("lines_split_top_streak strips off final streak", {
  levels <- 1:20
  streaks <- tibble::tribble(
    ~StreakId, ~Level,
    1,         1,
    2,         2,
    3,         2,
    2,         3,
    3,         3
  )
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      1,
    1,      2,
    1,      3,
    2,      2,
    3,      3
  )
  streak_lines <- list(
    remainder=streaks %>% filter(StreakId==1),
    lines=list(
      streaks %>% filter(StreakId != 3),
      streaks %>% filter(StreakId != 2)
    )
  )
  actual <- lines_split_top_streak(streak_lines, concordances, levels)
  expected <- list(
    remainder=tibble(StreakId=integer(), Level=integer()),
    lines=list(
      streaks %>% filter(StreakId != 3),
      streaks %>% filter(StreakId != 2),
      streaks %>% filter(StreakId == 1)
    )
  )
  expect_equal(actual, expected)
})

test_that("lines_split_all handles Y branch", {
  levels <- 1:20
  streaks <- tibble::tribble(
    ~StreakId, ~Level,
    1,         1,
    2,         2,
    3,         2,
    2,         3,
    3,         3
  )
  concordances <- tibble::tribble(
    ~Outer, ~Inner,
    1,      1,
    1,      2,
    1,      3,
    2,      2,
    3,      3
  )

  actual <- lines_split_all(streaks, concordances, levels)
  expected <- list(
    streaks %>% filter(StreakId != 3),
    streaks %>% filter(StreakId != 2),
    streaks %>% filter(StreakId == 1)
  )
  expect_equal(actual, expected)
})

test_that("lines_bind handles simple case", {
  line_list <- list(
    tibble(Foo=1:2),
    tibble(Foo=3:4),
    tibble(Foo=5:6)
  )
  actual <- lines_bind(line_list)
  expected <- tibble(
    Foo=1:6,
    LineIdx=c(1,1,2,2,3,3)
  )
  expect_equal(actual, expected)
})
