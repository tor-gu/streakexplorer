# TODO description
test_that("division_as_choice_label handles case without years specified", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  actual <- division_as_choice_label("AL", "East")
  expected <- "AL East"
  expect_equal(actual, expected)
})

test_that("division_as_choice_label handles case with years specified", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  actual <- division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1970, 2000)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)

})

test_that("division_as_choice_label handles case with years at max range", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  actual <- division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1980, 1990)  # min and max )
  expected <- "AL East"
  expect_equal(actual, expected)

})

test_that("division_as_choice_label handles case with almost full range", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  # Match the max but not the min
  actual <- division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1979, 1990)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)

  # Match the min but not the max
  actual <- division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1980, 1991)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)
})

