test_that("ui_filter_by_league_divisions handles division text and NA", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,
    "ALE",       "AL",     "East",
    "ALW",       "AL",     "West",
    "ALN",       "AL",     NA,
    "NLE",       "NL",     "East",
    "NLW",       "NL",     "West",
    "NLN",       "NL",     NA,
  )
  league_divisions <- list(
    list(league="AL", division="East"),
    list(league="NL", division=NA)
  )
  actual <- ui_filter_by_league_divisions(franchises, league_divisions)
  expected <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,
    "ALE",       "AL",     "East",
    "NLN",       "NL",     NA,
  )
  expect_equal(actual, expected, ignore_attr=TRUE)
})

test_that("ui_filter_by_league_divisions handles not-found values", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,
    "ALE",       "AL",     "East",
    "ALW",       "AL",     "West",
    "ALN",       "AL",     NA,
    "NLE",       "NL",     "East",
    "NLW",       "NL",     "West",
    "NLN",       "NL",     NA,
  )
  league_divisions <- list(
    list(league="AL", division="Central"),
    list(league="ZZ", division="East")
  )
  actual <- ui_filter_by_league_divisions(franchises, league_divisions) %>%
    nrow()
  expect_equal(actual, 0)
})

test_that("ui_division_choice_values_as_league_and_division_list handles basic scenario", {
  choice_values <- list("AL_None", "NL_West")
  expected <- list(
    list(league="AL", division=NA),
    list(league="NL", division="West")
  )
  actual <- ui_division_choice_values_as_league_and_division_list(choice_values)
  expect_equal(actual, expected)
})

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
