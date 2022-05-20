test_that("filter_ui_division_choice_values_as_league_and_division_list handles
          basic scenario", {
  choice_values <- list("AL_None", "NL_West")
  expected <- list(
    list(league="AL", division=NA),
    list(league="NL", division="West")
  )
  actual <- filter_ui_division_choice_values_as_league_and_division_list(
    choice_values)
  expect_equal(actual, expected)
})

test_that("filter_ui_build_divisions_choices basic test", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,  ~FirstSeason, ~FinalSeason,
    "ALE1",       "AL",     "East",    1901,         1920,
    "ALE2",       "AL",     "East",    1910,         1930,
    "ALW1",       "AL",     "West",    1905,         1925,
    "ALW2",       "AL",     "West",    1915,         1935,
    "NLW1",       "NL",     "West",    1900,         1930,
    "NLW2",       "NL",     "West",    1910,         1950
  )
  actual <- filter_ui_build_divisions_choices(franchises, 1900:1950, c("AL","NL"))
  expected <- list("AL Divisions"=
                     list("AL East (1901-1930)"="AL_East",
                          "AL West (1905-1935)"="AL_West"),
                   "NL Divisions"=
                     list("NL West"="NL_West")
  )
  expect_equal(actual, expected)
})

