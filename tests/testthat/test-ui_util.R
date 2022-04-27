test_that("ui_filter_by_year handles basic scenarios", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~FirstSeason, ~FinalSeason,
    "AA1",        1,            10,
    "AA2",        11,           20,
    "AA3",        21,           NA,
    "BB1",        15,           25,
    "BB2",        26,           35,
    "BB3",        36,           NA
  )

  year_10 <- ui_filter_by_year(franchises, 10) %>% dplyr::pull(FranchiseID)
  year_11 <- ui_filter_by_year(franchises, 11) %>% dplyr::pull(FranchiseID)
  year_21 <- ui_filter_by_year(franchises, 21) %>% dplyr::pull(FranchiseID)
  expect_equal(year_10, "AA1")
  expect_equal(year_11, "AA2")
  expect_equal(year_21, c("AA3", "BB1"))
})

test_that("ui_filter_by_years handles basic scenarios", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~FirstSeason, ~FinalSeason,
    "AA1",        1,            10,
    "AA2",        11,           20,
    "AA3",        21,           NA,
    "BB1",        15,           25,
    "BB2",        26,           35,
    "BB3",        36,           NA
  )

  years_0_40 <- ui_filter_by_years(franchises, 0:40) %>% dplyr::pull(FranchiseID)
  years_5_15 <- ui_filter_by_years(franchises, 5:15) %>% dplyr::pull(FranchiseID)
  years_25_40 <- ui_filter_by_years(franchises, 25:40) %>%
    dplyr::pull(FranchiseID)

  expect_setequal(years_0_40, c("AA1","AA2","AA3","BB1","BB2","BB3"))
  expect_setequal(years_5_15, c("AA1","AA2","BB1"))
  expect_setequal(years_25_40, c("AA3","BB1","BB2","BB3"))
})


test_that("ui_filter_by_league handles basic scenarios", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League,
    "AA1",        "AL",
    "AA2",        "AL",
    "AA3",        "AL",
    "BB1",        "NL",
    "BB2",        "NL",
    "BB3",        "NL",
  )

  empty <- ui_filter_by_league(franchises, character(0)) %>%
    dplyr::pull(FranchiseID)
  al <- ui_filter_by_league(franchises, "AL")  %>%
    dplyr::pull(FranchiseID)
  nl <- ui_filter_by_league(franchises, "NL") %>%
    dplyr::pull(FranchiseID)
  both <- ui_filter_by_league(franchises, c("AL", "NL")) %>%
    dplyr::pull(FranchiseID)


  expect_equal(length(empty), 0)
  expect_setequal(al, c("AA1","AA2","AA3"))
  expect_setequal(nl, c("BB1","BB2","BB3"))
  expect_setequal(both, c("AA1", "AA2", "AA3","BB1","BB2","BB3"))
})

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

test_that("ui_division_as_choice_label handles case without years specified", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  actual <- ui_division_as_choice_label("AL", "East")
  expected <- "AL East"
  expect_equal(actual, expected)
})

test_that("ui_division_as_choice_label handles case with years specified", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  actual <- ui_division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1970, 2000)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)

})

test_that("ui_division_as_choice_label handles case with years at max range", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  actual <- ui_division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1980, 1990)  # min and max )
  expected <- "AL East"
  expect_equal(actual, expected)

})

test_that("ui_division_as_choice_label handles case with almost full range", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
  )

  # Match the max but not the min
  actual <- ui_division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1979, 1990)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)

  # Match the min but not the max
  actual <- ui_division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1980, 1991)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)
})
