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

test_that("ui_truncate_years handles basic scenarios", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~FirstSeason, ~FinalSeason,
    "AA1",        1,            10,
    "AA2",        11,           20,
    "AA3",        21,           NA
  )

  actual <- ui_truncate_years(franchises, 5:25)
  expected <- tibble::tribble(
    ~FranchiseID, ~FirstSeason, ~FinalSeason,
    "AA1",        5,            10,
    "AA2",        11,           20,
    "AA3",        21,           25
  )
  expect_equal(actual, expected)
})

test_that("ui_get_divisions handles divisions", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,  ~FirstSeason, ~FinalSeason,
    "ALE1",       "AL",     "East",    1,            20,
    "ALE2",       "AL",     "East",    10,           30,
    "ALW1",       "AL",     "West",    5,            25,
    "ALW2",       "AL",     "West",    15,           35,
  )
  actual <- ui_get_divisions(franchises)
  expected <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1,            30,
    "AL",    "West",    5,            35
  )
  expect_equal(actual, expected)
})

test_that("ui_get_divisions handles non-divisions", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,  ~FirstSeason, ~FinalSeason,
    "AL1",        "AL",     NA,        1,            20,
    "AL2",        "AL",     NA,        10,           30,
    "NL1",        "NL",     NA,        5,            25,
    "NL2",        "NL",     NA,        15,           35,
  )
  actual <- ui_get_divisions(franchises)
  expected <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    NA,        1,            30,
    "NL",    NA,        5,            35
  )
  expect_equal(actual, expected)
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
  actual <- ui_division_as_choice_label("AL", "East")
  expected <- "AL East"
  expect_equal(actual, expected)
})

test_that("ui_division_as_choice_label handles case with years specified", {
  actual <- ui_division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1970, 2000)  # min and max )
  expected <- "AL East (1980-1990)"
  expect_equal(actual, expected)

})

test_that("ui_division_as_choice_label handles case with years at max range", {
  actual <- ui_division_as_choice_label("AL", "East",
                                     1980, 1990, # First and final season
                                     1980, 1990)  # min and max )
  expected <- "AL East"
  expect_equal(actual, expected)

})

test_that("ui_division_as_choice_label handles case with almost full range", {
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

test_that("ui_division_as_choice_label handles NA division", {
  actual <- ui_division_as_choice_label("AL", NA)
  expected <- "AL No Division"
  expect_equal(actual, expected)
})


test_that("ui_generate_league_division_selection handles identical year ranges", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         2021,
    "AL",    "West",    1969,         2021
  )
  actual <- ui_generate_league_division_selection(division_table, "AL")
  expected <- list("AL East"="AL_East", "AL West"="AL_West")
  expect_equal(actual, expected)
})

test_that("ui_generate_league_division_selection handles varying year ranges", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         1985,
    "AL",    "West",    1980,         2021,
    "AL",    "Central", 1969,         2021
  )
  actual <- ui_generate_league_division_selection(division_table, "AL")
  expected <- list("AL East (1969-1985)"="AL_East",
                   "AL West (1980-2021)"="AL_West",
                   "AL Central"="AL_Central")
  expect_equal(actual, expected)
})

test_that("ui_generate_league_division_selection handles NA division", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         1985,
    "AL",    NA,        1969,         2021,
  )
  actual <- ui_generate_league_division_selection(division_table, "AL")
  expected <- list("AL East (1969-1985)"="AL_East",
                   "AL No Division"="AL_None")
  expect_equal(actual, expected)
})


test_that("ui_generate_league_division_selection filters out irrelevant leagues", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         1985,
    "AL",    "West",    1980,         2021,
    "NL",    "East",    1960,         1990
  )
  actual <- ui_generate_league_division_selection(division_table, "AL")
  expected <- list("AL East (1969-1985)"="AL_East",
                   "AL West (1980-2021)"="AL_West")
  expect_equal(actual, expected)
})

test_that("ui_generate_division_selection single league", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         1985,
    "AL",    NA,        1969,         2021,
  )
  actual <- ui_generate_division_selection(division_table)
  expected <- list("AL East (1969-1985)"="AL_East",
                   "AL No Division"="AL_None")
  expect_equal(actual, expected)
})

test_that("ui_generate_division_selection both leagues", {
  division_table <- tibble::tribble(
    ~League, ~Division, ~FirstSeason, ~FinalSeason,
    "AL",    "East",    1969,         1985,
    "AL",    "West",    1980,         2021,
    "NL",    "East",    1960,         1990
  )
  actual <- ui_generate_division_selection(division_table)
  expected <- list("AL Divisions"=
                     list("AL East (1969-1985)"="AL_East",
                          "AL West (1980-2021)"="AL_West"),
                   "NL Divisions"=
                     list("NL East"="NL_East")
  )
  expect_equal(actual, expected)
})

test_that("ui_generate_team_selection basic test", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~Nickname,  ~FirstSeason,
    "AAA",        "AAA 1",    1,
    "AAA",        "AAA 2",    2,
    "BBB",        "BBB only", 2
  )
  actual <- ui_generate_team_selection(franchises)
  expected <- list(
    "AAA 2/AAA 1"="AAA", "BBB only"="BBB"
  )
  expect_equal(actual, expected)
})


test_that("ui_build_divisions_choices basic test", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,  ~FirstSeason, ~FinalSeason,
    "ALE1",       "AL",     "East",    1901,         1920,
    "ALE2",       "AL",     "East",    1910,         1930,
    "ALW1",       "AL",     "West",    1905,         1925,
    "ALW2",       "AL",     "West",    1915,         1935,
    "NLW1",       "NL",     "West",    1900,         1930,
    "NLW2",       "NL",     "West",    1910,         1950
  )
  actual <- ui_build_divisions_choices(franchises, 1900:1950, c("AL","NL"))
  expected <- list("AL Divisions"=
                     list("AL East (1901-1930)"="AL_East",
                          "AL West (1905-1935)"="AL_West"),
                   "NL Divisions"=
                     list("NL West"="NL_West")
  )
  expect_equal(actual, expected)
})

test_that("ui_build_teams_choices basic test", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~League, ~Division,  ~Nickname,  ~FirstSeason, ~FinalSeason,
    "TBA",        "AL",    "East",    "Devil Rays", 1998,        2007,
    "TBA",        "AL",    "East",    "Rays",       2008,        NA,
    "TEX",        "AL",     NA,       "Senators",   1961,        1968,
    "TEX",        "AL",    "East",    "Senators",   1969,        1971,
    "TEX",        "AL",    "West",    "Rangers",    1972,        NA,
    "TOR",        "AL",    "East",    "Blue Jays",  1977,        NA
  )
  actual <- ui_build_teams_choices(franchises, 1972:2020,
                                   list(list(league="AL", division="East")))
  expected <- list("Rays/Devil Rays"="TBA", "Blue Jays"="TOR")
  expect_equal(actual, expected)
})

test_that("ui_get_updated_division_selection preserves selection when possible", {
  division_choices <- list(
    "AL Divisions" =
      list(
        "AL East" = "AL_East",
        "AL West" = "AL_West"
      ),
    "NL Divisions" =
      list("NL West" = "NL_West")
  )

  input_divisions <- "AL_East"
  actual <- ui_get_updated_division_selection(division_choices,
                                              input_divisions, FALSE)
  expected <- "AL_East"
  expect_equal(actual, expected)
})

test_that("ui_get_updated_division_selection reverts to all when necessary", {
  division_choices <- list(
    "AL Divisions" =
      list(
        "AL East" = "AL_East",
        "AL West" = "AL_West"
      ),
    "NL Divisions" =
      list("NL West" = "NL_West")
  )

  input_divisions <- c("AL_East","NL_East")
  actual <- ui_get_updated_division_selection(division_choices,
                                              input_divisions, FALSE)
  # We don't care about the names
  names(actual) <- NULL
  expected <- c("AL_East","AL_West","NL_West")
  expect_equal(actual, expected)
})

test_that("ui_get_updated_division_selection reverts to all when 'All' is selected", {
  division_choices <- list(
    "AL Divisions" =
      list(
        "AL East" = "AL_East",
        "AL West" = "AL_West"
      ),
    "NL Divisions" =
      list("NL West" = "NL_West")
  )

  input_divisions <- "AL_East"
  actual <- ui_get_updated_division_selection(division_choices,
                                              input_divisions, TRUE)
  # We don't care about the names
  names(actual) <- NULL
  expected <- c("AL_East","AL_West","NL_West")
  expect_equal(actual, expected)
})

test_that("ui_get_updated_teams_selection keeps slection when possible", {
  teams_choices <- list("Rays/Devil Rays"="TBA", "Blue Jays"="TOR")
  input_teams <- "TOR"
  actual <- ui_get_updated_teams_selection(teams_choices, input_teams, FALSE)

  # We don't care about the names
  names(actual) <- NULL
  expected <- "TOR"
  expect_equal(actual, expected)
})

test_that("ui_get_updated_teams_selection reverts to all when necssary", {
  teams_choices <- list("Rays/Devil Rays"="TBA", "Blue Jays"="TOR")
  input_teams <- c("TOR","XXX")
  actual <- ui_get_updated_teams_selection(teams_choices, input_teams, FALSE)

  # We don't care about the names
  names(actual) <- NULL
  expected <- c("TBA","TOR")
  expect_equal(actual, expected)
})


test_that("ui_get_updated_teams_selection reverts to all when 'All' is checked", {
  teams_choices <- list("Rays/Devil Rays"="TBA", "Blue Jays"="TOR")
  input_teams <- "TOR"
  actual <- ui_get_updated_teams_selection(teams_choices, input_teams, TRUE)

    # We don't care about the names
  names(actual) <- NULL
  expected <- c("TBA","TOR")
  expect_equal(actual, expected)
})
