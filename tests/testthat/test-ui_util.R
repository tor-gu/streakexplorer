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
