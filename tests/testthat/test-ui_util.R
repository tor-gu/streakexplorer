



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
