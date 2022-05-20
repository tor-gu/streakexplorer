




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
