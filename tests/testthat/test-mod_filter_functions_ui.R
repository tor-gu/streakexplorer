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
