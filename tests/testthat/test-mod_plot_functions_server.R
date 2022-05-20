test_that("ps_franchise_ids_to_team_ids basic test", {
  franchises <- tibble::tribble(
    ~FranchiseID, ~TeamID, ~FirstSeason, ~FinalSeason,
    "AAA",        "AA1",   1,            10,
    "AAA",        "AA2",   11,           20,
    "AAA",        "AA3",   21,           30,
    "AAA",        "AA4",   31,           NA,
    "BBB",        "BB1",   1,            25,
    "BBB",        "BB2",   26,           NA,
    "CCC",        "CC1",   1,            22,
    "CCC",        "CC2",   23,           33,
    "CCC",        "CC3",   34,           NA,
  )
  franchise_ids <- c("AAA","CCC")
  min_year <- 22
  max_year <- 33
  actual <- ps_franchise_ids_to_team_ids(franchises, franchise_ids,
                                         min_year, max_year)
  expected <-  c("AA3","AA4","CC1","CC2")
  expect_equal(actual, expected)
})

