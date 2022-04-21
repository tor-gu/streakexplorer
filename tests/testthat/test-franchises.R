test_that("franchises_by_season_lzy handles basic scenarios", {
  franchises <- tibble::tribble(
    ~Id, ~FirstSeason, ~FinalSeason,
    1,   0,            1,
    2,   0,            37,
    3,   0,            100,
    4,   0,            NA,
    5,  37,            37,
    6,  37,            100,
    7,  37,            NA,
    8,  100,           100,
    0,  100,           NA
  )

  actual <- franchises_by_season_lzy(franchises, 37)
  expected <- franchises[c(2, 3, 4, 5, 6, 7), ]
  expect_equal(actual, expected)
})

test_that("franchises_get_division_by_team_year_lzy handles division", {
  franchises <- tibble::tribble(
    ~TeamID, ~League, ~Division, ~Location, ~Nickname, ~FirstSeason, ~FinalSeason,
    "AA1",   "AL",    "EAST",    "AA1 Loc", "AA1 Team", 1,           100,
    "AA2",   "AL",    "EAST",    "AA2 Loc", "AA2 Team", 1,           NA,
    "BB1",   "AL",    "WEST",    "BB1 Loc", "BB1 Team", 1,           100,
    "BB2",   "AL",    NA,        "BB2 Loc", "BB2 Team", 1,           100,
    "BB3",   "NL",    "EAST",    "BB3 Loc", "BB3 Team", 1,           100,
    "BB4",   "NL",    "EAST",    "BB4 Loc", "BB4 Team", 1,           100,
    "BB5",   "AL",    "EAST",    "BB5 Loc", "BB5 Team", 1,           36,
    "BB6",   "AL",    "EAST",    "BB6 Loc", "BB6 Team", 38,          100,
  )
  actual <- franchises_get_division_by_team_year_lzy(franchises, "AA1", 37)
  expected <- list(
    lzy_division=tibble::tibble(League="AL", Division="EAST", Year=37 ),
    lzy_teams=tibble::tribble(
      ~TeamID, ~Location, ~Nickname,
      "AA1",   "AA1 Loc", "AA1 Team",
      "AA2",   "AA2 Loc", "AA2 Team",
    )
  )
  expect_equal(actual, expected)
})


test_that("franchises_get_division_by_team_year_lzy handles NA division", {
  franchises <- tibble::tribble(
    ~TeamID, ~League, ~Division, ~Location, ~Nickname, ~FirstSeason, ~FinalSeason,
    "AA1",   "AL",    NA,        "AA1 Loc", "AA1 Team", 1,           100,
    "AA2",   "AL",    NA,        "AA2 Loc", "AA2 Team", 1,           NA,
    "BB1",   "AL",    "WEST",    "BB1 Loc", "BB1 Team", 1,           100,
    "BB2",   "AL",    "EAST",    "BB2 Loc", "BB2 Team", 1,           100,
    "BB3",   "NL",    "EAST",    "BB3 Loc", "BB3 Team", 1,           100,
    "BB4",   "NL",    "EAST",    "BB4 Loc", "BB4 Team", 1,           100,
    "BB5",   "AL",    "EAST",    "BB5 Loc", "BB5 Team", 1,           36,
    "BB6",   "AL",    "EAST",    "BB6 Loc", "BB6 Team", 38,          100,
  )
  actual <- franchises_get_division_by_team_year_lzy(franchises, "AA1", 37)
  expected <- list(
    lzy_division=tibble::tibble(League="AL", Division=NA_character_, Year=37 ),
    lzy_teams=tibble::tribble(
      ~TeamID, ~Location, ~Nickname,
      "AA1",   "AA1 Loc", "AA1 Team",
      "AA2",   "AA2 Loc", "AA2 Team",
    )
  )
  expect_equal(actual, expected)
})

