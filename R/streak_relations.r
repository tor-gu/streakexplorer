sr_level_names <- c("IDENTICAL", "SAME_STREAK", "RELATED_STREAK",
                    "SAME_TEAM_SEASON", "SAME_TEAM", "NO_RELATION")

sr_initialize <- function(streaks) {
  streaks %>% dplyr::mutate(Relation=factor("NO_RELATION",
                                            levels=sr_level_names, ordered=TRUE))
}

sr_get_related_streak_ids <- function(concordances, StreakId) {
  Inner <- concordances %>% dplyr::filter(Inner==StreakId) %>% dplyr::pull(Outer)
  Outer <- concordances %>% dplyr::filter(Outer==StreakId) %>% dplyr::pull(Inner)
  c(Inner,Outer) %>% unique()
}

sr_update_same_team <- function(streaks, Id) {
  Team <- streaks %>% dplyr::filter(Id==.env$Id) %>% dplyr::pull(Team)
  streaks %>% dplyr::mutate(
    Relation = factor(ifelse(Team==.env$Team,
                             "SAME_TEAM",
                             as.character(Relation)),
                      levels=sr_level_names, ordered = TRUE)
  )
}

sr_update_same_team_season <- function(streaks, Id) {
  this_streak <- streaks %>% dplyr::filter(Id==.env$Id)
  Team <- this_streak %>% dplyr::pull(Team)
  Year <- this_streak %>% dplyr::pull(Year)
  streaks %>% dplyr::mutate(
    Relation = factor(ifelse(Team==.env$Team & Year==.env$Year,
                             "SAME_TEAM_SEASON",
                             as.character(Relation)),
                      levels=sr_level_names, ordered = TRUE)
  )
}

sr_update_related_streak <- function(streaks, concordances, Id) {
  StreakId <- streaks %>% dplyr::filter(Id==.env$Id) %>% dplyr::pull(StreakId)
  related_streak_ids <- sr_get_related_streak_ids(concordances, StreakId)
  streaks %>% dplyr::mutate(
    Relation = factor(ifelse(StreakId %in% related_streak_ids,
                             "RELATED_STREAK",
                             as.character(Relation)),
                      levels=sr_level_names, ordered = TRUE)
  )
}

sr_update_same_streak <- function(streaks, Id) {
  StreakId <- streaks %>% dplyr::filter(Id==.env$Id) %>% dplyr::pull(StreakId)
  streaks %>% dplyr::mutate(
    Relation = factor(ifelse(StreakId==.env$StreakId,
                             "SAME_STREAK",
                             as.character(Relation)),
                      levels=sr_level_names, ordered = TRUE)
  )
}

sr_update_identical <- function(streaks, Id) {
  streaks %>% dplyr::mutate(
    Relation = factor(ifelse(Id==.env$Id,
                             "IDENTICAL",
                             as.character(Relation)),
                      levels=sr_level_names, ordered = TRUE)
  )
}

sr_update_all <- function(streaks, concordances, Id=NULL) {
  if ( !is.null(Id) ) {
    streaks %>%
      sr_update_same_team(Id) %>%
      sr_update_same_team_season(Id) %>%
      sr_update_related_streak(concordances, Id) %>%
      sr_update_same_streak(Id) %>%
      sr_update_identical(Id)
  } else {
    streaks
  }
}

