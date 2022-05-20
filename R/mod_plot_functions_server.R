plot_server_get_max_rank <- function(franchises, years, teams, hot) {
  # Get the max rank using n=10
  start_time <- Sys.time()
  on.exit(message(paste(rlang::call_name(sys.call()), Sys.time() - start_time, sep="||")))
  min_year <- years[[1]]
  max_year <- years[[2]]
  team_ids <- franchises_franchise_ids_to_team_ids(franchises, teams, min_year,
                                                   max_year)
  streaks_get_max_rank(10, min_year, max_year, team_ids, hot)
}
