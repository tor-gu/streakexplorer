standings_DT <- function(streak_info, standings) {
  standings <- standings %>% dplyr::select(Team=Nickname, W=Wins, L=Losses, GB)
  DT::datatable(
    standings,
    rownames = FALSE,
    options = list(
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    )
  ) %>%
    DT::formatStyle("Team",
                    target="row",
                    backgroundColor=DT::styleEqual(streak_info$Nickname,"red")
    )
}
