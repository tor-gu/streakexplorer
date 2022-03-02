# TODO we are not using this file now. Should move plot stuff from lines here
plot_make_data <- function(streaks, concordances, Id=NULL) {
  if ( !is.null(Id) && !(Id %in% streaks$Id) ) Id <- NULL
  streaks_with_relations <- streaks %>%
    sr_initialize() %>%
    sr_update_all(concordances, Id)

  streak_relations <- select(streaks_with_relations, Id, Relation)
  segments <- seg_make_segments(streaks, concordances, 1:99/100) %>%
    seg_add_relations(streaks_with_relations)

  list(points=streaks_with_relations, segments=segments)
}

plot_make_plot <- function(plot_data) {
  points <- plot_data$points
  segments <- plot_data$segments
  points_mapping <- ggplot2::aes(x=Level, y=AdjScore)
  segments_mapping <- ggplot2::aes(x=Level.x, y=AdjScore.x, xend=Level.y,
                                   yend=AdjScore.y)

  ggplot2::ggplot() +
    ggplot2::geom_point(data = points %>% filter(Relation=="IDENTICAL"),
               mapping = points_mapping,
               size = 2.5,
               color="red") +
    ggplot2::geom_point(data = points %>% filter(Relation=="SAME_STREAK"),
               mapping = points_mapping,
               size = 1.5,
               color="red") +
    ggplot2::geom_point(data = points %>% filter(Relation=="RELATED_STREAK"),
               mapping = points_mapping,
               size = 1,
               color="red") +
    ggplot2::geom_point(data = points %>% filter(Relation=="SAME_TEAM_SEASON"),
               mapping = points_mapping,
               size = .5,
               color="red") +
    ggplot2::geom_point(data = points %>% filter(Relation=="SAME_TEAM"),
               mapping = points_mapping,
               size = .5,
               color="blue") +
    ggplot2::geom_point(data = points %>% filter(Relation!="NO_RELATION"),
               mapping = points_mapping,
               size = .1) +
    ggplot2::geom_segment(data=segments %>% filter(Relation=="SAME_STREAK"),
                 mapping=segments_mapping,
                 color="red",
                 alpha=1,
                 size=1.5) +
    ggplot2::geom_segment(data=segments %>% filter(Relation=="RELATED_STREAK"),
                 mapping=segments_mapping,
                 color="red",
                 alpha=1,
                 size=.5) +
    ggplot2::geom_segment(data=segments %>%
                            filter(Relation=="SAME_TEAM_SEASON"),
                 mapping=segments_mapping,
                 color="red",
                 alpha=.5,
                 size=.5) +
    ggplot2::geom_segment(data=segments %>% filter(Relation=="SAME_TEAM"),
                 mapping=segments_mapping,
                 color="blue",
                 alpha=.5,
                 size=.5) +
    ggplot2::geom_segment(data=segments %>% filter(Relation=="NO_RELATION"),
                 mapping=segments_mapping,
                 alpha=.1)
}

