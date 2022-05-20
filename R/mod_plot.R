plotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(ns("streaks"))
  )
}

plotServer <- function(id, franchises, intensity_level_range, filter) {
  moduleServer(id, function(input, output, session) {

    selected_line_id <- reactiveVal(NULL)
    max_rank <- reactive({
      req(filter$teams())
      plot_server_get_max_rank(franchises, filter$years(), filter$teams(),
                               filter$hot())
    })

    lines <- reactive({
      req(filter$teams(), max_rank())
      server_build_lines(franchises, intensity_level_range, filter$years(),
                         filter$teams(), max_rank(), filter$hot())
    })

    selected_streak <- reactive({
      server_get_selected_streak(selected_line_id(), filter$hot())
    })

    highlighted_lines <- reactive({
      server_lines_highlight(lines(), selected_line_id(), filter$hot())
    })

    # This is the main graph
    main_plot <- reactive(
      server_main_plot(
        highlighted_lines(),
        intensity_level_range,
        max_rank(),
        filter$hot()
      )
    )

    # Set up an observer for the plot click, but wait until the plot
    # is created before creating the observer.
    observeEvent(
      main_plot(),
      observeEvent(
        plotly::event_data("plotly_click", source = "lines_plot"), {
          click_data <- plotly::event_data("plotly_click", source = "lines_plot")
          if (is.null(click_data)) {
            selected_line_id(NULL)
          } else {
            selected_line_id(click_data %>% dplyr::pull("key"))
          }
        }),
      once = TRUE
    )

    observeEvent(filter$hot(), ignoreInit = TRUE, {
      selected_line_id(NULL)
    })

    # Main plot
    output$streaks <- plotly::renderPlotly(main_plot())

    # Return value
    selected_streak
  })
}
