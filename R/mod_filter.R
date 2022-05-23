filterUI <- function(id, initial_year_range) {
  ns <- shiny::NS(id)
  shiny::tagList(
    #### Years slider ----
    shiny::fluidRow(shiny::column(
      12,
      shiny::sliderInput(
        ns("years"),
        "Years",
        min = 1948,
        max = 2021,
        step = 1,
        value = initial_year_range,
        sep = ""
      )
    )),

    #### League selection ----
    shiny::fluidRow(shiny::column(
      9,
      shiny::selectInput(
        ns("leagues"),
        "Leagues",
        choices = c(
          "All Leagues" = "BOTH",
          "AL" = "AL",
          "NL" = "NL"
        )
      )
    ),
    # This empty column is a placeholder, where the 'all' box would go
    shiny::column(3, )),

    #### Division selection ----
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::selectInput(
          ns("divisions"),
          "Divisions",
          choices = list(),
          multiple = TRUE
        ),
      ),
      shiny::column(3,
                    shiny::checkboxInput(ns("divisions_all"), "All",
                                         value = TRUE))
    ),

    #### Team selection ----
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::selectInput(
          ns("teams"),
          "Teams",
          choices = list(),
          multiple = TRUE
        ),
      ),
      shiny::column(3,
                    shiny::checkboxInput(ns("teams_all"), "All", value = TRUE))
    ),

    #### Hot/cold selection ----
    shiny::fluidRow(shiny::column(
      12,
      shiny::radioButtons(
        ns("streak_type"),
        "Streak Type",
        choices = c("HOT", "COLD"),
        selected = "HOT"
      )
    ))
  )
}

filterServer <- function(id, franchises) {
  moduleServer(id, function(input, output, session) {
    ## Reactives and reactive values ----
    hot <- reactive({
      input$streak_type == "HOT"
    })

    years <- reactive({
      input$years
    }) %>% debounce(333)

    selected_years <- reactive({
      years()[[1]]:years()[[2]]
    })
    selected_leagues <- reactiveVal(c("AL", "NL"))
    selected_league_divisions <- reactiveVal(
      list("AL_None", "NL_None") %>%
        filter_ui_division_choice_values_as_league_and_division_list()
    )

    divisions_choices <- reactive({
      filter_ui_build_divisions_choices(franchises,
                                 selected_years(),
                                 selected_leagues())
    })

    teams_choices <- reactive({
      filter_ui_build_teams_choices(franchises,
                                    selected_years(),
                                    selected_league_divisions())
    })

    no_divisions_choices <- reactive({
      all(unname(unlist(divisions_choices())) %in% c("AL_None", "NL_None"))
    })

    ## UI functions ----
    update_divisions_selection <- function() {
      if (no_divisions_choices()) {
        updateCheckboxInput(session, "divisions_all", value = TRUE)
        shinyjs::disable("divisions")
        shinyjs::disable("divisions_all")
      } else {
        if (input$divisions_all) {
          shinyjs::enable("divisions_all")
          shinyjs::disable("divisions")
        } else {
          shinyjs::enable("divisions")
          shinyjs::enable("divisions_all")
        }
      }
      selected <-
        filter_ui_get_updated_division_selection(divisions_choices(),
                                                 input$divisions,
                                                 input$divisions_all)
      updateSelectInput(
        session,
        "divisions",
        choices = divisions_choices(),
        selected = selected
      )
    }

    update_teams_selection <- function() {
      if (input$teams_all) {
        shinyjs::disable("teams")
      } else {
        shinyjs::enable("teams")
      }
      selected <-
        filter_ui_get_updated_teams_selection(teams_choices(),
                                              input$teams,
                                              input$teams_all)
      updateSelectInput(session,
                        "teams",
                        choices = teams_choices(),
                        selected = selected)
    }

    ## Observers ----
    observeEvent(input$teams_all, {
      update_teams_selection()
    })

    observeEvent(input$divisions_all, {
      update_divisions_selection()
    })

    observeEvent(input$leagues, {
      selected_leagues(if (input$leagues == "BOTH")
        c("AL", "NL")
        else
          input$leagues)
      update_divisions_selection()
    })

    observeEvent(years(), {
      update_divisions_selection()
      update_teams_selection()
    })

    observeEvent(input$divisions, {
      selected_league_divisions(
        input$divisions %>%
          filter_ui_division_choice_values_as_league_and_division_list())
      update_teams_selection()
    })

    list(
      years = years,
      teams = reactive(input$teams),
      hot = hot
    )
  })
}
