#' @title Cohort Selector UI Module
#' @description Provides radio buttons for selecting a starting cohort (SC3–SC6) in the SUF-Explorer app.
cohort_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(
      inputId = ns("cohort"),
      label = "Select Starting Cohort",
      choices = c(
        "Starting Cohort 3" = "sc3_semantic_files",
        "Starting Cohort 4" = "sc4_semantic_files",
        "Starting Cohort 5" = "sc5_semantic_files",
        "Starting Cohort 6" = "sc6_semantic_files"
      ),
      selected = "sc6_semantic_files",
      inline = TRUE
    )
  )
}

#' @title Cohort Selector Server Module
#' @description Provides a reactive path to the selected cohort folder.
cohort_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::reactive({
        shiny::req(input$cohort)
        path <- system.file("extdata", input$cohort, package = "NEPScribe")
      })
    }
  )
}

#' @title Settings UI Module
#' @description Provides global settings for the app, including sidebar width and label language.
settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(htmltools::HTML("<b>Global Settings</b>")),
    shiny::sliderInput(ns("sidebarWidth"), htmltools::tags$b("Sidebar Width"), min = 150, max = 600, value = 255),
    shiny::p(htmltools::HTML("<b>Variable Labels</b>")),
    shinyWidgets::switchInput(
      ns("language"),
      label = htmltools::tags$b("Labels"),
      value = TRUE,
      onLabel = "English",
      offLabel = "German",
      onStatus = "info",
      offStatus = "warning",
      inline = FALSE
    )
  )
}

#' @title Settings Server Module
#' @description Provides a reactive list of global settings selected in the UI.
settings_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::reactive({
        list(
          sidebarWidth = input$sidebarWidth,
          language = input$language
        )
      })
    }
  )
}
