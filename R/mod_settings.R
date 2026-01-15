#' @title Settings UI Module
#' @description Provides global settings for the app, including sidebar width and label language.
#' @keywords internal
#' @noRd
settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("sidebarWidth"), htmltools::tags$b("Sidebar Width"), min = 150, max = 600, value = 320),
  )
}

#' @title Settings Server Module
#' @description Provides a reactive list of global settings selected in the UI.
#' @keywords internal
#' @noRd
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
