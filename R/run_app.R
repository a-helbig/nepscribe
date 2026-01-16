#' Launch NEPScribe
#' @export
run_app <- function() {
  options(shiny.launch.browser = TRUE) # for Preview in Browser
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
