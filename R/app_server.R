#' @title Application Server
#' @description Define the server logic for the SUF-Explorer Shiny application.
#' Manages reactive modules, dataset exploration, data transformation, and sidebar updates.
#' @keywords internal
#' @noRd
app_server <- function(input, output, session) {

  # Stop the app cleanly when session ends
  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  observeEvent(input$show_changelog, {

    # You can read the changelog markdown file here
    changelog_file <- system.file("CHANGELOG.md", package = "NEPScribe")
    # or locally: changelog_file <- "CHANGELOG.md"

    showModal(modalDialog(
      title = "Changelog",
      size = "l",  # large modal
      easyClose = TRUE,
      footer = modalButton("Close"),
      shiny::includeMarkdown(changelog_file)
    ))
  })

  # --- Settings reactive for sidebar width, language, etc. ---
  settings_reactive <- settings_server("settings")

  # Send sidebar width setting to custom JS
  shiny::observeEvent(settings_reactive(), {
    session$sendCustomMessage("sidebarWidth", settings_reactive()$sidebarWidth)
  })

  # --- Dataset explorer module ---
  # Uses cohort_path() reactive; can return single or multiple cohort folders
  dataset_explorer_server(
    id = "explore_dataset",
    settings_reactive = settings_reactive
  )

  # --- Data transformation module ---
  # Always uses a single cohort
  data_transformation_server(
    "data_transformation",
    settings_reactive = settings_reactive
  )
}
