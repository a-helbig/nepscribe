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

  shiny::observeEvent(input$show_changelog, {

    # Determine path to changelog
    changelog_file <- system.file("extdata", "changelog.md", package = "YourPackage")

    # Fallback if running in dev (file not yet installed)
    if (!file.exists(changelog_file)) {
      changelog_file <- file.path("inst", "extdata", "changelog.md")
    }

    # Show modal
    shiny::showModal(
      shiny::modalDialog(
        title = "Changelog",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shiny::includeMarkdown(changelog_file)
      )
    )

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
