#' @title Application Server
#' @description Define the server logic for the SUF-Explorer Shiny application.
#' Manages reactive modules, dataset exploration, data transformation, and sidebar updates.
#' @keywords internal
#' @noRd
app_server <- function(input, output, session) {

  # When run locally from an interactive R session (e.g. RStudio), stop the app when the
  # browser tab is closed. Never on a server: there several users can share one R process,
  # and stopping it would disconnect everyone else on it.
  if (base::interactive()) {
    session$onSessionEnded(function() {
      shiny::stopApp()
    })
  }

  shiny::observeEvent(input$show_changelog, {

    # Determine path to changelog
    changelog_file <- system.file("extdata", "CHANGELOG.md", package = "NEPScribe")

    if (!file.exists(changelog_file)) {
      stop("Changelog file not found! Make sure it is included in inst/extdata.")
    }

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

  # Collapse the sidebar by default on the Start page (not needed there); keep it open elsewhere
  shiny::observeEvent(input$nav, {
    bslib::sidebar_toggle(id = "sidebar", open = input$nav != "Start", session = session)
  })

  # --- Settings reactive for sidebar width, language, etc. ---
  settings_reactive <- settings_server("settings")

  # Send sidebar width setting to custom JS
  shiny::observeEvent(settings_reactive(), {
    session$sendCustomMessage("sidebarWidth", settings_reactive()$sidebarWidth)
  })

  # --- Shared state between Explore Datasets and Transform Data ---
  # Transform Data publishes which datasets (and cohort) its Additional Variables can take;
  # Explore Datasets sends selected variables back via add_request.
  cross_module <- shiny::reactiveValues(
    available_datasets = character(0),
    cohort = NULL,
    add_request = NULL
  )

  # --- Dataset explorer module ---
  # Uses cohort_path() reactive; can return single or multiple cohort folders
  dataset_explorer_server(
    id = "explore_dataset",
    settings_reactive = settings_reactive,
    cross_module = cross_module
  )

  # --- Data transformation module ---
  # Always uses a single cohort
  data_transformation_server(
    "data_transformation",
    settings_reactive = settings_reactive,
    cross_module = cross_module
  )
}
