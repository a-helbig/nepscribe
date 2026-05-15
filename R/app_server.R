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

  shiny::observeEvent(input$show_impressum, {
    shiny::showModal(
      shiny::modalDialog(
        shiny::HTML("
        <div style='font-family: Georgia, serif; line-height: 1.7; color: #2c2c2c;'>

          <section style='margin-bottom: 1.5rem;'>
            <h5 style='font-size: 0.75rem; font-weight: 700; letter-spacing: 0.12em;
                       text-transform: uppercase; color: #888; margin-bottom: 0.6rem;'>
              Angaben gemäß § 5 DDG und § 18 MStV
            </h5>
            <p style='margin: 0;'>
              Alexander Helbig<br>
              Hanauer Str. 23<br>
              63549 Ronneburg
            </p>
          </section>

          <section>
            <h5 style='font-size: 0.75rem; font-weight: 700; letter-spacing: 0.12em;
                       text-transform: uppercase; color: #888; margin-bottom: 0.6rem;'>
              Kontakt
            </h5>
            <p style='margin: 0;'>
              E-Mail: <a href='mailto:alexander.helbig@yahoo.de'
                         style='color: #3a6ea5; text-decoration: none;'>
                alexander.helbig@yahoo.de
              </a>
            </p>
          </section>

        </div>
      "),
        title = "Impressum",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Schließen")
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
