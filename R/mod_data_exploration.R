#' Data Exploration Module
#'
#' Internal Shiny module for exploring datasets.
#'
#' @details
#' This module provides UI and server components for selecting datasets and viewing dataset summaries.
#' It uses reactive programming to handle multiple datasets and cohort paths.
#'
#' @keywords internal
dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      inputId = ns("dataset"),
      label = htmltools::tags$b("Dataset"),
      choices = NULL,
      multiple = TRUE,
      options = list(maxItems = 1)
    ),
    shinyjs::hidden(bslib::value_box(
      title = "Dataset",
      value = shiny::textOutput(ns("dataset")),
      theme = "primary",
      id = "value_box_short1"
    )),
    shinyjs::hidden(bslib::value_box(
      title = "Observations",
      value = shiny::textOutput(ns("obs")),
      theme = "info",
      id = "value_box_short2"
    )),
    shinyjs::hidden(bslib::value_box(
      title = "Distinct ID_ts",
      value = shiny::textOutput(ns("distinct_ID_ts")),
      theme = "primary",
      id = "value_box_short3"
    )),
    shinyjs::hidden(bslib::value_box(
      title = "Variables",
      value = shiny::textOutput(ns("vars")),
      theme = "info",
      id = "value_box_short4"
    ))
  )
}

dataset_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::hidden(
      shinyWidgets::dropdown(
        inputId = "dropdown_ov",
        label = "Settings",
        icon = shiny::icon("gear"),
        width = "300px",
        tooltip = shinyWidgets::tooltipOptions(title = "Click to see Settings"),
        shinyWidgets::pickerInput(
          inputId = ns("meta_selector"),
          label = "Meta Selection",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list('actions-box' = TRUE)
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("ms_all_datasets"),
          label = "Load all datasets",
          value = FALSE,
          status = "info"
        )
      )
    ),
    shiny::fluidRow(shinycssloaders::withSpinner(DT::DTOutput(ns("data_overview")), caption = .captiontext))
  )
}

# Server
#' Data Exploration Module
#'
#' Internal Shiny module for exploring datasets.
#'
#' @details
#' This module provides UI and server components for selecting datasets and viewing dataset summaries.
#' It uses reactive programming to handle multiple datasets and cohort paths.
#'
#' @keywords internal
dataset_explorer_server <- function(id, cohort_path, settings_reactive) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Reactive: list of files in selected cohort folder
      filenames <- shiny::reactive({
        shiny::req(cohort_path())

        # List all relevant files in the cohort path
        paths <- list.files(
          path = cohort_path(),
          pattern = ".*\\d{1,2}-\\d-\\d(_beta)?\\.dta$",
          full.names = TRUE
        )

        # Replace '_D_' with '_S_' so the app works with semantic files
        # paths <- stringr::str_replace_all(paths, "_S_", "_D_")

        # Remove folder path, keep only the file names
        if (length(paths) > 0) {
          substr(paths, nchar(dirname(paths[1])) + 2, nchar(paths))
        } else {
          character(0)  # return empty character vector if no files
        }
      })

      observe(print(filenames()))

      # Update dataset select input when filenames change
      shiny::observeEvent(filenames(), {
        shiny::updateSelectInput(session, "dataset", choices = filenames(), selected = NULL)
      })

      # Show/hide value boxes depending on dataset selection
      shiny::observeEvent(input$dataset, {
        if (!is.null(input$dataset) && input$dataset != "") {
          shinyjs::show(selector = "[id^='value_box_short']")
        } else {
          shinyjs::hide(selector = "[id^='value_box_short']")
        }
      })

      # Available metadata for selected dataset
      available_meta <- shiny::reactive({
        shiny::req(input$dataset)
        meta <- process_meta_for_input_list(cohort_path(), input$dataset)
        meta <- remove_prefix_suffix_capitalize_vec(meta)
        if ("Varlabel" %in% meta) meta <- move_string_to_position(meta, "Varlabel", 1)
        if ("Questiontext" %in% meta) meta <- move_string_to_position(meta, "Questiontext", 2)
        meta
      })

      # Generate dataset overview
      data_overview_r <- shiny::reactive({
        shiny::req(input$dataset)
        language <- ifelse(settings_reactive()$language, "en", "de")
        gen_data_overview(cohort_path(), input$dataset, language = language)
      })

      # Render data overview table
      output$data_overview <- DT::renderDataTable({
        print(paste("Dataset selected: ", input$dataset))
        shiny::req(input$dataset)
        data <- data_overview_r()
        print(head(data))
        meta_selection <- input$meta_selector
        if (!settings_reactive()$language) meta_selection <- add_suffix(meta_selection, "de")
        else meta_selection <- add_suffix(meta_selection, "en")

        if (length(meta_selection) > 0) {
          data <- data |> dplyr::select(Dataset, Variable, dplyr::starts_with("NEPS_varlabel"), dplyr::any_of(meta_selection))
        } else {
          data <- data |> dplyr::select(Dataset, Variable, dplyr::starts_with("NEPS_varlabel"))
        }

        DT::datatable(
          data,
          extensions = "Buttons",
          selection = list(mode = "multiple", target = 'row'),
          options = list(
            rowCallback = htmlwidgets::JS("customRowCallback"),
            pageLength = 50,
            dom = 'lfBrtip',
            buttons = .buttons,
            searchHighlight = TRUE
          )
        )
      })
    }
  )
}
