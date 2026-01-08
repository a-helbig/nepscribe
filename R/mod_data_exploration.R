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
    shiny::radioButtons(
      inputId = ns("cohort_data_explore"),
      label = htmltools::tags$b("Select Starting Cohort"),
      choices = c(
        "Starting Cohort 1" = "sc1_semantic_files",
        "Starting Cohort 2" = "sc2_semantic_files",
        "Starting Cohort 3" = "sc3_semantic_files",
        "Starting Cohort 4" = "sc4_semantic_files",
        "Starting Cohort 5" = "sc5_semantic_files",
        "Starting Cohort 6" = "sc6_semantic_files",
        "Starting Cohort 8" = "sc8_semantic_files"
      ),
      selected = "sc6_semantic_files",
      inline = TRUE
    ),
    shiny::selectizeInput(
      inputId = ns("dataset"),
      label = htmltools::tags$b("Dataset"),
      choices = NULL,
      multiple = TRUE,
      options = list(maxItems = 1)
    ),
    shinyWidgets::pickerInput(
      inputId = ns("meta_selector"),
      label = htmltools::tags$b("Meta Selection"),
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list('actions-box' = TRUE)
    ),
    shinyWidgets::materialSwitch(
      inputId = ns("ms_all_datasets"),
      label = htmltools::tags$b("Load all datasets"),
      value = FALSE,
      status = "info"
    ),
    bslib::value_box(
      title = "Dataset",
      value = shiny::textOutput(ns("dataset")),
      theme = "primary",
      id = "value_box_short1"
    ),
    bslib::value_box(
      title = "Variables",
      value = shiny::textOutput(ns("vars")),
      theme = "info",
      id = "value_box_short4"
    )
  )
}

dataset_overview_ui <- function(id) {
  ns <- shiny::NS(id)
    shiny::fluidRow(shinycssloaders::withSpinner(DT::DTOutput(ns("data_overview")), caption = .captiontext))
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

      # selected starting cohort datapath (semantic files)
      cohort_path <- shiny::reactive({
        shiny::req(input$cohort_data_explore)
        path <- system.file("extdata", input$cohort_data_explore, package = "NEPScribe")
      })

      # Reactive: list of files in selected cohort folder
      filenames <- shiny::reactive({
        shiny::req(cohort_path())

        # List all relevant files in the cohort path
        paths <- list.files(
          path = cohort_path(),
          pattern = ".*\\d{1,2}-\\d-\\d(_beta)?\\.dta$",
          full.names = TRUE
        )

        # Remove folder path, keep only the file names
        if (length(paths) > 0) {
          substr(paths, nchar(dirname(paths[1])) + 2, nchar(paths))
        } else {
          character(0)  # return empty character vector if no files
        }
      })

      # Observer to clear dataset input when "Load all datasets" is ON
      shiny::observe({
        if (isTRUE(input$ms_all_datasets)) {
          shiny::updateSelectInput(session, "dataset",
                                   choices = character(0),
                                   selected = NULL)
        }
      })

      # when a datapath is provided, reset the materialswitch that loads all vars from all datasets to FALSE
      shiny::observeEvent(cohort_path(), {
        shinyWidgets::updateMaterialSwitch(session = session, "ms_all_vars", value = F)
      })

      shiny::observeEvent(input$dataset, {
        shinyWidgets::updateMaterialSwitch(session = session, "ms_all_datasets", value = F)
      }
      )

      # Observer to update dataset choices ONLY if "Load all datasets" is OFF
      shiny::observe({
        req(filenames())                # filenames must be available
        if (isFALSE(input$ms_all_datasets)) {
          shiny::updateSelectInput(session, "dataset",
                                   choices = filenames(),
                                   selected = NULL)
        }
      })

      # when a dataset is selected or all datasets ms is selected, update meta selector
      shiny::observeEvent(list(input$dataset, input$ms_all_datasets), {

        meta <- available_meta()
        shiny::freezeReactiveValue(input, "meta_selector") # freeze here in order to stop execution of update before available_meta() is available
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "meta_selector",
          choices = meta,
          selected = intersect(c("Varlabel", "Questiontext"), meta)
        )

        # text for the info boxes for specific dataset
        if (input$ms_all_datasets == F) {
          output$dataset <- shiny::renderText({
            # Dataset must be selected
            req(input$dataset)

            # Get full file path
            file_path <- file.path(cohort_path(), input$dataset)

            # Only generate info if file exists
            if (!file.exists(file_path)) {
              return("Dataset not available in this cohort")
            }

            # Safe to call generate_info
            generate_info(cohort_path(), input$dataset)[[1]]
          })

          output$vars <- shiny::renderText({
            req(input$dataset)
            file_path <- file.path(cohort_path(), input$dataset)

            if (!file.exists(file_path)) {
              return("-")
            }

            generate_info(cohort_path(), input$dataset)[[4]]
          })

        }
        # text for the info boxes for all datasets
        else {
          output$dataset <- shiny::renderText({
            "All Datasets"
          })
          output$vars <- shiny::renderText({
            nrow(data_overview_r())
          })
        }
      }
      )

      # Available metadata for selected dataset
      available_meta <- shiny::reactive({
        shiny::req(input$dataset)
        meta <- process_meta_for_input_list(cohort_path(), input$dataset)
        meta <- remove_prefix_suffix_capitalize_vec(meta)
        if ("Varlabel" %in% meta) meta <- move_string_to_position(meta, "Varlabel", 1)
        if ("Questiontext" %in% meta) meta <- move_string_to_position(meta, "Questiontext", 2)
      })


      data_overview_r <- shiny::reactive({
        # Wait until dataset or cohort_path is available
        shiny::req(input$dataset, cohort_path())

        language <- if (settings_reactive()$language) "en" else "de"

        # Single dataset
        if(input$ms_all_datasets==F){
          file_path <- file.path(cohort_path(), input$dataset)
          if (!file.exists(file_path)) return(NULL)
          gen_data_overview(cohort_path(), input$dataset, language = language)

        } else {
          # All datasets: filter only files that exist
          existing_files <- filenames()[file.exists(file.path(cohort_path(), filenames()))]
          if (length(existing_files) == 0) return(NULL)

          dataframes <- purrr::map(existing_files, ~ gen_data_overview(cohort_path(), .x, language = language))
          do.call(dplyr::bind_rows, dataframes)
        }
      })



      # Render data overview table
      output$data_overview <- DT::renderDataTable({

        data <- data_overview_r()
        shiny::req(data)  # stop here if NULL

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
