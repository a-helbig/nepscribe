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
  # shiny::tagList(
  #     shinyWidgets::dropdown(
        # inputId = "dropdown_ov",
        # label = "Settings",
        # icon = shiny::icon("gear"),
        # width = "300px",
        # tooltip = shinyWidgets::tooltipOptions(title = "Click to see Settings"),
        # shinyWidgets::pickerInput(
        #   inputId = ns("meta_selector"),
        #   label = "Meta Selection",
        #   choices = NULL,
        #   selected = NULL,
        #   multiple = TRUE,
        #   options = list('actions-box' = TRUE)
        # ),
        # shinyWidgets::materialSwitch(
        #   inputId = ns("ms_all_datasets"),
        #   label = "Load all datasets",
        #   value = FALSE,
        #   status = "info"
        # )
      # ),
    shiny::fluidRow(shinycssloaders::withSpinner(DT::DTOutput(ns("data_overview")), caption = .captiontext))
  # )
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

        # Remove folder path, keep only the file names
        if (length(paths) > 0) {
          substr(paths, nchar(dirname(paths[1])) + 2, nchar(paths))
        } else {
          character(0)  # return empty character vector if no files
        }
      })

      # Update dataset select input when filenames change
      shiny::observeEvent(filenames(), {
        shiny::updateSelectInput(session, "dataset", choices = filenames(), selected = NULL)
      })

      # when a datapath is provided, reset the materialswitch that loads all vars from all datasets to FALSE
      shiny::observeEvent(cohort_path(), {
        shinyWidgets::updateMaterialSwitch(session = session, "ms_all_vars", value = F)
      })

      # Show/hide value boxes depending on dataset selection
      # shiny::observeEvent(input$dataset, {
      #   if (!is.null(input$dataset) && input$dataset != "") {
      #     shinyjs::show(selector = "[id^='value_box_short']")
      #   } else {
      #     shinyjs::hide(selector = "[id^='value_box_short']")
      #   }
      # })

      shiny::observeEvent(input$dataset, {
      #   shinyjs::show(id = "ms_all_datasets")
      #   shinyjs::show(id = "meta_selector")
      #   shinyjs::show(id = "sw-drop-dropdown_ov", asis =T)
      #   shinyjs::show(id = "sw-drop-dropdown_br", asis =T)
      #   shinyjs::show(id = "ms_all_vars")
      #   shinyjs::show(id = "var_selector")
      #   if(input$ms_all_datasets==F){
      #     shinyjs::show(selector = "[id^='value_box_short']") # show all objects with ids that start with that
      #   }
        shinyWidgets::updateMaterialSwitch(session = session, "ms_all_datasets", value = F)
      }
      )

      # shiny::observeEvent(input$ms_all_datasets == T, {
      #   shinyjs::hide(selector = "[id^='value_box_short']")
      # })

      # observeEvent(input$ms_all_datasets == F, {
      #   shinyjs::show(selector = "[id^='value_box_short']")
      # })

      # observe({
      #   cat("namespaced meta_selector id:",
      #       session$ns("meta_selector"), "\n")
      # })




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
            generate_info(cohort_path(), input$dataset)[[1]]
          })
          output$vars <- shiny::renderText({
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

      # observe({
      #   cat("dataset =", input$dataset, "\n")
      #   cat("ms_all_datasets =", input$ms_all_datasets, "\n")
      # })


      # Available metadata for selected dataset
      available_meta <- shiny::reactive({
        shiny::req(input$dataset)
        meta <- process_meta_for_input_list(cohort_path(), input$dataset)
        meta <- remove_prefix_suffix_capitalize_vec(meta)
        if ("Varlabel" %in% meta) meta <- move_string_to_position(meta, "Varlabel", 1)
        if ("Questiontext" %in% meta) meta <- move_string_to_position(meta, "Questiontext", 2)
      })


      # reactive for the data variable table on dataset exploration. It either generates the table with all vars from selected dataset or from all datasets if the input materialswitch is TRUE
      data_overview_r <- shiny::reactive({
        shiny::req(input$dataset)
        # first determine language dependent on the materialswitch input "language"
        if (settings_reactive()$language ==F) { language = "de" }
        else {                    language = "en" }

        # then determine if only the selected or all datasets should be loaded dependent on the materialswitch input "ms_all_datasets"
        if(input$ms_all_datasets==F){
          gen_data_overview(cohort_path(), input$dataset, language =language)
        }
        else {
          dataframes <- purrr::map(filenames(),~ gen_data_overview(cohort_path(), .x, language =language)) # show all variables in all datasets by creating a list of dataframes that are the variable tables from all datasets - this will crash if there are datasets in the provided datapath that contain datasets witout the neps expansionfields - we should catch this
          do.call(dplyr::bind_rows,dataframes) # this appends all these dfs together and makes a huge table
        }
      })


      # Render data overview table
      output$data_overview <- DT::renderDataTable({

        shiny::req(input$dataset)
        data <- data_overview_r()

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
