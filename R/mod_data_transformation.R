#' Module: Data Transformation
#'
#' This module provides UI and server logic for transforming NEPS SUF data
#' into a person-year format, including variable selection, spell prioritization,
#' script preview, and download.
#'
#' @keywords internal
data_transformation_add_variables_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(htmltools::tags$b(shiny::div("1. Step: Select Dataset")), width = 2),
      shiny::column(htmltools::tags$b(shiny::div("3. Step: Confirm selected variables")), width = 2),
    ),
    shiny::fluidRow(
      shiny::column(shiny::selectizeInput(ns("dataset"), label = NULL, choices = NULL, multiple = TRUE, options = list(maxItems = 1)), width = 2),
      shiny::column(
        shiny::actionButton(ns("confirm_variables"), "Confirm Vars", style = "width: 145px; height: 40px"), width = 2
        ,
        htmltools::tags$b(shiny::div("2. Step: Select Variables"))
      )
    ),
    shiny::fluidRow(
      shinyWidgets::multiInput(
        ns("multi_vars_input"), label = NULL,  choices = c(""), width = "50%",  options = list(
          enable_search = TRUE,
          non_selected_header = "Select Variables:",
          selected_header = "You have selected:"
        )
      ),
      shiny::column(
        htmltools::tags$b(shiny::div("Inspect selected Variables or reset everything (Button)")),
        shinyWidgets::pickerInput(
          ns("global_vars"),
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          selected = NULL,
          options = list('actions-box' = TRUE)
        ),
        shiny::actionButton(ns("reset_variables"), "Reset Vars", style = "width: 145px; height: 40px"), width = 2
      )
    )
  )
}

data_transformation_prio_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::HTML("To construct a person-year dataset where each row corresponds to one wave for each individual, a spell prioritization process is useful for identifying the principal spell in cases where multiple spells occur simultaneously. The following hierarchy of spell types dictates which episodes take precedence in this process, with the items at the top representing the highest priority and those at the bottom indicating the lowest priority."),
    sortable::rank_list(
      input_id = ns("prio_swap_list"),
      text = "Swap Items to change priorisation order.",
      labels = .labels,
      options = sortable::sortable_options(swap = FALSE)
    )
  )
}

data_transformation_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$div(title = "Harmonized Format: The data preparation of life-course trajectories is based on the edited and cleaned biography file. Subspell Format: The data preparation of life-course trajectories is based on the originally recorded subspell episodes.",
                        shiny::selectizeInput(
                          ns("sub_format_select"),
                          htmltools::tags$b("Person-Year-Data: Format"),
                          choices = c("Harmonized Spell Format (Recommended)", "Original Subspell Format"),
                          multiple = TRUE,
                          selected = "Harmonized Spell Format (Recommended)",
                          options = list(maxItems = 1)
                        )),
    htmltools::tags$div(title = "Currently supported script formats: R or STATA.",
                        shiny::radioButtons(ns("stata_or_r"), htmltools::tags$b("Script file format"), c("STATA", "R"), selected = "R")),
    shiny::checkboxGroupInput(ns("settings"), htmltools::tags$b("Settings"), choices = c("Set Missing Values","Script: English Labels", "Include Parallel Spells")),
    shiny::p(""),
    shiny::checkboxGroupInput(ns("add_modules"), htmltools::tags$b("Add exemplary data preparation"), choices = c("Further Training","Partner","Children", "Highest Education")),
    shiny::p(""),
    htmltools::tags$div(title = "Show a preview of the script with the actual settings.",
                        shiny::actionButton(ns("previewScript"), "Preview Script", shiny::icon("play-circle"))),
    htmltools::tags$div(title = "Download the script with the actual settings.",
                        shiny::downloadButton(ns("downloadScript"), "Download Script")),
    htmltools::tags$div(title = "Generate the data according to the generated script",
                        shiny::actionButton(ns("generateData"), "Generate Data", shiny::icon("database")))
  )
}


# Server
data_transformation_server <- function(id, cohort_path, settings_reactive) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Update dataset select input when cohort changes
      shiny::observeEvent(cohort_path(), {
        shiny::updateSelectInput(
          session,
          "dataset",
          label = NULL,
          selected = NULL,
          choices = datasets()
        )
      })

      # Update multiInput when dataset is selected
      shiny::observeEvent(input$dataset, {
        shinyWidgets::updateMultiInput(
          session,
          "multi_vars_input",
          label = NULL,
          selected = NULL,
          choices = gen_comb_char(cohort_path(), input$dataset, settings_reactive()$language)
        )
      })

      # List all .dta files in selected cohort
      filenames <- shiny::reactive({
        shiny::req(cohort_path())
        files <- base::list.files(cohort_path(), pattern = "*.dta", full.names = TRUE)
        substr(files, stringr::str_length(cohort_path()) + 2, 500)
      })

      # Reactive list of dataset names for select input
      datasets <- shiny::reactive({
        shiny::req(cohort_path())
        create_dataset_names(cohort_path())
      })

      # Reactive values for selected variables
      varlist <- shiny::reactiveValues(data = list())
      all_lists <- shiny::reactiveVal(list())

      # Confirm selected variables
      shiny::observeEvent(input$confirm_variables, {
        shiny::req(input$dataset, input$multi_vars_input)

        vars_vec_short <- stringr::str_replace_all(input$multi_vars_input, " - .*", "")

        dataframe <- create_dataframe(input$dataset, vars_vec_short)

        dataframe <- dplyr::left_join(dataframe, create_linkage_data(cohort_path()), by = "Dataset")

        varlist$data[[input$dataset]] <- dataframe

        # Update picker input for global_vars
        new_list <- gen_list_for_picker(input$dataset, input$multi_vars_input)
        current_lists <- all_lists()
        current_lists[[input$dataset]] <- new_list
        all_lists(current_lists)

        shinyWidgets::updatePickerInput(
          session,
          "global_vars",
          choices = current_lists,
          selected = base::unique(base::unlist(current_lists))
        )

        shinyalert::shinyalert(
          title = "",
          text = "Dataset, selected variables and merge procedure added to script. You may continue with another dataset.",
          size = "xs",
          closeOnClickOutside = TRUE,
          type = "success",
          confirmButtonCol = "#AEDEF4"
        )
      })

      # Reset variables
      shiny::observeEvent(input$reset_variables, {
        shinyjs::reset("dataset")
        shinyWidgets::updatePickerInput(
          session,
          "global_vars",
          choices = c(""),
          selected = NULL
        )
        shinyWidgets::updateMultiInput(
          session,
          "multi_vars_input",
          choices = c(""),
          selected = NULL
        )
        varlist$data <- NULL
        all_lists(list())
      })

      # Preview script modal
      shiny::observeEvent(input$previewScript, {
        shiny::showModal(shiny::modalDialog(
          title = "Preview of script",
          size = "l",
          htmltools::HTML(base::paste(
            gen_script(
              datapath_conv = stringr::str_replace_all(cohort_path(), "\\\\", "/"),
              suf_version = extract_suf_version(cohort_path()),
              suf_version_short = extract_suf_version(cohort_path(), short = TRUE),
              dataformat = input$stata_or_r,
              subformat = input$sub_format_select,
              datalist = filter_dataframes(varlist$data, stringr::str_replace_all(input$global_vars, " - .*", "")),
              prio = input$prio_swap_list,
              english = "Script: English Labels" %in% input$settings,
              set_missings = "Set Missing Values" %in% input$settings,
              parallel = "Include Parallel Spells" %in% input$settings,
              further_training = "Further Training" %in% input$add_modules,
              education = "Highest Education" %in% input$add_modules,
              children = "Children" %in% input$add_modules
            ), collapse = "<br>"
          )),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
      })

      # Generate data
      shiny::observeEvent(input$generateData, {
        shiny::withProgress(message = 'Processing data, please wait...', value = 0, {
          shiny::showModal(shiny::modalDialog(
            title = "Note",
            size = "s",
            htmltools::HTML("Please wait while data is being processed. The resulting dataset is generated according to your settings and is identical to the dataset you would receive by downloading and executing the script."),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          ))
          gen_data(
            datapath = stringr::str_replace_all(cohort_path(), "\\\\", "/"),
            suf_version = extract_suf_version(cohort_path()),
            suf_version_short = extract_suf_version(cohort_path(), short = TRUE),
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            datalist = filter_dataframes(varlist$data, stringr::str_replace_all(input$global_vars, " - .*", "")),
            prio = input$prio_swap_list,
            english = "Script: English Labels" %in% input$settings,
            set_missings = "Set Missing Values" %in% input$settings,
            parallel = "Include Parallel Spells" %in% input$settings,
            further_training = "Further Training" %in% input$add_modules,
            education = "Highest Education" %in% input$add_modules,
            children = "Children" %in% input$add_modules
          )
        })
      })

      # Download script
      output$downloadScript <- shiny::downloadHandler(
        filename = function() {
          if (input$stata_or_r == "R") {
            "data_wrangling_script.R"
          } else {
            "data_wrangling_script.do"
          }
        },
        content = function(file) {
          script_harm <- gen_script(
            datapath_conv = stringr::str_replace_all(cohort_path(), "\\\\", "/"),
            suf_version = extract_suf_version(cohort_path()),
            suf_version_short = extract_suf_version(cohort_path(), short = TRUE),
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            datalist = filter_dataframes(varlist$data, stringr::str_replace_all(input$global_vars, " - .*", "")),
            prio = input$prio_swap_list,
            english = "Script: English Labels" %in% input$settings,
            set_missings = "Set Missing Values" %in% input$settings,
            parallel = "Include Parallel Spells" %in% input$settings,
            further_training = "Further Training" %in% input$add_modules,
            education = "Highest Education" %in% input$add_modules,
            children = "Children" %in% input$add_modules
          )
          writeLines(script_harm, file)
        },
        contentType = "text/plain"
      )
    }
  )
}
