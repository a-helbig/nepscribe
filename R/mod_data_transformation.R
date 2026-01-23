#' Module: Data Transformation
#'
#' This module provides UI and server logic for transforming NEPS SUF data
#' into a person-year format, including variable selection, spell prioritization,
#' script preview, and download.


#' UI func for the sidebar
#'
#' @keywords internal
#' @noRd
data_transformation_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$div(title = "Harmonized Format: The data preparation of life-course trajectories is based on the edited and cleaned biography file.\n\nSubspell Format: The data preparation of life-course trajectories is based on the originally recorded subspell episodes.",
                        shiny::selectizeInput(
                          ns("sub_format_select"),
                          htmltools::tags$b("Person-Year-Data: Format"),
                          choices = c("Harmonized Spell Format", "Original Subspell Format"),
                          multiple = TRUE,
                          selected = "Harmonized Spell Format",
                          options = list(maxItems = 1)
                        )),
    htmltools::tags$div(title = "Please select NEPS Starting Cohort.",
                        shiny::radioButtons(
      inputId = ns("cohort_data_trans"),
      label = htmltools::tags$b("Starting Cohort"),
      choices = c(
        "Starting Cohort 6" = "sc6_semantic_files",
        "Starting Cohort 5" = "sc5_semantic_files",
        "Starting Cohort 4" = "sc4_semantic_files",
        "Starting Cohort 3" = "sc3_semantic_files"
      ),
      selected = "sc6_semantic_files",
      inline = TRUE
    )),
    htmltools::tags$div(title = "Currently supported script formats: R or STATA.",
                        shiny::radioButtons(ns("stata_or_r"), htmltools::tags$b("Script file format"), c("STATA", "R"), selected = "STATA")),
    htmltools::tags$div(title = "Set missing values: Specific NEPS missing codes will be set to Statas missing notation '.' or NA in R. \n\n Include Parallel Spells: Variables on type and timing of parallel spell will be generated in the script.",
                        shiny::checkboxGroupInput(ns("settings"), htmltools::tags$b("Settings"), choices = c("Set Missing Values", "Include Parallel Spells"))),
    shiny::p(""),
    htmltools::tags$div(title = "Adds code for data preparation of modules, that cant simply be added via the 'Additional Variables' tab",
    shiny::checkboxGroupInput(ns("add_modules"), htmltools::tags$b("Add exemplary data preparation"), choices = c("Further Training","Children", "Highest Education"))),
    shiny::p(""),
    htmltools::tags$div(title = "Switch language of variables in the data preparation script",
    shiny::p(htmltools::HTML("<b>Variable Labels</b>")),
                            shinyWidgets::switchInput(
      ns("language"),
      label = htmltools::tags$b("Labels"),
      value = TRUE,
      onLabel = "English",
      offLabel = "German",
      onStatus = "info",
      offStatus = "success",
      inline = FALSE
    )),
    htmltools::tags$div(title = "Show a preview of the script with the actual settings.",
                        shiny::actionButton(ns("previewScript"), label = "Preview Script", shiny::icon("eye"), class = "btn btn-info")),
    htmltools::tags$div(title = "Download the script with the actual settings.",
                        shiny::downloadButton(ns("downloadScript"), label = "Download Script", class = "btn btn-info",)),
    shiny::p(""),
    htmltools::tags$div(title = "You can paste an URL to your local SUF files. This datapath will then be added at the beginning of the script.",
    shiny::p(htmltools::HTML("<b>Optional: Add local SUF URL</b>")),
                        shiny::textInput(
      inputId = ns("datapath"),
      "Datapath",
      value = "",
      placeholder = "Optional: Paste local URL",
      width = "100%"
    ))
  )
}


#' UI func for the "add variables" feature
#'
#' @keywords internal
#' @noRd
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
        shiny::actionButton(ns("confirm_variables"), "Confirm Vars", style = "width: 145px; height: 40px", class = "btn btn-info"), width = 2)
        ,
        htmltools::tags$b(shiny::div("2. Step: Select Variables"))

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
        shiny::actionButton(ns("reset_variables"), "Reset Vars", style = "width: 145px; height: 40px", class = "btn btn-info"), width = 2
      )
    )
  )
}

#' UI func for the prioritisation tab
#'
#' @keywords internal
#' @noRd
data_transformation_prio_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::HTML("To construct a person-year dataset where each row corresponds to one wave for each individual, a spell prioritization process is useful for identifying the principal spell in cases where multiple spells occur simultaneously. The following hierarchy of spell types dictates which episodes take precedence in this process, with the items at the top representing the highest priority and those at the bottom indicating the lowest priority."),
    shiny::uiOutput(ns("prio_ui"))
  )
}




#' Main func for the data trans server
#'
#' @importFrom shiny reactive observe observeEvent req updateTextInput
#' @keywords internal
#' @noRd
data_transformation_server <- function(id, settings_reactive) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

# select local NEPS suf directory -----------------------------------------

      # selected starting cohort datapath (semantic files)
      cohort_path <- shiny::reactive({
        shiny::req(input$cohort_data_trans)
        path <- system.file("extdata", input$cohort_data_trans, package = "NEPScribe")
      })

      valid_files <- shiny::reactive({
        path <- input$datapath %||% ""
        if (path == "" || !dir.exists(path)) {
          return(character(0))
        }
        list.files(path, pattern = "^SC\\d+.*\\.dta$", full.names = TRUE)
      })

      # Reactive flag is TRUE when valid files found, FALSE otherwise
      valid_path <- shiny::reactive({
        length(valid_files()) > 0
      })

      # Feedback based on valid_path
      shiny::observe({
        path <- input$datapath %||% ""
        if (path == "") {
          shinyFeedback::hideFeedback("datapath")
          return()
        }

        if (valid_path()) {
          shinyFeedback::hideFeedback("datapath")
          shinyFeedback::showFeedbackSuccess("datapath", "Success: NEPS SUF files found. Local datapath has been added to the script.")
        } else {
          shinyFeedback::hideFeedback("datapath")
          shinyFeedback::feedbackWarning(
            "datapath",
            TRUE,
            "Warning: this is either no filepath or no NEPS SUF files are being detected"
          )
        }
      })

      # reactive datapath that will be added to the script
      datapath_local <- shiny::reactive({
        if (isTRUE(valid_path())) {
          input$datapath
        } else {
          ""
        }
      })

# Update exemplary data preparation further training depending on sc --------

      shiny::observeEvent(input$cohort_data_trans, {

        if(input$cohort_data_trans == "sc6_semantic_files")
          choices <- c("Further Training","Children", "Highest Education")
        else
          choices <- c("Children", "Highest Education")

        shiny::updateCheckboxGroupInput(session,
                                        "add_modules",
                                          choices = choices,
                                          selected = input$dataset)
      })


# Spell Prioritisation  ----------------------------------------------------

      # Reactive to differentiate labels between sc3,sc4,sc6 and sc5
      labels_for_prio <- shiny::reactive({
        if(input$cohort_data_trans == "sc5_semantic_files") {
          .labels_sc5
        } else {
          .labels_sc3_4_6
        }
      })

      # Render the sortable rank_list UI dynamically
      output$prio_ui <- shiny::renderUI({
        labels <- labels_for_prio()
        sortable::rank_list(
          input_id = session$ns("prio_swap_list"),
          text = "Swap Items to change priorisation order.",
          labels = labels,
          options = sortable::sortable_options(swap = FALSE)
        )
      })


# Add Variables ---------------------------------------------------

      # List all .dta files in selected cohort
      filenames <- shiny::reactive({
        shiny::req(cohort_path())
        files <- base::list.files(cohort_path(), pattern = "*.dta", full.names = TRUE)
        substr(files, stringr::str_length(cohort_path()) + 2, 500)
      })

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
          choices = gen_comb_char(cohort_path(), input$dataset, input$language)
        )
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



      # Reset variables when either reset button is clicked or cohort changes
      shiny::observeEvent(
        list(input$reset_variables, input$cohort_data_trans),  # <-- list of triggers
        {
          # Reset the form / inputs
          shinyjs::reset("dataset")

          # update global_vars input
          shinyWidgets::updatePickerInput(
            session,
            "global_vars",
            choices = c(""),
            selected = NULL
          )

          # update multi_vars_input
          shinyWidgets::updateMultiInput(
            session,
            "multi_vars_input",
            choices = c(""),
            selected = NULL
          )

          # Reset internal reactive values
          varlist$data <- NULL
          all_lists(list())
        }
      )


# Preview script  ---------------------------------------------------------

      shiny::observeEvent(input$previewScript, {

        # Generate script (vector of lines)
        script_vector <- gen_script(
          datapath_conv = stringr::str_replace_all(cohort_path(), "\\\\", "/"),
          datapath_local = stringr::str_replace_all(datapath_local(), "\\\\", "/"),
          suf_version = extract_suf_version(cohort_path()),
          suf_version_short = extract_suf_version(cohort_path(), short = TRUE),
          dataformat = input$stata_or_r,
          subformat = input$sub_format_select,
          datalist = filter_dataframes(
            varlist$data,
            stringr::str_replace_all(input$global_vars, " - .*", "")
          ),
          prio = input$prio_swap_list,
          english = input$language,
          set_missings = "Set Missing Values" %in% input$settings,
          parallel = "Include Parallel Spells" %in% input$settings,
          further_training = "Further Training" %in% input$add_modules,
          education = "Highest Education" %in% input$add_modules,
          children = "Children" %in% input$add_modules
        )

        # Determine language class
        lang_class <- if (toupper(input$stata_or_r) == "R") "language-r" else ""

        # Wrap comment lines in <span class='hljs-comment'>
        script_text <- sapply(script_vector, function(line) {
          if (toupper(input$stata_or_r) == "R" && grepl("^\\s*#", line)) {
            paste0("<span class='hljs-comment'>", line, "</span>")
          } else if (toupper(input$stata_or_r) == "STATA" && grepl("^\\s*\\*", line)) {
            paste0("<span class='hljs-comment'>", line, "</span>")
          } else {
            line
          }
        })

        # Keep line breaks using \n
        script_text <- paste(script_text, collapse = "\n")

        # Show modal
        shiny::showModal(
          shiny::modalDialog(
            title = "Preview of script",
            size = "l",

            htmltools::tags$pre(
              htmltools::tags$code(
                class = lang_class,
                htmltools::HTML(script_text)  # <span> survives, \n works because of pre-wrap
              ),
              style = "
          max-height: 600px;
          overflow-y: auto;
          overflow-x: auto;
          white-space: pre-wrap;  /* preserves \n */
          word-break: break-word;
          background-color: #f7f7f7;
          padding: 12px;
          border-radius: 4px;
          font-family: Consolas, 'Courier New', monospace;
          font-size: 13px;
        "
            ),

            # Run highlight.js only for R (optional)
            htmltools::tags$script(
              htmltools::HTML(
                if (toupper(input$stata_or_r) == "R") {
                  "setTimeout(function() {
                document.querySelectorAll('pre code').forEach(el => {
                  // highlight all except .hljs-comment
                  hljs.highlightElement(el);
                  // force comment color again
                  el.querySelectorAll('.hljs-comment').forEach(c => {
                    c.style.color = '#2a9d8f';
                    c.style.fontStyle = 'italic';
                  });
                });
             }, 50);"
                } else {
                  ""
                }
              )
            ),

            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )

      })


# download script ---------------------------------------------------------

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
            datapath_local = stringr::str_replace_all(datapath_local(), "\\\\", "/"),
            suf_version = extract_suf_version(cohort_path()),
            suf_version_short = extract_suf_version(cohort_path(), short = TRUE),
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            datalist = filter_dataframes(varlist$data, stringr::str_replace_all(input$global_vars, " - .*", "")),
            prio = input$prio_swap_list,
            english = input$language,
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
