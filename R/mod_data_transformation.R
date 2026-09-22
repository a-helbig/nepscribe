#' Module: Data Transformation
#'
#' This module provides UI and server logic for transforming NEPS SUF data
#' into a person-year format, including variable selection, spell prioritization,
#' script preview, and download.


#' Wrap a bit of text in a Bootstrap-styled hover tooltip (grey, matches the
#' rest of the app), instead of the browser's native (black) title tooltip.
#' @keywords internal
#' @noRd
tt <- function(text, tip, bold = FALSE) {
  inner <- if (bold) htmltools::tags$b(text) else text
  htmltools::tags$span(`data-toggle` = "tooltip", title = tip, inner)
}

#' Hover text per "Settings" checkbox choice, keyed by choice value
#' @keywords internal
#' @noRd
.settings_tooltips <- c(
  "Set Missing Values" = "Specific NEPS missing codes will be set to Stata's missing notation '.' or NA in R.",
  "Include Parallel Spells" = "Variables on type and timing of parallel spell will be generated in the script.",
  "Work experience" = "Include an indicator that (retrospectively) counts the months spent in any employment.",
  "Unemployment experience" = "Include an indicator that (retrospectively) counts the months spent in any unemployment."
)

#' Hover text per "Add exemplary data preparation" checkbox choice, keyed by choice value
#' @keywords internal
#' @noRd
.add_modules_tooltips <- c(
  "Further Training" = "Adds example code for preparing:<br>1. A dummy variable on participation in further training,<br>2. A variable on the number of further training courses,<br>3. A variable on the overall hours of further training participation.<br>Currently only available for Starting Cohort 6.",
  "Children" = "Adds example code for preparing a specific indicator on whether the respondent's children are in tertiary education (SC5-SC6), or children cared for at home (SC3-SC4).",
  "Highest Education" = "Adds example code for preparing CASMIN, ISCED, and full education/qualification variables."
)

#' Build tooltip-wrapped choiceNames for a (possibly filtered) subset of choice values,
#' looking up each one's hover text from a named vector like .settings_tooltips.
#' @keywords internal
#' @noRd
tooltip_choice_names <- function(values, tooltips) {
  lapply(values, function(v) tt(v, tooltips[[v]]))
}

#' UI func for the sidebar
#'
#' @keywords internal
#' @noRd
data_transformation_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectizeInput(
      ns("sub_format_select"),
      tt("Person-Year-Data: Format", "Harmonized Format: The data preparation of life-course trajectories is based on the edited and cleaned biography file.<br><br>Subspell Format: The data preparation of life-course trajectories is based on the originally recorded subspell episodes.", bold = TRUE),
      choices = c("Harmonized Spell Format", "Original Subspell Format"),
      multiple = TRUE,
      selected = "Harmonized Spell Format",
      options = list(maxItems = 1)
    ),
    shiny::radioButtons(
      inputId = ns("cohort_data_trans"),
      label = tt("Starting Cohort", "Please select NEPS Starting Cohort.", bold = TRUE),
      choiceNames = list(
        tt("Starting Cohort 6", "Uses NEPS Starting Cohort 6 semantic structure files and scripts."),
        tt("Starting Cohort 5", "Uses NEPS Starting Cohort 5 semantic structure files and scripts."),
        tt("Starting Cohort 4", "Uses NEPS Starting Cohort 4 semantic structure files and scripts."),
        tt("Starting Cohort 3", "Uses NEPS Starting Cohort 3 semantic structure files and scripts.")
      ),
      choiceValues = c("sc6_semantic_files", "sc5_semantic_files", "sc4_semantic_files", "sc3_semantic_files"),
      selected = "sc6_semantic_files",
      inline = TRUE
    ),
    shiny::p(""),
    shiny::radioButtons(
      ns("stata_or_r"),
      tt("Script file format", "Currently supported script formats: R or STATA.", bold = TRUE),
      choiceNames = list(
        tt("STATA", "Generates a Stata do-file."),
        tt("R", "Generates an R script.")
      ),
      choiceValues = c("STATA", "R"),
      selected = "STATA"
    ),
    shiny::checkboxGroupInput(
      ns("settings"),
      tt("Settings", "Optional settings that add extra code to the generated script.", bold = TRUE),
      choiceNames = tooltip_choice_names(names(.settings_tooltips), .settings_tooltips),
      choiceValues = names(.settings_tooltips)
    ),
    shiny::p(""),
    shiny::checkboxGroupInput(
      ns("add_modules"),
      tt("Add exemplary data preparation", "Adds code for data preparation of modules, that cant simply be added via the 'Additional Variables' tab", bold = TRUE),
      choiceNames = tooltip_choice_names(names(.add_modules_tooltips), .add_modules_tooltips),
      choiceValues = names(.add_modules_tooltips)
    ),
    shiny::p(""),
    shiny::p(tt(htmltools::HTML("<b>Variable Labels</b>"), "Switch language of variables in the data preparation script")),
    shinyWidgets::switchInput(
      ns("language"),
      label = htmltools::tags$b("Labels"),
      value = TRUE,
      onLabel = "English",
      offLabel = "German",
      onStatus = "info",
      offStatus = "success",
      inline = FALSE
    ),
    shiny::actionButton(
      ns("previewScript"), label = "Preview Script", shiny::icon("eye"), class = "btn btn-info",
      `data-toggle` = "tooltip", title = "Show a preview of the script with the actual settings."
    ),
    shiny::downloadButton(
      ns("downloadScript"), label = "Download Script", class = "btn btn-info",
      `data-toggle` = "tooltip", title = "Download the script with the actual settings."
    ),
    shiny::p(""),
    shiny::p(tt(htmltools::HTML("<b>Optional: Add local SUF URL</b>"), "You can paste an URL to your local SUF files. This datapath will then be added at the beginning of the script.")),
    shiny::textInput(
      inputId = ns("datapath"),
      "Datapath",
      value = "",
      placeholder = "Optional: Paste local URL",
      width = "100%"
    )
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
      shiny::column(
        htmltools::tags$b(shiny::div("1. Step: Select Dataset")),
        width = 2
      ),
      shiny::column(
        htmltools::tags$b(shiny::div("3. Step: Confirm selected variables")),
        width = 2
      )
    ),

    shiny::fluidRow(
      shiny::column(
        shiny::selectizeInput(
          ns("dataset"),
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        width = 2
      ),
      shiny::column(
        shiny::actionButton(
          ns("confirm_variables"),
          "Confirm Vars",
          style = "width: 145px; height: 40px",
          class = "btn btn-info"
        ),
        width = 2
      ),
      htmltools::tags$b(shiny::div("2. Step: Select Variables"))
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,
        shinyWidgets::multiInput(
          ns("multi_vars_input"),
          label = NULL,
          choices = c(""),
          width = "100%",
          options = list(
            enable_search = TRUE,
            non_selected_header = "Select Variables:",
            selected_header = "You have selected:"
          )
        )
      ),
      shiny::column(
        width = 5,
        shiny::div(
          style = "width: 320px;",

          htmltools::tags$div(
            title = "Resets all datasets and variables in this UI, and removes everything already added to the script so far.",
            shiny::actionButton(
              ns("reset_variables"),
              "Reset Everything",
              style = "width: 180px; height: 40px; white-space: nowrap;",
              class = "btn btn-info"
            )
          ),
          htmltools::tags$br(),
          htmltools::tags$br(),

          htmltools::tags$b(shiny::div("Selected Variables")),
          htmltools::tags$br(),

          shiny::div(
            style = "max-height: 350px; overflow-y: auto;",
            shiny::uiOutput(ns("variable_summary"))
          ),
          htmltools::tags$br(),
          htmltools::tags$div(
            style = paste(
              "width: 100%;",
              "box-sizing: border-box;",
              "min-height: 120px;",
              "padding: 12px 14px;",
              "background-color: #f1f8ff;",
              "border: 1px solid #cfe2ff;",
              "border-radius: 8px;",
              "font-family: inherit;",
              "font-size: 0.95rem;",
              "line-height: 1.5;"
            ),
            htmltools::tags$b("Note"),
            htmltools::tags$br(),
            htmltools::tags$br(),
            htmltools::tags$p(
              style = "margin-bottom: 0.5rem;",
              "1. Not all NEPS datasets are available here: some (e.g. spFurtherEdu1, spChild, spPartner) need additional data preparation before they can be merged into a person-year dataset. For some of these, we instead provide exemplary data preparation code in the sidebar. More examples are planned for future releases."
            ),
            htmltools::tags$p(
              style = "margin-bottom: 0.5rem;",
              "2. Note that if you do not use the current SUF version, you might accidentally add variables here that do not yet exist in your SUF version."
            ),
            htmltools::tags$p(
              style = "margin-bottom: 0;",
              "3. Since the semantic structured files are based on remote SUF versions, some variable choices here might not be available in the download SUF version. We are working on a solution for this issue."
            )
          )
        )
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

      # valid_files <- shiny::reactive({
      #   path <- input$datapath %||% ""
      #   if (path == "" || !dir.exists(path)) {
      #     return(character(0))
      #   }
      #   list.files(path, pattern = "^SC\\d+.*\\.dta$", full.names = TRUE)
      # })
      #
      # # Reactive flag is TRUE when valid files found, FALSE otherwise
      # valid_path <- shiny::reactive({
      #   length(valid_files()) > 0
      # })

      # Feedback based on valid_path
      # shiny::observe({
      #   path <- input$datapath %||% ""
      #   if (path == "") {
      #     shinyFeedback::hideFeedback("datapath")
      #     return()
      #   }
      #
      #   if (valid_path()) {
      #     shinyFeedback::hideFeedback("datapath")
      #     shinyFeedback::showFeedbackSuccess("datapath", "Success: NEPS SUF files found. Local datapath has been added to the script.")
      #   } else {
      #     shinyFeedback::hideFeedback("datapath")
      #     shinyFeedback::feedbackWarning(
      #       "datapath",
      #       TRUE,
      #       "Warning: this is either no filepath or no NEPS SUF files are being detected"
      #     )
      #   }
      # })

      # reactive datapath that will be added to the script
      datapath_local <- shiny::reactive({
          input$datapath
      })

      # give warning when users select sc3-sc5 --------
      shiny::observeEvent(input$cohort_data_trans, {

        # 1. Specific Warning for sc3
        if (input$cohort_data_trans == "sc3_semantic_files") {
          shiny::showModal(
            shiny::modalDialog(
              shiny::HTML("
          <div style='display: flex; align-items: flex-start; gap: 1rem;'>
            <span style='font-size: 1.5rem;'>\u26a0\ufe0f</span>
            <p style='margin: 0; line-height: 1.6;'>
              The script for the selected starting cohort has not been fully tested yet
              and may contain errors. Please use it with caution and run scripts line by line.
              <br><br>
              <strong>Note:</strong> Full biographies, including all person-years of schooling,
              are only available when selecting the 'harmonized spell format'.
            </p>
          </div>
        "),
              title = "Warning: Script not fully tested (SC3)",
              size = "m",
              easyClose = TRUE,
              footer = shiny::modalButton("Understood")
            )
          )

          # 2. General Warning for sc4 and sc5
        } else if (input$cohort_data_trans %in% c("sc4_semantic_files", "sc5_semantic_files")) {
          shiny::showModal(
            shiny::modalDialog(
              shiny::HTML("
          <div style='display: flex; align-items: flex-start; gap: 1rem;'>
            <span style='font-size: 1.5rem;'>\u26a0\ufe0f</span>
            <p style='margin: 0; line-height: 1.6;'>
              The script for the selected starting cohort has not been fully tested yet
              and may contain errors. Please use it with caution and run scripts line by line.
            </p>
          </div>
        "),
              title = "Warning: Script was not tested for this cohort",
              size = "m",
              easyClose = TRUE,
              footer = shiny::modalButton("Understood")
            )
          )
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
                                        choiceNames = tooltip_choice_names(choices, .add_modules_tooltips),
                                        choiceValues = choices)
      })

# Update settings options depending on format --------

shiny::observeEvent(input$sub_format_select, {

  if(input$sub_format_select == "Harmonized Spell Format")
    choices <- c("Set Missing Values", "Include Parallel Spells", "Work experience", "Unemployment experience")
  else
    choices <- c("Set Missing Values", "Include Parallel Spells")

  shiny::updateCheckboxGroupInput(session,
                                  "settings",
                                  choiceNames = tooltip_choice_names(choices, .settings_tooltips),
                                  choiceValues = choices)
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

      # Update multiInput when dataset is selected, or when the language is switched
      # while a dataset is already loaded (so labels/question texts follow the switch too)
      shiny::observeEvent(list(input$dataset, input$language), {
          shiny::req(input$dataset)

          shiny::showModal(
            shiny::modalDialog(
              title = "Loading variables",
              "This may take a few seconds depending on the size of the dataset. Please wait.",
              easyClose = FALSE,
              footer = NULL
            )
          )


        # Clear the multiInput immediately
        shinyWidgets::updateMultiInput(
          session,
          "multi_vars_input",
          label = NULL,
          selected = character(0),
          choices = character(0)
        )

        # Compute the variable list
        new_choices <- gen_comb_char(cohort_path(), input$dataset, input$language)

        # Update multiInput with the new choices
        shinyWidgets::updateMultiInput(
          session,
          "multi_vars_input",
          label = NULL,
          selected = character(0),
          choices = new_choices
        )

        # Send question texts for the new choices, so the picker can show them as hover tooltips.
        # Variables without one (mostly derived/administrative vars) get a fallback message
        # instead of no tooltip at all, so hovering them doesn't look like it's just not working.
        qtexts <- gen_comb_questiontext(cohort_path(), input$dataset, input$language)
        vars_short <- stringr::str_replace_all(new_choices, " - .*", "")
        qtext_values <- base::unname(qtexts[vars_short])
        qtext_values[base::is.na(qtext_values)] <- "No question text available for this variable."
        qtext_map <- stats::setNames(as.list(qtext_values), new_choices)
        session$sendCustomMessage("variableQuestiontexts", qtext_map)

        # Close modal
          shiny::removeModal()
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

        # delete variable label and the - from selected vars
        vars_vec_short <- stringr::str_replace_all(input$multi_vars_input, " - .*", "")

        # create a df with selected dataset and selected variables
        dataframe <- create_dataframe(input$dataset, vars_vec_short)

        # join linkage/merge key information from a package filed named linkage_keys.csv to the dataframe so each dataset gets the appropriate merge variables
        dataframe <- dplyr::left_join(dataframe, create_linkage_data(cohort_path()), by = "Dataset")

        # put this df into the list of dfs
        varlist$data[[input$dataset]] <- dataframe

        # Add the selected variables list under the selected dataset; the
        # variable_summary accordion re-renders reactively off this.
        new_list <- gen_list_for_picker(input$dataset, input$multi_vars_input)
        # read the current stored list into current_lists
        current_lists <- all_lists()
        # add the selected variables list to current_lists under the selected dataset
        current_lists[[input$dataset]] <- new_list
        # save the updated combined list back into the reactive value. This is how the app remembers all dataset-variable lists across clicks.
        all_lists(current_lists)

        # confirmation/success message popup
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

      # Re-label already-confirmed variables when the language toggle changes, without touching
      # the underlying selection: create_dataframe()/varlist$data only ever stores bare variable
      # names, never labels, so the generated script itself was always language-consistent - this
      # only fixes the accordion display, which otherwise kept showing whatever language was
      # active when each dataset was confirmed (mixed languages across datasets confirmed at
      # different times).
      shiny::observeEvent(input$language, {
        current_lists <- all_lists()
        if (base::length(current_lists) == 0) return()

        relabeled <- base::lapply(base::names(current_lists), function(dataset) {
          vars_short <- stringr::str_replace_all(base::unname(base::unlist(current_lists[[dataset]])), " - .*", "")

          all_choices <- gen_comb_char(cohort_path(), dataset, input$language)
          all_vars_short <- stringr::str_replace_all(all_choices, " - .*", "")

          new_choices <- all_choices[base::match(vars_short, all_vars_short)]
          new_choices <- new_choices[!base::is.na(new_choices)]
          gen_list_for_picker(dataset, new_choices)
        })
        base::names(relabeled) <- base::names(current_lists)
        all_lists(relabeled)
      })

      # Build a stable, valid Shiny-input-id fragment identifying a dataset
      dataset_short_id <- function(dataset) {
        short <- stringr::str_match(dataset, "SC\\d+_(.*?)_S")[, 2]
        base::gsub("[^A-Za-z0-9_]", "_", short)
      }

      # Collapsible per-dataset summary of confirmed variables, replacing the old global_vars picker.
      # Each dataset's checkboxes double as the include/exclude filter used at script-generation time.
      output$variable_summary <- shiny::renderUI({
        current_lists <- all_lists()

        if (base::length(current_lists) == 0) {
          return(shiny::div(style = "color: #888; font-style: italic;", "No variables selected yet."))
        }

        panels <- base::lapply(base::names(current_lists), function(dataset) {
          vars <- base::unname(base::unlist(current_lists[[dataset]]))
          short <- stringr::str_match(dataset, "SC\\d+_(.*?)_S")[, 2]

          bslib::accordion_panel(
            title = base::paste0(short, " (", base::length(vars), ")"),
            htmltools::tags$a(
              href = "#",
              style = "font-size: 0.85rem;",
              onclick = base::sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                session$ns("remove_dataset"), dataset
              ),
              "Remove this dataset"
            ),
            shiny::checkboxGroupInput(
              session$ns(base::paste0("include_", dataset_short_id(dataset))),
              label = NULL,
              choices = vars,
              selected = vars
            )
          )
        })

        base::do.call(bslib::accordion, c(panels, list(open = TRUE)))
      })

      # Currently-included variables (bare varnames) across all confirmed datasets;
      # replaces input$global_vars as the include/exclude filter for script generation.
      included_vars <- shiny::reactive({
        current_lists <- all_lists()
        selected <- base::unlist(base::lapply(base::names(current_lists), function(dataset) {
          input[[base::paste0("include_", dataset_short_id(dataset))]]
        }))
        stringr::str_replace_all(selected, " - .*", "")
      })

      # Remove a single dataset's confirmed variables (triggered by the "Remove this dataset" link)
      shiny::observeEvent(input$remove_dataset, {
        varlist$data[[input$remove_dataset]] <- NULL

        current_lists <- all_lists()
        current_lists[[input$remove_dataset]] <- NULL
        all_lists(current_lists)
      })


# Preview script  ---------------------------------------------------------

      shiny::observeEvent(input$previewScript, {

        # Generate script (vector of lines)
        script_vector <- gen_script(
          datapath_conv = stringr::str_replace_all(cohort_path(), "\\\\", "/"),
          datapath_local = stringr::str_replace_all(datapath_local(), "\\\\", "/"),
          suf_version = extract_suf_version(cohort_path()),
          dataformat = input$stata_or_r,
          subformat = input$sub_format_select,
          datalist = filter_dataframes(
            varlist$data,
            included_vars()
          ),
          prio = input$prio_swap_list,
          english = input$language,
          set_missings = "Set Missing Values" %in% input$settings,
          parallel = "Include Parallel Spells" %in% input$settings,
          work_exp = "Work experience" %in% input$settings,
          unemp_exp = "Unemployment experience" %in% input$settings,
          further_training = "Further Training" %in% input$add_modules,
          education = "Highest Education" %in% input$add_modules,
          children = "Children" %in% input$add_modules
        )

        # Determine language class for formatting
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

            htmltools::tags$code(
              class = lang_class,
              style = "
        display: block;
        max-height: 600px;
        overflow-y: auto;
        overflow-x: auto;
        white-space: pre;
        margin: 0;
        background-color: #f7f7f7;
        padding: 12px;
        border-radius: 4px;
        font-family: Consolas, 'Courier New', monospace;
        font-size: 13px;
      ",
              htmltools::HTML(script_text)
            ),

            htmltools::tags$script(
              htmltools::HTML(
                if (toupper(input$stata_or_r) == "R") {
                  "setTimeout(function() {
             document.querySelectorAll('code.language-r').forEach(el => {
               hljs.highlightElement(el);
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
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            datalist = filter_dataframes(varlist$data, included_vars()),
            prio = input$prio_swap_list,
            english = input$language,
            set_missings = "Set Missing Values" %in% input$settings,
            parallel = "Include Parallel Spells" %in% input$settings,
            work_exp = "Work experience" %in% input$settings,
            unemp_exp = "Unemployment experience" %in% input$settings,
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
