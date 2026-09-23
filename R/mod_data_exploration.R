#' UI func
#' @keywords internal
#' @noRd
dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    # Multi-select cohort picker
    shinyWidgets::pickerInput(
      inputId = ns("cohort_data_explore"),
      label = tt("Select Starting Cohorts", "Select one or multiple starting cohorts.", bold = TRUE),
      choices = c(
        "SC1" = "sc1_semantic_files",
        "SC2" = "sc2_semantic_files",
        "SC3" = "sc3_semantic_files",
        "SC4" = "sc4_semantic_files",
        "SC5" = "sc5_semantic_files",
        "SC6" = "sc6_semantic_files",
        "SC8" = "sc8_semantic_files"
      ),
      # selected = "sc6_semantic_files",
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deselect All",
        `select-all-text` = "Select All"
      )
    ),

    # Multi-select dataset picker
    shinyWidgets::pickerInput(
      inputId = ns("dataset"),
      label = tt("Dataset", "Select one or multiple datasets of the selected starting cohort(s).", bold = TRUE),
      choices = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deselect All",
        `select-all-text` = "Select All",
        `live-search` = TRUE,
        `tick-icon` = "glyphicon glyphicon-ok"  # optional, default is check
      ),
      width = "100%"
    ),

    # Meta selector
    shinyWidgets::pickerInput(
      inputId = ns("meta_selector"),
      label = tt("Meta Selection", "Filter the datatable with available meta infos on variables in the selected datasets", bold = TRUE),
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Deselect All",
        `select-all-text` = "Select All",
        `live-search` = TRUE,
        `tick-icon` = "glyphicon glyphicon-ok"  # optional, default is check
      ),
      width = "100%"
    ),
    shiny::p(tt(htmltools::HTML("<b>Variable Labels</b>"), "Switch language of variables in the datatable")),
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
    # Value boxes
    bslib::value_box(
      title = "Datasets",
      value = shiny::uiOutput(ns("dataset_summary")),
      theme = "primary",
      id = "value_box_short1"
    )
    ,
    bslib::value_box(
      title = "Variables",
      value = shiny::textOutput(ns("vars_summary")),
      theme = "info",
      id = "value_box_short4"
    )
  )
}

#' UI func for output
#' @keywords internal
#' @noRd
dataset_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinycssloaders::withSpinner(
    DT::DTOutput(ns("data_overview")),
    caption = .captiontext
    )
  )
}

#' Server funcs
#'
#' @importFrom shiny reactive observe observeEvent req updateTextInput
#' @keywords internal
#' @noRd
dataset_explorer_server <- function(id, settings_reactive, cross_module) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # --- Reactive: gather datasets from selected cohorts ---
      datasets_available <- shiny::reactive({
        shiny::req(input$cohort_data_explore)
        all_files <- purrr::map(input$cohort_data_explore, function(cohort) {
          cohort_dir <- system.file("extdata", cohort, package = "NEPScribe")
          files <- list.files(
            path = cohort_dir,
            pattern = ".*\\d{1,2}-\\d-\\d(_beta)?\\.dta$",
            full.names = TRUE
          )
          if (length(files) == 0) return(NULL)
          data.frame(
            cohort = cohort,
            file = basename(files),
            full_path = files,
            stringsAsFactors = FALSE
          )
        })
        all_files <- do.call(rbind, all_files)
        if (is.null(all_files)) return(NULL)
        all_files
      })

      # --- Update dataset picker whenever cohorts change ---
      shiny::observeEvent(input$cohort_data_explore, {
        df <- datasets_available()
        if (is.null(df)) {
          shiny::freezeReactiveValue(input, "dataset")
          shinyWidgets::updatePickerInput(session, "dataset", choices = character(0), selected = NULL)
        } else {
          shinyWidgets::updatePickerInput(
            session,
            "dataset",
            choices = df$file,
            selected = NULL
          )
        }
        # Clear meta selector and value boxes until dataset is selected
        shiny::freezeReactiveValue(input, "meta_selector")
        shinyWidgets::updatePickerInput(session, "meta_selector", choices = character(0), selected = NULL)
        output$dataset <- shiny::renderText({ "" })
        output$vars <- shiny::renderText({ "" })
      })

      # --- Available metadata ---
      available_meta <- shiny::reactive({
        shiny::req(input$dataset)

        df <- datasets_available()
        selected_files <- df$full_path[df$file %in% input$dataset]
        if (length(selected_files) == 0) return(character(0))

        # Collect meta from ALL selected datasets
        meta_list <- purrr::map(selected_files, function(fp) {
          process_meta_for_input_list(
            dirname(fp),
            basename(fp)
          )
        })

        # Union of all metadata
        meta <- meta_list |>
          unlist(use.names = FALSE) |>
          unique()

        meta <- remove_prefix_suffix_capitalize_vec(meta)

        # Optional ordering
        if ("Varlabel" %in% meta) meta <- move_string_to_position(meta, "Varlabel", 1)
        if ("Questiontext" %in% meta) meta <- move_string_to_position(meta, "Questiontext", 2)

        meta
      })


      # --- Update meta selector ---
      shiny::observeEvent(input$dataset, {
        meta <- available_meta()
        shiny::freezeReactiveValue(input, "meta_selector")
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "meta_selector",
          choices = meta,
          selected = intersect(c("Varlabel", "Questiontext"), meta)
        )
      })

      # --- Reactive: data overview ---
      data_overview_r <- shiny::reactive({
        shiny::req(input$dataset)
        df <- datasets_available()
        selected_files <- df$full_path[df$file %in% input$dataset]
        if (length(selected_files) == 0) return(NULL)
        language <- ifelse(input$language, "en", "de")
        dataframes <- purrr::map(selected_files, ~ gen_data_overview(dirname(.x), basename(.x), language = language))
        do.call(dplyr::bind_rows, dataframes)
      })

      # --- Value boxes ---

      # first box: Datasets
        output$dataset_summary <- shiny::renderUI({
          shiny::req(input$dataset)

          datasets <- input$dataset
          n <- length(datasets)

          if (n == 0) {
            return(htmltools::tags$span("-"))
          }

          # Limit to 5 items
          shown <- utils::head(datasets, 5)

          htmltools::tags$ul(
            style = "
        margin: 0;
        padding-left: 1em;
        font-size: 0.85rem;
        line-height: 1.3;
      ",
            lapply(shown, htmltools::tags$li),
            if (n > 5) htmltools::tags$li("...")
          )
        })

        # second box: Variables count
        output$vars_summary <- shiny::renderText({
          df <- data_overview_r()
          if (is.null(df) || nrow(df) == 0) return("-")
          nrow(df)
        })



      # --- Render DataTable ---
      output$data_overview <- DT::renderDataTable({
        data <- data_overview_r()
        req(data)
        meta_selection <- input$meta_selector
        if (!input$language) meta_selection <- add_suffix(meta_selection, "de")
        else meta_selection <- add_suffix(meta_selection, "en")
        if (length(meta_selection) > 0) {
          data <- data |> dplyr::select(Dataset, Variable, dplyr::starts_with("NEPS_varlabel"), dplyr::any_of(meta_selection))
        } else {
          data <- data |> dplyr::select(Dataset, Variable, dplyr::starts_with("NEPS_varlabel"))
        }

        # Leading marker column: a small icon on rows that can be sent to Transform Data's
        # Additional Variables (mergeable dataset of the cohort currently selected there)
        marker_icon <- as.character(shiny::icon(
          "circle-plus",
          class = "add-marker-icon",
          `data-toggle` = "tooltip",
          title = "Can be added to the script"
        ))
        marker <- base::ifelse(data$Dataset %in% cross_module$available_datasets, marker_icon, "")
        data <- dplyr::mutate(data, ` ` = marker, .before = 1)

        # Export buttons leave out the marker column; plus an "Add Selected to Script" button
        # in the same toolbar, which triggers input$add_to_script
        buttons <- c(
          base::lapply(.buttons, function(b) c(b, list(exportOptions = list(columns = ":not(.dt-marker)")))),
          list(list(
            # DT requires a built-in button type to extend; the custom action replaces copy's
            extend = "copy",
            text = "<i class='fas fa-circle-plus add-marker-icon'></i> Add Selected to Script",
            action = htmlwidgets::JS(base::sprintf(
              "function() { Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); }",
              session$ns("add_to_script")
            ))
          ))
        )

        DT::datatable(
          data,
          extensions = "Buttons",
          selection = list(mode = "multiple", target = 'row'),
          escape = base::setdiff(base::names(data), " "),
          options = list(
            rowCallback = htmlwidgets::JS("customRowCallback"),
            # table pages are fetched separately from Shiny's own update cycle, so the marker
            # tooltips need initializing after every redraw, not just on shiny:idle
            drawCallback = htmlwidgets::JS("function() { if (window.initAppTooltips) window.initAppTooltips(); }"),
            # a single grey + between the buttons and the table, explaining the marker on hover
            initComplete = htmlwidgets::JS(
              "function() {",
              "  $(this.api().table().container()).find('div.dt-add-hint').empty().append(",
              "    $('<i>', {'class': 'fas fa-circle-plus add-marker-icon', 'data-toggle': 'tooltip',",
              "      title: 'Marks variables you can add from here directly to the person-year script in Transform Data. ' +",
              "        'However, the selected starting cohorts in both tabs must match. ' +",
              "        'Click rows to select them and confirm with the \"Add Selected to Script\" button.'})",
              "  );",
              "  if (window.initAppTooltips) window.initAppTooltips();",
              "}"
            ),
            columnDefs = list(list(targets = 1, className = "dt-marker", orderable = FALSE)),
            pageLength = 50,
            dom = 'lfB<"dt-add-hint">rtip',
            buttons = buttons,
            searchHighlight = TRUE
          )
        )
      })

      # --- Send selected rows to Transform Data's Additional Variables ---
      shiny::observeEvent(input$add_to_script, {
        rows <- input$data_overview_rows_selected
        data <- data_overview_r()

        alert <- function(text, type) {
          shinyalert::shinyalert(
            title = "",
            text = text,
            html = TRUE,
            size = "s",
            type = type,
            closeOnClickOutside = TRUE,
            confirmButtonCol = "#AEDEF4"
          )
        }

        if (base::length(rows) == 0 || base::is.null(data)) {
          alert("Select one or more variables in the table first by clicking on their rows.", "info")
          return()
        }

        selected <- data[rows, c("Dataset", "Variable")]
        trans_cohort <- if (base::is.null(cross_module$cohort)) "" else cross_module$cohort
        row_cohort <- stringr::str_extract(selected$Dataset, "^SC\\d+")
        wrong_cohort <- base::is.na(row_cohort) | row_cohort != trans_cohort
        addable <- !wrong_cohort & selected$Dataset %in% cross_module$available_datasets
        not_mergeable <- !wrong_cohort & !addable

        to_add <- selected[addable, ]
        if (base::nrow(to_add) > 0) {
          cross_module$add_request <- list(
            vars = base::split(to_add$Variable, to_add$Dataset),
            nonce = stats::runif(1)
          )
          DT::selectRows(DT::dataTableProxy("data_overview"), NULL)
        }

        esc <- function(x) htmltools::htmlEscape(base::paste(base::unique(x), collapse = ", "))
        msg <- character(0)
        if (base::any(addable)) {
          msg <- c(msg, base::sprintf(
            "Added %d variable(s) from %s to the script. You can review them under Transform Data &rarr; Additional Variables.",
            base::sum(addable), esc(to_add$Dataset)
          ))
        }
        if (base::any(wrong_cohort)) {
          msg <- c(msg, base::sprintf(
            "Skipped %d variable(s) from a different starting cohort (%s): Transform Data is currently set to %s. Variables can only be added from the starting cohort selected there (Starting Cohorts 3 to 6 are supported).",
            base::sum(wrong_cohort), esc(row_cohort[wrong_cohort]), htmltools::htmlEscape(trans_cohort)
          ))
        }
        if (base::any(not_mergeable)) {
          msg <- c(msg, base::sprintf(
            "Skipped %d variable(s) from datasets that can't be merged directly into a person-year dataset (%s). For some of these, exemplary data preparation code is available in the Transform Data sidebar.",
            base::sum(not_mergeable), esc(selected$Dataset[not_mergeable])
          ))
        }

        type <- if (!base::any(addable)) "warning" else if (base::all(addable)) "success" else "info"
        alert(base::paste(msg, collapse = "<br><br>"), type)
      })

    }
  )
}
