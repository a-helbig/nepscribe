#' UI func
#' @keywords internal
#' @noRd
dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    # Multi-select cohort picker
    shinyWidgets::pickerInput(
      inputId = ns("cohort_data_explore"),
      label = htmltools::tags$b("Select Starting Cohorts"),
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
      label = htmltools::tags$b("Dataset"),
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
      label = htmltools::tags$b("Meta Selection"),
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
dataset_explorer_server <- function(id, settings_reactive) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # --- Reactive: gather datasets from selected cohorts ---
      datasets_available <- shiny::reactive({
        req(input$cohort_data_explore)
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
        req(input$dataset)

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
          req(input$dataset)

          datasets <- input$dataset
          n <- length(datasets)

          if (n == 0) {
            return(tags$span("-"))
          }

          # Limit to 3 items
          shown <- head(datasets, 5)

          tags$ul(
            style = "
        margin: 0;
        padding-left: 1em;
        font-size: 0.85rem;
        line-height: 1.3;
      ",
            lapply(shown, tags$li),
            if (n > 5) tags$li("...")
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
