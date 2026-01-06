

# select dataset UI module
dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("dataset"),
      label = tags$b("Dataset"),
      choices = NULL,
      multiple = TRUE,
      options = list(maxItems = 1)
    ),
    # Value boxes for key metrics
    hidden(value_box(
      title="Dataset",
      value = textOutput(ns("dataset")),
      theme = "primary",
      id = "value_box_short1"
    )),
    hidden(value_box(
      title="Observations",
      value = textOutput(ns("obs")),
      theme = "info",
      id = "value_box_short2"
    )),
    hidden(value_box(
      title="Distinct ID_ts",
      value = textOutput(ns("distinct_ID_ts")),
      theme = "primary",
      id = "value_box_short3"
    )),
    hidden(value_box(
      title="Variables",
      value = textOutput(ns("vars")),
      theme = "info",
      id = "value_box_short4"
    )),
    value_box(
      title="!! Warning !!",
      value = p("If you load huge files like pTarget.dta or all datasets at once, you might want to use that time to brew some coffee.",style = "font-size: 100%;") ,
      theme = "secondary",
      id = "value_box_short5"
    )
  )
}

# dataset overview table UI module
dataset_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      shinyWidgets::dropdown(
        inputId = "dropdown_ov",
        label = "Settings",
        icon = icon("gear"),
        width = "300px",
        tooltip = tooltipOptions(title = "Click to see Settings"),
        # this is initially hidden until the user selects a dataset. Otherwise the user might click on the input to get all datasets but nothings happens caus it is currently coded that the output table only appears if a dataset is selected. There are alternatives to that but for now, this is ok
        pickerInput(
          inputId = ns("meta_selector"),
          label = "Meta Selection",
          choices = NULL,
          selected = NULL,
          multiple = T,
          options = list('actions-box' = TRUE)
        ),
        materialSwitch(
          inputId = ns("ms_all_datasets"),
          label = "Load all datasets",
          value = F,
          status = "info"
        )
      )
    ),
    fluidRow(withSpinner(DT::DTOutput(ns("data_overview")), caption = .captiontext)),
  )
}


# Server
dataset_explorer_server <- function(id, reactiveDatapath, settings_reactive) {

  # Calling the moduleServer function
  moduleServer(
    id,
    function(input, output, session) {

      filenames <- reactive({
        req(reactiveDatapath()$datapath)
        filenames <- list.files(reactiveDatapath()$datapath, pattern=pattern <- ".*\\d{1,2}-\\d-\\d(_beta)?\\.dta$", full.names=TRUE)  #list all dta files in provided datapath
        substr(filenames, stringr::str_length(reactiveDatapath()$datapath)+1, 500) # extract only the filenames and cut off datapath before
      })

      # update dataset input fields with user provided filenames from suf directory
      observeEvent(reactiveDatapath()$datapath, {
        updateSelectInput(
          session = getDefaultReactiveDomain(),
          "dataset",
          choices = filenames(),
          selected = NULL
        )
      })

      # when a datapath is provided, reset the materialswitch that loads all vars from all datasets to FALSE
      observeEvent(reactiveDatapath()$datapath, {
        updateMaterialSwitch(session = session, "ms_all_datasets", value = F)
        updateMaterialSwitch(session = session, "ms_all_vars", value = F)
      })


      # show "subspell 0" materialswitch when there is a subspell variable in selected dataset
      observeEvent(input$dataset, {
        shinyjs::show(id = "ms_all_datasets")
        shinyjs::show(id = "meta_selector")
        shinyjs::show(id = "sw-drop-dropdown_ov", asis =T)
        shinyjs::show(id = "sw-drop-dropdown_br", asis =T)
        shinyjs::show(id = "var_selector")
        if(input$ms_all_datasets==F){
          shinyjs::show(selector = "[id^='value_box_short']") # show all objects with ids that start with that
        }
        updateMaterialSwitch(session = session, "ms_all_datasets", value = F)
      }
      )

      observeEvent(input$ms_all_datasets == T, {
        shinyjs::hide(selector = "[id^='value_box_short']")
      })

      observeEvent(input$ms_all_datasets == F, {
        shinyjs::show(selector = "[id^='value_box_short']")
      })

      observeEvent(list(input$dataset, settings_reactive()$language), { # if one of these reactives changes, update picker input
        updatePickerInput(
          session = getDefaultReactiveDomain(),
          "var_selector",
          choices = names(data_browse()),
          selected = names(data_browse()),
          options = list('actions-box' = TRUE,
                         'liveSearch' = TRUE)
        )
      })

      # when a dataset is selected or all datasets ms is selected, update meta selector
      observeEvent(list(input$dataset, input$ms_all_datasets), {
        updatePickerInput(
          session = getDefaultReactiveDomain(),
          "meta_selector",
          choices = available_meta(),
          selected = c("Varlabel", "Questiontext")
        )

        # text for the info boxes for specific dataset
        if (input$ms_all_datasets == F) {
          output$dataset <- renderText({
            generate_info(reactiveDatapath()$datapath, input$dataset)[[1]]
          })
          output$obs <- renderText({
            generate_info(reactiveDatapath()$datapath, input$dataset)[[2]]
          })
          output$distinct_ID_ts <- renderText({
            generate_info(reactiveDatapath()$datapath, input$dataset)[[3]]
          })
          output$vars <- renderText({
            generate_info(reactiveDatapath()$datapath, input$dataset)[[4]]
          })
        }
        # text for the info boxes for all datasets
        else {
          output$dataset <- renderText({
            "All Datasets"
          })
          output$obs <- renderText({
            "-"
          })
          output$distinct_ID_ts <- renderText({
            "-"
          })
          output$vars <- renderText({
            nrow(data_overview_r())
          })
        }
      })

      # reactive to update the meta selector - it calls the function process_meta which generates a vector of all the attracted data in the "expansionfield" of the variable
      available_meta <- reactive({
        req(input$dataset)
        if(input$ms_all_datasets ==F){
          meta <- process_meta_for_input_list(reactiveDatapath()$datapath, input$dataset)
        }
        else{
          list_of_metas <- map(filenames(),~ process_meta_for_input_list(reactiveDatapath()$datapath, .x))
          list_of_metas_vec <- do.call(c,list_of_metas)
          meta <- unique(list_of_metas_vec)
        }
        # move varlabel and questiontext (if they exist) to early positions so that theyre fixed there.
        meta <- remove_prefix_suffix_capitalize_vec(meta)
        if("Varlabel" %in% meta)
          meta <- move_string_to_position(meta, "Varlabel",1)
        if("Questiontext" %in% meta)
          meta <- move_string_to_position(meta, "Questiontext",2)
        return(meta)
      })

      # reactive for the data variable table on dataset exploration. It either generates the table with all vars from selected dataset or from all datasets if the input materialswitch is TRUE
      data_overview_r <- reactive({
        req(input$dataset)
        # first determine language dependent on the materialswitch input "language"
        if (settings_reactive()$language ==F) { language = "de" }
        else {                    language = "en" }

        # then determine if only the selected or all datasets should be loaded dependent on the materialswitch input "ms_all_datasets"
        if(input$ms_all_datasets==F){
          gen_data_overview(reactiveDatapath()$datapath, input$dataset, language =language)
        }
        else {
          dataframes <- map(filenames(),~ gen_data_overview(reactiveDatapath()$datapath, .x, language =language)) # show all variables in all datasets by creating a list of dataframes that are the variable tables from all datasets - this will crash if there are datasets in the provided datapath that contain datasets witout the neps expansionfields - we should catch this
          do.call(bind_rows,dataframes) # this appends all these dfs together and makes a huge table
        }
      })

      # this reactive combines two inputs. This is used then below to trigger the updating of the input meta_selector whenever dataset input OR the materialswitch "ms_all_datasets" changes
      data_browse <- reactive({
        req(input$dataset)
        gen_data_browse(reactiveDatapath()$datapath, input$dataset, input$ms_all_vars, settings_reactive()$language) # generate the data for the table
      })

      # Render the overview data table with meta infos on variables
      output$data_overview <- DT::renderDataTable({
        req(input$dataset)
        data <- data_overview_r()
        meta_selection <- input$meta_selector
        if(settings_reactive()$language ==F){
          meta_selection <- add_suffix(meta_selection, "de")
        }
        else {
          meta_selection <- add_suffix(meta_selection, "en")
        }

        # select variables in table according to selected meta fields
        if(length(meta_selection)>0){
          data <- data |>
            select(Dataset, Variable,  starts_with("NEPS_varlabel"), any_of(meta_selection))
        }
        else{
          data <- data |>
            select(Dataset, Variable,  starts_with("NEPS_varlabel"))
        }

        datatable(
          data,
          extensions =  "Buttons",
          selection = list(mode = "multiple",target = 'row'),
          options = list(
            rowCallback = JS("customRowCallback"),
            pageLength = 50,
            dom = 'lfBrtip', # Position of different UI elements in the table
            buttons = .buttons, # Use the custom buttons defined earlier
            searchHighlight = TRUE
          )
        )
      })



    }
  )}

