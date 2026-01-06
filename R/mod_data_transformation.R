# module data transformation
# todos here:
# catch possible errors that could occur if there are other data types in provided datapath. Also non-NEPS dta files could break code
# This is a div for the loading spinners
captiontext <- div(strong("Loading"), br(), "Please wait")

# UI
data_transformation_add_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(tags$b(div("1. Step: Select Dataset")), width = 2),
      column(tags$b(div("3. Step: Confirm selected variables")), width = 2),
    ),
    fluidRow(
      column(selectizeInput(ns("dataset"), label = NULL, choices = NULL, multiple = TRUE, options = list(maxItems = 1)), width = 2),
      column(
        actionButton(ns("confirm_variables"), "Confirm Vars", style = "width: 145px; height: 40px"), width = 2)
      ,
      tags$b(div("2. Step: Select Variables"))),
    fluidRow(
      multiInput(
        ns("multi_vars_input"), label = NULL,  choices = c(""), width = "50%",  options = list(
          enable_search = TRUE,
          non_selected_header = "Select Variables:",
          selected_header = "You have selected:"
        )
      ),
      column(
        tags$b(div("Inspect selected Variables or reset everything (Button)")),
        pickerInput(
          ns("global_vars"),
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          selected = NULL,
          options = list('actions-box' = TRUE)
        ),
        actionButton(ns("reset_variables"), "Reset Vars", style = "width: 145px; height: 40px"), width = 2
      ),
      # div(""),
      # actionButton(ns("add_vars_from_explore_data"), "Add vars from Explore Datasets tab", style = "width: 145px; height: 40px")
    )
  )
}

data_transformation_prio_ui <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("To construct a person-year dataset where each row corresponds to one wave for each individual, a spell prioritization process is useful for identifying the principal spell in cases where multiple spells occur simultaneously. The following hierarchy of spell types dictates which episodes take precedence in this process, with the items at the top representing the highest priority and those at the bottom indicating the lowest priority."),
    rank_list(
      input_id = ns("prio_swap_list"),
      text = "Swap Items to change priorisation order.",
      labels = labels,
      options = sortable_options(swap = F)
    ))
}

data_transformation_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(title = "Harmonized Format: The data preparation of life-course trajectories is based on the edited and cleaned biography file. Subspell Format: The data preparation of life-course trajectories is based on the originally recorded subspell episodes.",
             selectizeInput(
               ns("sub_format_select"),
               tags$b("Person-Year-Data: Format"),
               choices = c("Harmonized Spell Format (Recommended)", "Original Subspell Format"),
               multiple = TRUE,
               selected = "Harmonized Spell Format (Recommended)",
               options = list(maxItems = 1)
             )),
    tags$div(title = "Currently supported script formats: R or STATA.",
             radioButtons(ns("stata_or_r"), tags$b("Script file format"), c("STATA", "R"), selected = "R")),
    # puts all quickfilters in one column with consistent alignment
    checkboxGroupInput(ns("settings"), tags$b("Settings"), choices = c("Set Missing Values","Script: English Labels", "Include Parallel Spells")),
    p(""),
    checkboxGroupInput(ns("add_modules"), tags$b("Add exemplary data preparation"), choices = c("Further Training","Partner","Children", "Highest Education")),
    p(""),
    tags$div(title = "Show a preview of the script with the actual settings.",
             actionButton(ns("previewScript"), "Preview Script", icon("play-circle"))),
    tags$div(title = "Download the script with the actual settings.",
             downloadButton(ns("downloadScript"), "Download Script")),
    tags$div(title = "Generate the data according to the generated script",
             actionButton(ns("generateData"), "Generate Data", icon("database")))
  )
}


# Server

data_transformation_server <- function(id, reactiveDatapath,settings_reactive) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(reactiveDatapath()$datapath,{
        updateSelectInput(
          session,
          "dataset",
          label = NULL,
          selected = NULL,
          choices = datasets()
        )
      })

      observeEvent(input$dataset,{
        updateMultiInput(
          session,
          "multi_vars_input",
          label = NULL,
          selected = NULL,
          choices = gen_comb_char(reactiveDatapath()$datapath, input$dataset,settings_reactive()$language)
        )
      })

      # reactive for SUF Files
      # list all files in user provided datapath and then extract only the filename (without directory path) in order to get a list for the user to choose from
      filenames <- reactive({
        req(reactiveDatapath()$datapath)
        # list all dta files in provided datapath
        filenames <- list.files(reactiveDatapath()$datapath,
                                pattern = "*.dta",
                                full.names = TRUE)
        # extract only the filenames and cut off datapath before
        substr(filenames,
               stringr::str_length(reactiveDatapath()$datapath) + 1,
               500)
      })


      # reactive list with dataset names in datapath. will be used as options in selectinput.
      datasets <- reactive({
        req(reactiveDatapath()$datapath)
        create_dataset_names(reactiveDatapath()$datapath)
      })

      # Empty reactive value list that will be filled in the obseverEvent below with the selected vars from selected dataset and then passed to the gen_script function
      varlist <- reactiveValues(data = list())  # Initializes as a list

      # Reactive list to store all lists that are required for the updating of the picker input "global_vars"
      all_lists <- reactiveVal(list())


      # Observe the action button - when its clicked create the lists of variables, that will be merged in the person year dataset in the scripts. Another list is created for the global_vars picker input
      observeEvent(input$confirm_variables, {
        req(input$dataset,input$multi_vars_input)

        # shorten the label from the var vector
        vars_vec_short <- str_replace_all(input$multi_vars_input, " - .*","")

        # create a basic dataset with 2 cols: a vector of selected variables from multiinput and a column of equal length with dataset name
        dataframe <- create_dataframe(input$dataset, vars_vec_short)

        # add linkage keys from external csv file (this file contains the record linkage or merging variables for each NEPS suf file)
        dataframe <- left_join(dataframe,create_linkage_data(reactiveDatapath()$datapath), by= "Dataset" )

        # Append the new dataframe to the reactive list by subsetting the list and adding the df to the list under datasets name. This will then be passed to the gen_script function that generates the output scripts
        varlist$data[[input$dataset]] <- dataframe

        # The next few steps generate a list of lists to update the picker input "global_vars" with all selected vars form selected datasets
        # create a list of variable names from multiinput with dataset as lists name
        new_list <- gen_list_for_picker(input$dataset, input$multi_vars_input)

        # Update the list of all generated lists
        current_lists <- all_lists() # generates a list that is filled with the reactive List
        current_lists[[input$dataset]] <- new_list # assigns the new list from above to current_lists with the name from the dataset input
        all_lists(current_lists) # saves current_lists in the reactive list

        # update the picker input: pass list of lists to global vars input field
        updatePickerInput(
          session,
          "global_vars",
          choices = current_lists,
          selected = unique(unlist(current_lists))
        )

        # open an alert field that confirms that variables from dataset have been added
        shinyalert(
          title = "",
          text = "Dataset, selected variables and merge procedure added to script. You may continue with another dataset.",
          size = "xs",
          closeOnClickOutside = TRUE,
          type = "success",
          confirmButtonCol = "#AEDEF4",
        )
      })

      # reset inputs and lists if button is clicked
      observeEvent(input$reset_variables, {
        shinyjs::reset("dataset")
        updatePickerInput(
          session,
          "global_vars",
          choices = c(""),
          selected = NULL
        )
        updateMultiInput(
          session,
          "multi_vars_input",
          choices = c(""),
          selected = NULL
        )
        varlist$data <- NULL
        all_lists(list())
      })

      # show preview in modal when action btn is clicked
      observeEvent(input$previewScript,{


        #open modal when btn is clicked and show preview of script
        showModal(modalDialog(
          title = "Preview of script",
          size = "l",
          HTML(paste(gen_script(
            # call self written function that is basically a huge string vector and can be modified by the functions arguments
            datapath_conv = str_replace_all(reactiveDatapath()$datapath, "\\\\", "/"),
            suf_version = extract_suf_version(reactiveDatapath()$datapath),
            suf_version_short = extract_suf_version(reactiveDatapath()$datapath,short=T),
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            # here, all the selected and confirmed multiinput vars from one or multiple datasets will be filtered with the global_vars input list (with function: filter_dataframes). Only those that are selected there will be added to the script. Additionally duplicates are removed, because users are able to multiple confirm variables from 1 dataset and thus add it multiple times to varlist$data:
            datalist = filter_dataframes(varlist$data, str_replace_all(input$global_vars, " - .*","")),
            prio = input$prio_swap_list,
            english = "Script: English Labels" %in% input$settings,
            set_missings = "Set Missing Values" %in% input$settings,
            parallel = "Include Parallel Spells" %in% input$settings,
            further_training = "Further Training" %in% input$add_modules,
            education = "Highest Education" %in% input$add_modules,
            children = "Children" %in% input$add_modules
          ), collapse = "<br>")),  # Combine vector elements with line breaks
          easyClose = TRUE,
          footer = modalButton("Close")
        )
        )
      }
      )


      # show preview in modal when action btn is clicked
      observeEvent(input$generateData,{

        withProgress(message = 'Processing data, please wait until loading bar dissapears', value = 0, {
          showModal(modalDialog(
            title = "Note",
            size = "s",
            HTML("Please wait while data is being processed. <br><br> The resulting dataset is generated according to your settings and is identical to the dataset you would receive by downloading and executing the script. The dataset will be stored in the specified SUF-Datapath. <br><br> Subsequently, you can preview the variables of the person-year-dataset in the Explore Variables tab. <br><br> However, please note that additional data wrangling, weighting procedures and other tasks may be required for thorough analysis."),
            easyClose = TRUE,
            footer = modalButton("Close")))
          gen_data( # call function that generates a person year dataset according to the settings
            datapath = str_replace_all(reactiveDatapath()$datapath, "\\\\", "/"),
            suf_version = extract_suf_version(reactiveDatapath()$datapath),
            suf_version_short = extract_suf_version(reactiveDatapath()$datapath,short=T),
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            # here, all the selected and confirmed multiinput vars from one or multiple datasets will be filtered with the global_vars input list (with function: filter_dataframes). Only those that are selected there will be added to the script. Additionally duplicates are removed, because users are able to multiple confirm variables from 1 dataset and thus add it multiple times to varlist$data:
            datalist = filter_dataframes(varlist$data, str_replace_all(input$global_vars, " - .*","")),
            prio = input$prio_swap_list,
            english = "Script: English Labels" %in% input$settings,
            set_missings = "Set Missing Values" %in% input$settings,
            parallel = "Include Parallel Spells" %in% input$settings,
            further_training = "Further Training" %in% input$add_modules,
            education = "Highest Education" %in% input$add_modules,
            children = "Children" %in% input$add_modules
          )})
      })


      # download button that ll download a script according to user selection criteria
      output$downloadScript <- downloadHandler(
        filename = function() {
          # function for naming the download file
          if (input$stata_or_r == "R") {
            paste("data_wrangling_script", ".R", sep = "")
          }
          else{
            paste("data_wrangling_script", ".do", sep = "")
          }
        },
        content = function(file) {

          # function for creating the sc specific script
          script_harm <- gen_script(
            # call self written function that is basically a huge string vector and can be modified by the functions arguments and save it as script_harm vector
            datapath_conv = str_replace_all(reactiveDatapath()$datapath, "\\\\", "/"),
            suf_version = extract_suf_version(reactiveDatapath()$datapath),
            suf_version_short = extract_suf_version(reactiveDatapath()$datapath,short=T),
            dataformat = input$stata_or_r,
            subformat = input$sub_format_select,
            # here, all the selected and confirmed multiinput vars from one or multiple datasets will be filtered with the global_vars input list, only those that are selected there will be added to the script. Additionally duplicates are removed, because users are able to multiple confirm variables from 1 dataset and thus add it multiple times to varlist$data:
            datalist = filter_dataframes(varlist$data, str_replace_all(input$global_vars, " - .*","")),
            prio = input$prio_swap_list,
            english = "Script: English Labels" %in% input$settings,
            set_missings = "Set Missing Values" %in% input$settings,
            parallel = "Include Parallel Spells" %in% input$settings,
            further_training = "Further Training" %in% input$add_modules,
            education = "Highest Education" %in% input$add_modules,
            children = "Children" %in% input$add_modules
          )
          writeLines(script_harm, file) # we then write this to file
        },
        contentType = "text/plain"
      )

    }
  )}

