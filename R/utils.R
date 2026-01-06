# Main Functions ---------------------------------------------------------

# function that returns string about starting cohort in form: 'sc3':'sc6'
identify_sc <- function(datapath) {
  # List files in the directory
  files <- list.files(datapath)

  # Extract strings starting with "SC" followed by a digit at the beginning of filenames
  sc_string <- unique(stringr::str_extract(files, "^SC\\d"))

  # Remove NAs (files that don't match)
  sc_string <- sc_string[!is.na(sc_string)]

  # Check if no matching SC found
  if (length(sc_string) == 0) {
    stop("No SC cohort identifier found in the dataset directory. Please provide datasets starting with 'SC' followed by a digit.")
  }

  # Check if more than one unique SC found — throw error
  if (length(sc_string) > 1) {
    stop(
      paste0(
        "Multiple SC cohorts detected in provided datapath: ",
        paste(sc_string, collapse = ", "),
        ". Please dont mix NEPS datasets from different starting cohorts in provided datapath"
      )
    )
  }
  return(tolower(sc_string))
}

getWindowsDrives <- function() {
  drives <- sprintf("%s:/", LETTERS)
  drives <- drives[file.exists(drives)]
  names(drives) <- gsub(":/", "", drives)  # C, D, E
  drives
}

# function that filters a dataset depending on variable type
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  }
  else if (is.factor(x) | is.character(x) | inherits(x, "labelled")) {
    x %in% val
  }
  else {
    # No control, so don't filter
    TRUE
  }
}

# function to check and transform variables to a factor if they are not of type factor or labelled doubles
transform_all_to_factors <- function(data) {
  # Loop through each variable in the dataframe
  for (variable in names(data)) {
    # Check if the variable is not already a factor or a labelled double
    if (!is.factor(data[[variable]]) && !inherits(data[[variable]], "labelled")) {
      # Transform the variable to a factor
    }
  }
  return(data)  # Return the modified dataframe
}

# helper function for "filter_data" function
cond_filter <- function(data, vars) {
  for (var in vars) {
    if (var %in% names(data)) {
      # data <- data[data[[var]] == 1, ]
      data <- data |>
        filter(data[[var]] == 1)
    }
  }
  return(data)
}

# function to truncate strings to 15 chars and return as unnamed char vec
truncate_strings <- function(input_strings) {
  # Check the length of each string and apply truncation if necessary
  truncated <- ifelse(nchar(input_strings) > 15,
                      paste0(substr(input_strings, 1, 15), "..."),
                      input_strings)

  # Return the result as an unnamed character vector
  return(unname(truncated))
}

# faster ifelse version:
fast_ifelse <- function(test, yes, no) {
  stopifnot(identical(class(yes), class(no)))

  out <- rep(NA, length(test))
  out[test] <- yes
  out[!test] <- no
  class(out) <- class(yes)

  out
}

# function to create a list of valid dataset names for the dataset input field.
create_dataset_names <- function(datapath){
  linkage_keys <- read.csv(file = "SC6_linkage_keys.csv", sep=";")
  datasetnames <- linkage_keys |>
    filter(Linkage != "") |>
    select(-Info, -Linkage) |>
    pull()
  # we need to add to each string element the suf-version in datapath (it takes latest when there are multiple suf version in directory):

  # so we detect latest suf version in reactive datapath
  latest_suf_version <- extract_suf_version(datapath)
  # next, we cut of ".dta" from dataset names, so we can easily paste suf_version and then .dta back to the names
  datasetnames <- str_replace(datasetnames, ".dta","")
  datasetnames <- paste0(datasetnames, latest_suf_version, ".dta")
  return(datasetnames)
}


# extracts the latest suf version from the datafiles name of the suf data in given datapath
extract_suf_version <- function(datapath, short =F){

  # extract the suf version part of each filename and reduce it to unique string
  distinct_strings <- unique(str_extract(list.files(datapath,pattern="\\d{1,2}-\\d-\\d"), "\\d{1,2}-\\d-\\d"))
  # extract suf version and turn to numeric
  num_strings <- as.numeric(str_extract(distinct_strings,"\\d{1,2}"))
  last_part_of_string <- str_extract(distinct_strings,"-\\d-\\d")[1]
  num_strings <- sort(num_strings, decreasing =T)
  num_string <- num_strings[1]

  if(length(distinct_strings)==0){
    warning("Not possible to determine suf version - check provided directory and restart the app")
  }
  else if(length(distinct_strings) == 1){
    if(short==T){
      return(num_strings)
    }
    else {
      return(paste0(as.character(num_strings),last_part_of_string))
    }
  }
  else if(length(distinct_strings) > 1){ # if there are multiple suf version, determine the latest ones and use this
    # ensure taking the latest suf version in case there are multiple unique values
    if(short==T){
      return(num_string)
    }
    else {
      return(paste0(as.character(num_string),last_part_of_string))
    }
  }
}

#function to read dta and assign variable labels (which are attracted but not yet assigned) to the variables
readstata13_label <- function(x){
  data <- readstata13::read.dta13(x)
  #extracts the column "names" as vector
  data_meta_names = as.vector(attr(data,"names"))
  #extracts the column "labels"as vector
  data_meta_labels = as.vector(attr(data,"var.labels"))
  #assigns the names to the labels
  names(data_meta_labels) = data_meta_names
  #assigns the labels to the variables
  for(i in seq_along(data)){
    Hmisc::label(data[,i]) = data_meta_labels[i]
  }
  data
}

gen_questiontext <- function(datapath,dataset, col_select, variable, language){
  #check if its an original suf dataset, if yes, do str_trunc and language switch depending on language argument and add questiontext. if not, dont add question text because trying to read this expansion field in a non standard neps dataset would break code
  if(str_detect(dataset, "SC.*\\d\\d-\\d-\\d\\.dta$")){
    # if language is not english
    if(!language){
      # data <- str_trunc(
      data <-
        read_exp_fields(
          paste0(datapath,"/", dataset),
          cols = col_select,
          attr_type = "NEPS_questiontext_de",
          only_value = T
          # ),
        )
      # 100
      # )
    }
    else{
      # data <- str_trunc(
      data <-
        read_exp_fields(
          paste0(datapath,"/", dataset),
          cols = col_select,
          attr_type = "NEPS_questiontext_en",
          only_value = T
          # ),
        )
      # 100
      # )
    }
    return(paste0("<b>Selected Variable: ",
                  variable, # variable name and label
                  ". Questiontext: <i>",
                  if(length(data)==0) "Not available"  else data, # Questiontext if available
                  "</i></b>"))
  }
  else {
    return(paste0("<b>Selected Variable: ",
                  variable))
  }
}


# function to bind vectors of different lenghts and fill missings up with NA
bind_cols_fill <- function(df_list) {
  max_rows <- map_int(df_list, nrow) |> max()

  map(df_list, function(df) {
    if (nrow(df) == max_rows)
      return(df)
    first <- names(df)[1] |> sym()
    df |>  add_row(!!first := rep(NA, max_rows - nrow(df)))
  })  |>  bind_cols()
}

# Helper function to capitalize the first letter of a string
capitalize_first_letter <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

# helper function to remove neps prefix from variable names in df in overview table (explore datasets) and make first letter capital
remove_prefix_suffix_capitalize_df <- function(df, colnames = T) {
  # Ensure the input is a df
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  if(colnames){
    # Remove "NEPS_" and "_de, _en" from column names and capitalize the first letter
    colnames(df) <- gsub("^NEPS_", "", colnames(df))
    colnames(df) <- gsub("(_de|_en)$", "", colnames(df))
    colnames(df) <- sapply(colnames(df), capitalize_first_letter)
  }

  else{
    # Remove "NEPS_" and "_de, _en" from column names
    df$type <- gsub("^NEPS_", "", df$type)
    df$type <- gsub("(_de|_en)$", "", df$type)
    df$type <- capitalize_first_letter(df$type)
  }

  return(df)
}

# helper function to remove neps prefix from variable names in vector in overview table (explore datasets) and make first letter capital
remove_prefix_suffix_capitalize_vec <- function(var_names) {
  # Ensure the input is a character vector
  if (!is.character(var_names)) {
    stop("Input must be a character vector")
  }

  # Function to capitalize the first letter of a string
  capitalize_first_letter <- function(x) {
    # Ensure first letter is capitalized and the rest remains unchanged
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }

  # Remove suffixes "_de", "_en", and "_test" from variable names
  var_names <- gsub("(_de|_en)$", "", var_names)
  var_names <- gsub("^NEPS_", "", var_names)
  var_names <- sapply(var_names, capitalize_first_letter, USE.NAMES = FALSE)

  return(var_names)
}

process_meta_for_input_list <- function(datapath,dataset){
  read_exp_fields(paste0(datapath,dataset)) |>
    filter(variable != "_dta" & !str_detect(type, "^NEPSMISS") & !str_detect(type, "shortvarlab") & !str_detect(type, "^_lang") & !(type %in% c("NEPS_varname","NEPS_procedures_de","NEPS_procedures_en", "NEPS_relations_de", "NEPS_relations_en", "NEPS_limitations_de","NEPS_limitations_en" ))) |> # delete dataset-specific meta data, we only want variable meta data here
    distinct(type, .keep_all = F) |>
    pull() |>
    str_remove_all("_de$|_en$") |>
    unique()
}

read_exp_fields <- function(datapath, cols=NULL, attr_type=NULL, only_value=F) {
  # Read the data with only the "ID_t" column and the first row for performance. We use the slower read.dta13 function here because it reads all attracted meta data while havens read_dta func does not
  data <- readstata13::read.dta13(datapath, select.cols = "ID_t", select.rows = 1)
  # Extract the expansion fields attribute
  exp_fields <- attr(data, "expansion.fields")
  # check if there were expansion.fields, if not throw an informative error message
  if(is.null(exp_fields) | length(exp_fields)==0){
    stop("Cant find attracted meta data in selected dataset. Please ensure you select only NEPS datasets here")
  }
  # Extract variable, type, and value from the expansion fields and unite these vectors into a df
  fields_df <- data.frame(
    variable = sapply(exp_fields, `[[`, 1),
    type = sapply(exp_fields, `[[`, 2),
    value = sapply(exp_fields, `[[`, 3),
    stringsAsFactors = FALSE
  )
  # access the required information from the "value" column for the selected variable and selected type of information
  if (!is.null(attr_type) & !is.null(cols)) {
    fields_df <- fields_df |>
      filter(variable %in% cols & type == attr_type)
  }
  if (is.null(attr_type) & !is.null(cols)) {
    fields_df <- fields_df |>
      filter(variable %in% cols)
  }
  if (!is.null(attr_type) & is.null(cols)) {
    fields_df <- fields_df |>
      filter(type == attr_type)
  }
  if(only_value)
    fields_df <- fields_df |>
    pull(value)

  return(fields_df)
}

# helper function to switch labels between english and german for specfic variables. Function returns all variables that were supploed in strings argument.
add_suffix <- function(strings, language = "de") {
  # The vector of specific strings to check against
  specific_strings <- c("_lang_v", "_lang_l", "NEPS_varlabel",
                        "NEPS_interviewerinstruction", "NEPS_questiontext",
                        "NEPS_outputfilter", "NEPS_autofillinstruction", "NEPS_valuefilter","NEPS_variablequestion","NEPS_inputfilter","NEPS_shortvarlab","NEPS_itemsinstruction","NEPS_varlabel","NEPS_unit")

  # Use ifelse to modify strings based on match
  modified_strings <- ifelse(strings %in% specific_strings,
                             paste0(strings, paste0("_",language)),
                             strings)
  return(modified_strings)
}

# helper function to change order of strings in vec
move_string_to_position <- function(vec, string_to_move, target_position) {
  # Ensure the input is a character vector
  if (!is.character(vec)) {
    stop("Input must be a character vector")
  }

  # Check if the string to move exists in the vector
  if (!(string_to_move %in% vec)) {
    stop(paste("String", string_to_move, "does not exist in the vector"))
  }

  # Remove the string to move from its current position
  vec <- vec[vec != string_to_move]

  # Check if the target position is valid
  if (target_position < 1 || target_position > length(vec) + 1) {
    stop("Target position is out of bounds")
  }

  # Insert the string at the specified target position
  vec <- append(vec, string_to_move, after = target_position - 1)

  return(vec)
}

# helper function to change order of vars in dataframe
move_variable_to_position <- function(df, var_to_move, after_var) {
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  # Check if specified variables exist in dataframe
  if (!(var_to_move %in% colnames(df))) {
    stop(paste("Variable", var_to_move, "does not exist in the dataframe"))
  }

  if (!(after_var %in% colnames(df))) {
    stop(paste("Variable", after_var, "does not exist in the dataframe"))
  }

  # Get the current order of columns
  columns <- colnames(df)

  # Find the index positions
  after_index <- which(columns == after_var)

  # Ensure that the 'after' position is valid
  if (after_index == 0) {
    stop("After variable not found")
  }

  # Remove the variable to move from its current position
  columns <- columns[-which(columns == var_to_move)]

  # Insert it right after the specified variable
  new_order <- c(
    columns[1:after_index],
    var_to_move,
    columns[(after_index + 1):length(columns)]
  )

  # Rearrange the dataframe
  df <- df[new_order]

  return(df)
}

gen_comb_char <- function(datapath, dataset, language){
  # load data
  data <- haven::read_dta(paste0(datapath,"/", dataset), n_max = 0)
  # if language switch to english: read english var labels and assign them
  if(language &  str_detect(dataset, "SC.*\\d\\d-\\d-\\d\\.dta$")){ # in case of english ms and original neps dataset
    varlabels_en <- read_exp_fields(paste0(datapath,"/",  dataset), attr_type="NEPS_varlabel_en") |> select(-type) # load english var labels
    data <- assign_var_labels(data, varlabels_en) # assign english var labels
  }
  # paste variable names and variable labels
  paste(names(data), "-", unname(sapply(data, attr, which = "label")))
}


# generates table with meta info
gen_data_overview <- function(datapath, dataset, language){
  #set datapath with dataset name
  datasetpath <- paste0(datapath,dataset)
  # read data
  data <- read_exp_fields(datasetpath)
  # pivot to wide format to obtain dataframe format for the datatable output
  data <- tidyr::pivot_wider(data, names_from = type, values_from = value)

  # Create a new variable called "Dataset"
  data$Dataset <- dataset

  # Reorder the columns to make "Dataset" the first variable
  data <- data.frame(Dataset = data$Dataset, data[, names(data) != "Dataset", drop = FALSE])

  # remove this "_dta" variable and rename variable col
  data <- data |>
    filter(variable != "_dta") |>
    rename(Variable = variable)

  # remove variables depending on the language argument
  if(language == "de"){
    # Create a logical vector identifying columns that end with "_en"
    columns_to_remove <- grepl("_en$", colnames(data))
    # Return the data frame with columns removed
    data <- data[, !columns_to_remove]
  }
  else{
    # Create a logical vector identifying columns that end with "_de"
    columns_to_remove <- grepl("_de$", colnames(data))
    # Return the data frame with columns removed
    data <- data[, !columns_to_remove]
  }

  # remove pre and suffixes and capitalize first letter
  data <- remove_prefix_suffix_capitalize_df(data)

  # move varlabel and questiontext (if they exist) to early positions so that theyre fixed there.
  if("Varlabel" %in% colnames(data))
    data <- move_variable_to_position(data, "Varlabel", "Variable")
  if("Questiontext" %in% colnames(data))
    data <- move_variable_to_position(data, "Questiontext","Varlabel")
  return(data)
}


gen_meta_var_table <- function(datapath,dataset,sel_var,language, value_labels){
  # read data
  if(str_detect(dataset, "\\d{2}-\\d-\\d")){
    data <- read_exp_fields(paste0(datapath, "/", dataset), cols = sel_var)
    data <- data[, -1]  # Select columns

    # remove variables depending on the language argument
    if(!language){ # german labels
      # Create a logical vector identifying columns that end with "_en"
      rows_to_remove <- grepl("_en$", data$type)
      # Return the data frame with columns removed
      data <- data[!rows_to_remove, ]
      # add one line with a char vector with all existent value labels
      # thus, first read data and then extract attracted labels
      val_labels <- attr(read_dta(paste0(datapath,"/", dataset), n_max = 1)[[sel_var]], "labels")
      unnamed_val_labels <- paste0(val_labels, ": ", names(val_labels),";")
      # turn into one single string
      unnamed_val_labels <- paste(unnamed_val_labels, collapse = " ")
      new_row <- data.frame(type = "Value Labels", value = unnamed_val_labels)
      data <- rbind(new_row, data)
    }
    else{ # english labels
      # Create a logical vector identifying columns that end with "_de"
      rows_to_remove <- grepl("_de$", data$type)
      # Return the data frame with columns removed
      data <- data[!rows_to_remove, ]
      # load data and switch to english value labels with custom function, then extract attracted english labels
      val_labels <- attr(assign_val_labels(paste0(datapath, dataset), haven::read_dta(paste0(datapath,"/", dataset), n_max = 0))[[sel_var]], "labels")
      unnamed_val_labels <- paste0(val_labels, ": ", names(val_labels),";")
      # turn into one single string
      unnamed_val_labels <- paste(unnamed_val_labels, collapse = " ")
      new_row <- data.frame(type = "Value Labels", value = unnamed_val_labels)
      data <- rbind(new_row, data)
    }
    # Remove pre and suffixes and capitalize first letter
    data <- remove_prefix_suffix_capitalize_df(data, colnames=F)

    # Rename columns
    data <- rename(data, "Type of Detail" = "type", "Value" = "value")

    return(data)
  }
  # if there are no expansion fields, return informative message
  else {
    return("No meta table available for this dataset")
  }
}

read_dta_eng_labels <- function(datasetpath, col_select = NULL) {
  # read data with haven::read_dta
  if(is.null(col_select))
    data <- read_dta(datasetpath)
  else
    data <- read_dta(datasetpath, col_select = all_of(col_select))
  # retrieve english labels from expansion fields
  en_labels <- read_exp_fields(datasetpath, col_select, "NEPS_varlabel_en") |> select(-type)
  # assign english variable labels
  data <- assign_var_labels(data, en_labels)
  # assign english value labels
  data <- assign_val_labels(datasetpath, data)

  # ensure all 'labels' attributes are named double vectors
  data[] <- lapply(data, function(x) {
    lab <- attr(x, "labels")
    if (!is.null(lab) && is.integer(lab)) {
      lab <- as.double(lab)
      names(lab) <- names(attr(x, "labels"))
      attr(x, "labels") <- lab
    }
    x
  })

  return(data)
}

# generate different dataset infos and returns a list with different info strings
generate_info <- function(datapath, dataset){
  dataset_name <- str_extract(dataset, "(?<=_)([^_]+)(?=_)") # regex: first part: positive lookbehind: It asserts that what immediately precedes the current position is an underscore 2. part. main capture part: one or more any character but underscore. 3. part. positive lookahead: It asserts that what immediately follows the current position is an underscore
  data <- read_dta(paste0(datapath,dataset), n_max=1)
  var_count <- length(data)
  data2 <- read_dta(paste0(datapath,dataset), col_select = 1)
  obs_count <- nrow(data2)
  obs_count_distinct <- sum(!duplicated(data2$ID_t))
  infolist <- list(dataset_name, obs_count,obs_count_distinct,var_count)
  return(infolist)
}


# Function to create a dataframe from user inputs in datatransformation multiinput
create_dataframe <- function(dataset, variable) {
  # Split the variables by comma and trim whitespace
  variable <- trimws(unlist(strsplit(variable, ",")))

  # Create a dataframe with the variable
  dataframe <- data.frame(variable = variable, stringsAsFactors = FALSE)

  # Create a new vector with the length equal to the number of rows in the dataframe
  dataset_vector <- rep(dataset, nrow(dataframe))

  # Add the new vector as a column to the original dataframe
  dataframe$Dataset <- dataset_vector

  return(dataframe)
}

# returns a dataframe for the merging of selected variables to person-year-dataset
create_linkage_data <- function(datapath){
  # read csv file with linkage keys
  linkage_keys <- read.csv(file = "SC6_linkage_keys.csv", sep=";")
  # determine suf version
  suf_version <- extract_suf_version(datapath)
  # determine sc in datapath
  sc <- toupper(identify_sc(datapath))

  data <- linkage_keys |>
    filter(Linkage != "" & str_starts(Dataset, sc)) |>
    select(-Info)

  print(head(data))
  print(tail(data))
  # we need to add to each string element in the Dataset column the suf-version in datapath
  # first, we  ".dta" from dataset names, so we can easily paste suf_version and then .dta back to the names
  data$Dataset <- str_replace(data$Dataset, ".dta","") # remove .dta
  data$Dataset <- paste0(data$Dataset, suf_version, ".dta")
  return(data)
}

# function that returns a vector of valid dataset names for the dataset input field.
create_dataset_names <- function(datapath){
  linkage_keys <- read.csv(file = "SC6_linkage_keys.csv", sep=";")

  # determine suf version
  suf_version <- extract_suf_version(datapath)
  # determine sc in datapath
  sc <- toupper(identify_sc(datapath))

  datasetnames <- linkage_keys |>
    filter(Linkage != "" & str_starts(Dataset, sc)) |>
    select(-Info, -Linkage, -Sptype) |>
    pull()
  # we need to add to each string element the suf-version in datapath
  # next, we cut of ".dta" from dataset names, so we can easily paste suf_version and then .dta back to the names
  datasetnames <- str_replace(datasetnames, ".dta","")
  datasetnames <- paste0(datasetnames, suf_version, ".dta")
  return(datasetnames)
}


# function for updateing the multiinput field with varnames from selected dataset
gen_labels_for_multi <- function(datapath, dataset) {
  data <- haven::read_dta(paste0(datapath,"/", dataset), n_max=1)
  varlabels <- paste(names(data),"-", unname(sapply(data, attr,which="label")))
  return(varlabels[!varlabels %in% c("wave - Welle","ID_t - Target-ID","splink - Link für Spell-Merging")])
}

assign_val_labels <- function(datasetpath, data) {
  # load names of val labels from expansion fields along with variable names in a df
  suppressWarnings(expfields <- read_exp_fields(datasetpath, attr_type = '_lang_l_en'))

  # extract attracted val labels from 'label.table' as a named integer list along with names of val labels
  suppressWarnings(label_table <- attr(readstata13::read.dta13(datasetpath, select.rows = 1), 'label.table'))

  # Loop through each variable in exp_fields
  for (i in 1:nrow(expfields)) {
    variable_name <- expfields$variable[i]
    value_name <- expfields$value[i]

    # Check if the value exists in label_table
    if (value_name %in% names(label_table)) {
      # Get the labels from label_table
      new_labels <- label_table[[value_name]]

      # Convert integer labels to double (haven::write_dta requires double)
      if (is.integer(new_labels)) {
        new_labels <- as.double(new_labels)
        names(new_labels) <- names(label_table[[value_name]])
      }

      # Check if the attribute exists in data and assign new labels
      if (variable_name %in% names(data) && !is.null(attr(data[[variable_name]], 'labels'))) {
        attr(data[[variable_name]], 'labels') <- new_labels
      }
    }
  }

  return(data)
}

assign_var_labels <- function(data, en_labels) {
  # Iterate through each variable in the data frame
  for (var in names(data)) {
    # Find the corresponding label in en_labels
    label <- en_labels |> filter(variable == var) |> pull(value)
    # If a label is found, apply it to the variable
    if (length(label) > 0) {
      attr(data[[var]], 'label') <- label
    }
  }
  return(data)
}


# creates a list of variable names from multiinput with dataset as lists name
gen_list_for_picker <-  function(dataset, vars){
  # extract datasetname
  dataset_name_short <- str_match(dataset, "SC\\d+_(.*?)_D")[, 2]

  # Create a named list with the name provided by 'dataset'
  new_list <- setNames(as.list(vars), vars)

  # Assign the list to the name stored in 'dataset'
  assign(dataset_name_short, new_list, envir = .GlobalEnv)

  # Return the newly created list (optional, depending on use case)
  return(new_list)
}

# This function filters in a list of dataframes for all vars provided in the 2. argument
filter_dataframes <- function(list_of_dfs, vars) {
  filtered_list_of_dfs <- lapply(list_of_dfs, function(df) {
    # Filter the dataframe based on the variable names provided in vars
    filtered_df <- df %>%
      filter(variable %in% vars) # assuming variable named "variable" exists in dfs in list_of_dfs

    return(filtered_df)
  })
  return(filtered_list_of_dfs)
}
