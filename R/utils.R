# Main Functions ---------------------------------------------------------

#' # function that returns string about starting cohort in form: 'sc3':'sc6'
#' @keywords internal
#' @noRd
getWindowsDrives <- function() {
  drives <- sprintf("%s:/", LETTERS)
  drives <- drives[file.exists(drives)]
  names(drives) <- gsub(":/", "", drives)  # C, D, E
  drives
}


#' inspects filenames in a dir and return sc cohort identifier
#' @keywords internal
#' @noRd
identify_sc <- function(datapath) {
  # List files in the directory
  files <- base::list.files(datapath)

  # Extract strings starting with "SC" followed by a digit at the beginning of filenames
  sc_string <- base::unique(stringr::str_extract(files, "^SC\\d"))

  # Remove NAs (files that don't match)
  sc_string <- sc_string[!base::is.na(sc_string)]

  # Check if no matching SC found
  if (length(sc_string) == 0) {
    stop("No SC cohort identifier found in the dataset directory. Please provide datasets starting with 'SC' followed by a digit.")
  }

  # Check if more than one unique SC found, if yes, throw error
  if (length(sc_string) > 1) {
    stop(
      base::paste0(
        "Multiple SC cohorts detected in provided datapath: ",
        base::paste(sc_string, collapse = ", "),
        ". Please dont mix NEPS datasets from different starting cohorts in provided datapath"
      )
    )
  }
  return(base::tolower(sc_string))
}

#' function that filters a dataset depending on variable type
#' @keywords internal
#' @noRd
filter_var <- function(x, val) {
  if (base::is.numeric(x)) {
    !base::is.na(x) & x >= val[1] & x <= val[2]
  }
  else if (base::is.factor(x) | base::is.character(x) | inherits(x, "labelled")) {
    x %in% val
  }
  else {
    TRUE
  }
}

#' function to check and transform variables to a factor if they are not of type factor or labelled doubles
#' @keywords internal
#' @noRd
transform_all_to_factors <- function(data) {
  for (variable in base::names(data)) {
    if (!base::is.factor(data[[variable]]) && !inherits(data[[variable]], "labelled")) {
      data[[variable]] <- base::as.factor(data[[variable]])
    }
  }
  return(data)
}

#' helper function for "filter_data" function
#' @keywords internal
#' @noRd
cond_filter <- function(data, vars) {
  for (var in vars) {
    if (var %in% base::names(data)) {
      data <- data |>
        dplyr::filter(.data[[var]] == 1)
    }
  }
  return(data)
}

#' function to truncate strings to 15 chars and return as unnamed char vec
#' @keywords internal
#' @noRd
truncate_strings <- function(input_strings) {
  truncated <- base::ifelse(base::nchar(input_strings) > 15,
                            base::paste0(base::substr(input_strings, 1, 15), "..."),
                            input_strings)
  return(base::unname(truncated))
}

#' faster ifelse version:
#' @keywords internal
#' @noRd
fast_ifelse <- function(test, yes, no) {
  stopifnot(identical(base::class(yes), base::class(no)))

  out <- base::rep(NA, length(test))
  out[test] <- yes
  out[!test] <- no
  base::class(out) <- base::class(yes)

  out
}

#' extracts the latest suf version from the datafiles name of the suf data in given datapath
#' @keywords internal
#' @noRd
extract_suf_version <- function(datapath, short = FALSE){
  distinct_strings <- base::unique(stringr::str_extract(base::list.files(datapath,pattern="\\d{1,2}-\\d-\\d"), "\\d{1,2}-\\d-\\d"))
  num_strings <- base::as.numeric(stringr::str_extract(distinct_strings,"\\d{1,2}"))
  last_part_of_string <- stringr::str_extract(distinct_strings,"-\\d-\\d")[1]
  num_strings <- base::sort(num_strings, decreasing = TRUE)
  num_string <- num_strings[1]

  if(base::length(distinct_strings)==0){
    warning("Not possible to determine suf version - check provided directory and restart the app")
  }
  else if(base::length(distinct_strings) == 1){
    if(short){
      return(num_strings)
    } else {
      return(base::paste0(as.character(num_strings), last_part_of_string))
    }
  }
  else if(base::length(distinct_strings) > 1){
    if(short){
      return(num_string)
    } else {
      return(base::paste0(as.character(num_string), last_part_of_string))
    }
  }
}

#' function to read dta and assign variable labels (which are attracted but not yet assigned) to the variables
#' @keywords internal
#' @noRd
readstata13_label <- function(x){
  data <- suppressWarnings(readstata13::read.dta13(x))
  data_meta_names <- base::as.vector(attr(data,"names"))
  data_meta_labels <- base::as.vector(attr(data,"var.labels"))
  base::names(data_meta_labels) <- data_meta_names
  for(i in seq_along(data)){
    Hmisc::label(data[,i]) <- data_meta_labels[i]
  }
  data
}

#' Generate question text for a variable
#' @keywords internal
#' @noRd
gen_questiontext <- function(datapath, dataset, col_select, variable, language){
  if(stringr::str_detect(dataset, "SC.*\\d\\d-\\d-\\d\\.dta$")){
    if(!language){
      data <- read_exp_fields(
        base::paste0(datapath,"/", dataset),
        cols = col_select,
        attr_type = "NEPS_questiontext_de",
        only_value = TRUE
      )
    } else {
      data <- read_exp_fields(
        base::paste0(datapath,"/", dataset),
        cols = col_select,
        attr_type = "NEPS_questiontext_en",
        only_value = TRUE
      )
    }
    return(base::paste0("<b>Selected Variable: ",
                        variable,
                        ". Questiontext: <i>",
                        if(base::length(data)==0) "Not available" else data,
                        "</i></b>"))
  }
  else {
    return(base::paste0("<b>Selected Variable: ", variable))
  }
}


#' function to bind vectors of different lengths and fill missings up with NA
#' @keywords internal
#' @noRd
bind_cols_fill <- function(df_list) {
  max_rows <- purrr::map_int(df_list, base::nrow) |> base::max()

  purrr::map(df_list, function(df) {
    if (base::nrow(df) == max_rows) return(df)
    first <- base::names(df)[1] |> rlang::sym()
    df |> tibble::add_row(!!first := base::rep(NA, max_rows - base::nrow(df)))
  }) |> dplyr::bind_cols()
}

#' Helper function to capitalize the first letter of a string
#' @keywords internal
#' @noRd
capitalize_first_letter <- function(x) {
  base::paste0(base::toupper(base::substring(x, 1, 1)), base::substring(x, 2))
}

#' helper function to remove neps prefix from variable names in df and capitalize first letter
#' @keywords internal
#' @noRd
remove_prefix_suffix_capitalize_df <- function(df, colnames = TRUE) {
  if (!base::is.data.frame(df)) stop("Input must be a dataframe")

  if(colnames){
    base::colnames(df) <- stringr::str_replace(base::colnames(df), "^NEPS_", "")
    base::colnames(df) <- stringr::str_replace(base::colnames(df), "(_de|_en)$", "")
    base::colnames(df) <- base::sapply(base::colnames(df), capitalize_first_letter)
  } else {
    df$type <- stringr::str_replace(df$type, "^NEPS_", "")
    df$type <- stringr::str_replace(df$type, "(_de|_en)$", "")
    df$type <- capitalize_first_letter(df$type)
  }

  return(df)
}

#' helper function to remove neps prefix from variable names in vector and capitalize first letter
#' @keywords internal
#' @noRd
remove_prefix_suffix_capitalize_vec <- function(var_names) {
  if (!base::is.character(var_names)) stop("Input must be a character vector")

  var_names <- stringr::str_replace(var_names, "(_de|_en)$", "")
  var_names <- stringr::str_replace(var_names, "^NEPS_", "")
  var_names <- base::sapply(var_names, capitalize_first_letter, USE.NAMES = FALSE)

  return(var_names)
}

#' Process metadata for input selection list
#' @keywords internal
#' @noRd
process_meta_for_input_list <- function(datapath, dataset){
  read_exp_fields(file.path(datapath, dataset)) |>
    dplyr::filter(
      variable != "_dta" &
        !stringr::str_detect(type, "^NEPSMISS") &
        !stringr::str_detect(type, "shortvarlab") &
        !stringr::str_detect(type, "^_lang") &
        !(type %in% c(
          "NEPS_varname","NEPS_procedures_de","NEPS_procedures_en",
          "NEPS_relations_de","NEPS_relations_en",
          "NEPS_limitations_de","NEPS_limitations_en"
        ))
    ) |>
    dplyr::distinct(type, .keep_all = FALSE) |>
    dplyr::pull() |>
    stringr::str_remove_all("_de$|_en$") |>
    base::unique()
}

#' Read expansion fields from a dataset
#' @keywords internal
#' @noRd
read_exp_fields <- function(datapath, cols = NULL, attr_type = NULL, only_value = FALSE) {
  data <- suppressWarnings(readstata13::read.dta13(datapath, select.cols = "ID_t", select.rows = 1))
  exp_fields <- base::attr(data, "expansion.fields")

  if(base::is.null(exp_fields) | base::length(exp_fields) == 0){
    stop("Cant find attracted meta data in selected dataset. Please ensure you select only NEPS datasets here")
  }

  fields_df <- base::data.frame(
    variable = base::sapply(exp_fields, `[[`, 1),
    type     = base::sapply(exp_fields, `[[`, 2),
    value    = base::sapply(exp_fields, `[[`, 3),
    stringsAsFactors = FALSE
  )

  if (!base::is.null(attr_type) & !base::is.null(cols)) {
    fields_df <- fields_df |> dplyr::filter(variable %in% cols & type == attr_type)
  }
  if (base::is.null(attr_type) & !base::is.null(cols)) {
    fields_df <- fields_df |> dplyr::filter(variable %in% cols)
  }
  if (!base::is.null(attr_type) & base::is.null(cols)) {
    fields_df <- fields_df |> dplyr::filter(type == attr_type)
  }
  if (only_value) fields_df <- fields_df |> dplyr::pull(value)

  return(fields_df)
}

#' helper function to switch labels between english and german for specific variables
#' @keywords internal
#' @noRd
add_suffix <- function(strings, language = "de") {
  specific_strings <- c(
    "_lang_v", "_lang_l", "NEPS_varlabel",
    "NEPS_interviewerinstruction", "NEPS_questiontext",
    "NEPS_outputfilter", "NEPS_autofillinstruction",
    "NEPS_valuefilter","NEPS_variablequestion",
    "NEPS_inputfilter","NEPS_shortvarlab",
    "NEPS_itemsinstruction","NEPS_varlabel","NEPS_unit"
  )

  base::ifelse(strings %in% specific_strings,
               base::paste0(strings, "_", language),
               strings)
}

#' helper function to change order of strings in vector
#' @keywords internal
#' @noRd
move_string_to_position <- function(vec, string_to_move, target_position) {
  if (!base::is.character(vec)) stop("Input must be a character vector")
  if (!(string_to_move %in% vec)) stop(base::paste("String", string_to_move, "does not exist in the vector"))

  vec <- vec[vec != string_to_move]
  if (target_position < 1 || target_position > base::length(vec) + 1) stop("Target position is out of bounds")

  base::append(vec, string_to_move, after = target_position - 1)
}

#' helper function to change order of vars in dataframe
#' @keywords internal
#' @noRd
move_variable_to_position <- function(df, var_to_move, after_var) {
  if (!base::is.data.frame(df)) stop("Input must be a dataframe")
  if (!(var_to_move %in% colnames(df))) stop(base::paste("Variable", var_to_move, "does not exist in the dataframe"))
  if (!(after_var %in% colnames(df))) stop(base::paste("Variable", after_var, "does not exist in the dataframe"))

  columns <- colnames(df)
  after_index <- which(columns == after_var)
  columns <- columns[-which(columns == var_to_move)]
  new_order <- c(columns[1:after_index], var_to_move, columns[(after_index + 1):base::length(columns)])
  df[new_order]
}

#' Generate char vector of variable labels with dataset
#' @keywords internal
#' @noRd
gen_comb_char <- function(datapath, dataset, language){
  data <- haven::read_dta(base::file.path(datapath,dataset), n_max = 0)

  if(language & stringr::str_detect(dataset, "SC.*\\d\\d-\\d-\\d\\.dta$")){
    varlabels_en <- read_exp_fields(base::file.path(datapath, dataset), attr_type="NEPS_varlabel_en") |> dplyr::select(-type)
    data <- assign_var_labels(data, varlabels_en)
  }

  base::paste(names(data), "-", base::unname(base::sapply(data, base::attr, which = "label")))
}


#' generates table with meta info
#' @keywords internal
#' @noRd
gen_data_overview <- function(datapath, dataset, language){
  datasetpath <- base::file.path(datapath, dataset)

  # read data
  data <- read_exp_fields(datasetpath)

  # pivot to wide format
  data <- tidyr::pivot_wider(data, names_from = "type", values_from = "value")

  # add Dataset column
  data$Dataset <- dataset
  data <- data.frame(Dataset = data$Dataset, data[, base::names(data) != "Dataset", drop = FALSE])

  # remove _dta variable and rename
  data <- data |>
    dplyr::filter(variable != "_dta") |>
    dplyr::rename(Variable = variable)

  # remove columns depending on language
  if(language == "de"){
    columns_to_remove <- base::grepl("_en$", base::colnames(data))
    data <- data[, !columns_to_remove]
  } else {
    columns_to_remove <- base::grepl("_de$", base::colnames(data))
    data <- data[, !columns_to_remove]
  }

  # remove prefix/suffix and capitalize
  data <- remove_prefix_suffix_capitalize_df(data)

  # move Varlabel and Questiontext to early positions
  if("Varlabel" %in% base::colnames(data))
    data <- move_variable_to_position(data, "Varlabel", "Variable")
  if("Questiontext" %in% base::colnames(data))
    data <- move_variable_to_position(data, "Questiontext", "Varlabel")

  return(data)
}

#' Generate metadata table for a single variable
#' @keywords internal
#' @noRd
gen_meta_var_table <- function(datapath, dataset, sel_var, language, value_labels){
  if(stringr::str_detect(dataset, "\\d{2}-\\d-\\d")){
    data <- read_exp_fields(file.path(datapath, dataset), cols = sel_var)
    data <- data[, -1]  # drop variable column

    if(!language){ # german labels
      rows_to_remove <- base::grepl("_en$", data$type)
      data <- data[!rows_to_remove, ]

      val_labels <- base::attr(haven::read_dta(file.path(datapath, dataset), n_max = 1)[[sel_var]], "labels")
      unnamed_val_labels <- paste0(val_labels, ": ", names(val_labels), ";")
      unnamed_val_labels <- paste(unnamed_val_labels, collapse = " ")
      new_row <- data.frame(type = "Value Labels", value = unnamed_val_labels)
      data <- rbind(new_row, data)

    } else { # english labels
      rows_to_remove <- base::grepl("_de$", data$type)
      data <- data[!rows_to_remove, ]

      val_labels <- attr(assign_val_labels(base::paste0(datapath, dataset),
                                                 haven::read_dta(base::paste0(datapath,"/", dataset), n_max = 0))[[sel_var]], "labels")
      unnamed_val_labels <- paste0(val_labels, ": ", names(val_labels), ";")
      unnamed_val_labels <- paste(unnamed_val_labels, collapse = " ")
      new_row <- base::data.frame(type = "Value Labels", value = unnamed_val_labels)
      data <- rbind(new_row, data)
    }

    # remove prefix/suffix and capitalize
    data <- remove_prefix_suffix_capitalize_df(data, colnames = FALSE)

    # rename columns
    data <- dplyr::rename(data, "Type of Detail" = "type", "Value" = "value")
    return(data)

  } else {
    return("No meta table available for this dataset")
  }
}

#' Read Stata dataset with English labels assigned
#' @keywords internal
#' @noRd
read_dta_eng_labels <- function(datasetpath, col_select = NULL) {
  data <- if(base::is.null(col_select)) {
    haven::read_dta(datasetpath)
  } else {
    haven::read_dta(datasetpath, col_select = dplyr::all_of(col_select))
  }

  en_labels <- read_exp_fields(datasetpath, col_select, "NEPS_varlabel_en") |> dplyr::select(-type)
  data <- assign_var_labels(data, en_labels)
  data <- assign_val_labels(datasetpath, data)

  data[] <- base::lapply(data, function(x){
    lab <- base::attr(x, "labels")
    if(!base::is.null(lab) && base::is.integer(lab)){
      lab <- base::as.double(lab)
      base::names(lab) <- base::names(base::attr(x, "labels"))
      base::attr(x, "labels") <- lab
    }
    x
  })

  return(data)
}

#' Generate basic dataset info
#' @keywords internal
#' @noRd
generate_info <- function(datapath, dataset){
  dataset_name <- stringr::str_extract(dataset, "(?<=_)([^_]+)(?=_)")
  data <- haven::read_dta(base::file.path(datapath, dataset), n_max = 1)
  var_count <- base::length(data)
  data2 <- haven::read_dta(base::file.path(datapath, dataset), col_select = 1)
  obs_count <- base::nrow(data2)
  obs_count_distinct <- base::sum(!base::duplicated(data2$ID_t))
  base::list(dataset_name, obs_count, obs_count_distinct, var_count)
}

#' Create dataframe from dataset and variable names
#' @keywords internal
#' @noRd
create_dataframe <- function(dataset, variable) {
  variable <- base::trimws(base::unlist(base::strsplit(variable, ",")))
  dataframe <- base::data.frame(variable = variable, stringsAsFactors = FALSE)
  dataframe$Dataset <- base::rep(dataset, base::nrow(dataframe))
  return(dataframe)
}

#' Create linkage dataset for merging in data transformation
#' @keywords internal
#' @noRd
create_linkage_data <- function(datapath){
  linkage_keys_path <- system.file("extdata", "linkage_keys.csv", package = "NEPScribe")
  if (linkage_keys_path == "") stop("linkage_keys.csv not found in NEPScribe/extdata")

  linkage_keys <- utils::read.csv(
    file = linkage_keys_path,
    sep = ";"
  )
  suf_version <- extract_suf_version(datapath)
  sc <- base::toupper(identify_sc(datapath))

  data <- linkage_keys |>
    dplyr::filter(Linkage != "" & stringr::str_starts(Dataset, sc)) |>
    dplyr::select(-Info)

  data$Dataset <- stringr::str_replace(data$Dataset, ".dta", "")
  data$Dataset <- base::paste0(data$Dataset, suf_version, ".dta")
  return(data)
}

#' Create dataset names from linkage keys
#' @keywords internal
#' @noRd
create_dataset_names <- function(datapath){
  linkage_keys_path <- system.file("extdata", "linkage_keys.csv", package = "NEPScribe")
  linkage_keys_path
  if (linkage_keys_path == "") stop("linkage_keys.csv not found in NEPScribe/extdata")

  linkage_keys <- utils::read.csv(
    file = linkage_keys_path,
    sep = ";"
  )
  suf_version <- extract_suf_version(datapath)
  sc <- base::toupper(identify_sc(datapath))

  datasetnames <- linkage_keys |>
    dplyr::filter(Linkage != "" & stringr::str_starts(Dataset, sc)) |>
    dplyr::select(-Info, -Linkage, -Sptype) |>
    dplyr::pull()

  datasetnames <- stringr::str_replace(datasetnames, ".dta","")
  datasetnames <- base::paste0(datasetnames, suf_version, ".dta")
  return(datasetnames)
}

#' Create variable labels for multi-variable selections
#' @keywords internal
#' @noRd
gen_labels_for_multi <- function(datapath, dataset) {
  data <- haven::read_dta(base::paste0(datapath,"/", dataset), n_max = 1)
  varlabels <- base::paste(names(data), "-", base::unname(base::sapply(data, base::attr, which="label")))
  return(varlabels[!varlabels %in% c("wave - Welle","ID_t - Target-ID","splink - Link für Spell-Merging")])
}

#' Assign value labels to dataset
#' @keywords internal
#' @noRd
assign_val_labels <- function(datasetpath, data){
  suppressWarnings(expfields <- read_exp_fields(datasetpath, attr_type = '_lang_l_en'))
  suppressWarnings(label_table <- base::attr(readstata13::read.dta13(datasetpath, select.rows = 1), 'label.table'))

  for(i in 1:base::nrow(expfields)){
    variable_name <- expfields$variable[i]
    value_name <- expfields$value[i]

    if(value_name %in% base::names(label_table)){
      new_labels <- label_table[[value_name]]
      if(base::is.integer(new_labels)){
        new_labels <- base::as.double(new_labels)
        base::names(new_labels) <- base::names(label_table[[value_name]])
      }
      if(variable_name %in% base::names(data) && !base::is.null(base::attr(data[[variable_name]], 'labels'))){
        base::attr(data[[variable_name]], 'labels') <- new_labels
      }
    }
  }
  return(data)
}

#' Assign variable labels to dataset
#' @keywords internal
#' @noRd
assign_var_labels <- function(data, en_labels){
  for(var in base::names(data)){
    label <- en_labels |> dplyr::filter(variable == var) |> dplyr::pull(value)
    if(base::length(label) > 0) base::attr(data[[var]], 'label') <- label
  }
  return(data)
}

#' Generate list for picker UI
#' @keywords internal
#' @noRd
gen_list_for_picker <-  function(dataset, vars){
  dataset_name_short <- stringr::str_match(dataset, "SC\\d+_(.*?)_S")[, 2]
  new_list <- stats::setNames(base::as.list(vars), vars)
  base::assign(dataset_name_short, new_list, envir = .GlobalEnv)
  return(new_list)
}

#' Filter a list of dataframes by variable names
#' @keywords internal
#' @noRd
filter_dataframes <- function(list_of_dfs, vars){
  base::lapply(list_of_dfs, function(df){
    dplyr::filter(df, variable %in% vars)
  })
}
