# Build the codebook frequency data shipped with the app (inst/extdata/codebook).
#
# Input: the CSV files written by extract_codebook.py (run_all.ps1), one pair per
# starting cohort and language:
#   codebook_vars_<SC>_<version>_<lang>.csv   one row per variable
#   codebook_freq_<SC>_<version>_<lang>.csv   one row per value of a variable
# Source: NEPS codebooks, FDZ-LIfBi, licensed under CC BY 4.0.
#
# Output: one .rds per cohort and language (codebook_sc6_en.rds, ...) plus
# codebook_index.rds, the keys of all variables with codebook information.
# Rerun this after extracting the codebooks of a new SUF release.

csv_dir <- "C:/work_cowork/codebook_extract/output"
out_dir <- "inst/extdata/codebook"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_cb <- function(path) {
  utils::read.csv(path, encoding = "UTF-8", fileEncoding = "UTF-8-BOM",
                  colClasses = "character", na.strings = character(0))
}

vars_files <- list.files(csv_dir, pattern = "^codebook_vars_SC\\d+_[0-9-]+_(de|en)\\.csv$")
index <- character(0)

for (vf in vars_files) {
  parts <- regmatches(vf, regexec("^codebook_vars_(SC\\d+)_([0-9-]+)_(de|en)\\.csv$", vf))[[1]]
  sc <- parts[2]; version <- parts[3]; lang <- parts[4]

  vars <- read_cb(file.path(csv_dir, vf))
  freq <- read_cb(file.path(csv_dir, sub("^codebook_vars_", "codebook_freq_", vf)))

  wave_cols <- grep("^w\\d+$", names(freq), value = TRUE)
  freq <- data.frame(
    key = paste(freq$dataset, freq$variable, sep = "|"),
    value = freq$value,
    label = freq$label,
    missing = freq$missing == "1",
    n_total = suppressWarnings(as.integer(freq$n_total)),
    lapply(freq[wave_cols], function(x) suppressWarnings(as.integer(x))),
    stringsAsFactors = FALSE
  )
  # rows of a variable are consecutive in the extract; remember where each one starts
  vars$key <- paste(vars$dataset, vars$variable, sep = "|")
  runs <- rle(freq$key)
  ends <- cumsum(runs$lengths)
  vars$first <- (ends - runs$lengths + 1L)[match(vars$key, runs$values)]
  vars$n_rows <- runs$lengths[match(vars$key, runs$values)]
  vars$n_rows[is.na(vars$n_rows)] <- 0L
  freq$key <- NULL

  cb <- list(
    cohort = sc,
    version = version,
    language = lang,
    vars = data.frame(
      key = vars$key,
      varlabel = vars$varlabel,
      has_table = vars$has_table == "1",
      by_wave = vars$by_wave == "1",
      omitted_values = as.integer(vars$omitted_values),
      first = vars$first,
      n_rows = vars$n_rows,
      stringsAsFactors = FALSE
    ),
    freq = freq
  )
  saveRDS(cb, file.path(out_dir, sprintf("codebook_%s_%s.rds", tolower(sc), lang)), compress = "xz")

  # the Explore table shows the codebook button only for variables with a frequency table
  if (lang == "en") {
    with_table <- vars$has_table == "1" & vars$n_rows > 0
    index <- c(index, paste(sc, version, vars$key[with_table], sep = "|"))
  }
  message(sprintf("%s %s %s: %d variables, %d value rows", sc, version, lang, nrow(vars), nrow(freq)))
}

saveRDS(sort(unique(index)), file.path(out_dir, "codebook_index.rds"), compress = "xz")
