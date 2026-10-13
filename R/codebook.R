# Frequencies from the public NEPS codebooks (FDZ-LIfBi, CC BY 4.0), shown in a modal
# in Explore Datasets. The data is built by dev/build_codebook_data.R from the codebook
# PDFs and lives in inst/extdata/codebook: one file per starting cohort and language,
# loaded the first time a variable of that cohort is opened.

.codebook_cache <- new.env(parent = emptyenv())

#' Read a file from inst/extdata/codebook once per session
#' @keywords internal
#' @noRd
codebook_read <- function(file) {
  if (is.null(.codebook_cache[[file]])) {
    path <- system.file("extdata", "codebook", file, package = "NEPScribe")
    .codebook_cache[[file]] <- if (nzchar(path)) readRDS(path) else FALSE
  }
  .codebook_cache[[file]]
}

#' Split semantic file names like "SC6_Basics_S_17-0-0.dta" into cohort, dataset and version
#' @keywords internal
#' @noRd
codebook_split_file <- function(file) {
  m <- regmatches(file, regexec("^(SC\\d+)_(.+)_S_(\\d+-\\d+-\\d+)(_beta)?\\.dta$", file))
  part <- function(i) vapply(m, function(x) if (length(x)) x[i] else NA_character_, character(1))
  list(cohort = part(2), dataset = part(3), version = part(4))
}

#' Keys "SC6|17-0-0|Basics|t70000y" as stored in codebook_index.rds
#' @keywords internal
#' @noRd
codebook_keys <- function(file, variable) {
  f <- codebook_split_file(file)
  base::paste(f$cohort, f$version, f$dataset, variable, sep = "|")
}

#' Which rows of the Explore table have codebook information
#'
#' The version is part of the key, so after an update of the semantic files the
#' modal stays hidden until the codebook data of the same release is built.
#' @keywords internal
#' @noRd
codebook_available <- function(file, variable) {
  index <- codebook_read("codebook_index.rds")
  if (isFALSE(index)) return(rep(FALSE, length(variable)))
  codebook_keys(file, variable) %in% index
}

#' Codebook entry of one variable: metadata row and its frequency rows
#' @keywords internal
#' @noRd
codebook_entry <- function(file, variable, language) {
  f <- codebook_split_file(file)
  if (is.na(f$cohort)) return(NULL)
  # fall back to the other language if a variable is missing in one extract
  for (lang in unique(c(language, setdiff(c("en", "de"), language)))) {
    cb <- codebook_read(sprintf("codebook_%s_%s.rds", tolower(f$cohort), lang))
    if (isFALSE(cb) || cb$version != f$version) next
    i <- match(base::paste(f$dataset, variable, sep = "|"), cb$vars$key)
    if (is.na(i)) next
    v <- cb$vars[i, ]
    rows <- if (v$n_rows > 0) seq(v$first, length.out = v$n_rows) else integer(0)
    return(list(
      cohort = f$cohort, version = f$version, dataset = f$dataset, variable = variable,
      language = lang, var = v, freq = cb$freq[rows, , drop = FALSE]
    ))
  }
  NULL
}

#' Number formatting following the label language
#' @keywords internal
#' @noRd
codebook_num <- function(x, language) {
  de <- language == "de"
  out <- base::formatC(x, format = "d", big.mark = if (de) "." else ",", decimal.mark = if (de) "," else ".")
  out[is.na(x)] <- ""
  out
}
codebook_pct <- function(x, language) {
  out <- base::formatC(100 * x, format = "f", digits = 1, decimal.mark = if (language == "de") "," else ".")
  out <- base::paste0(out, "%")
  out[is.na(x) | !is.finite(x)] <- ""
  out
}

#' Modal with the codebook frequencies of one variable
#' @keywords internal
#' @noRd
codebook_modal <- function(entry) {
  tags <- htmltools::tags

  if (is.null(entry)) {
    return(shiny::modalDialog(
      title = "No codebook information",
      "The codebook contains no information on this variable.",
      easyClose = TRUE, footer = shiny::modalButton("Close")
    ))
  }

  lang <- entry$language
  v <- entry$var
  fr <- entry$freq
  label <- v$varlabel

  # waves that appear in this variable's table, and the valid cases in each of them
  wave_cols <- grep("^w\\d+$", names(fr), value = TRUE)
  wave_cols <- wave_cols[vapply(fr[wave_cols], function(x) any(!is.na(x)), logical(1))]
  valid <- !fr$missing
  valid_by_wave <- vapply(fr[wave_cols], function(x) sum(x[valid], na.rm = TRUE), numeric(1))
  valid_total <- sum(fr$n_total[valid], na.rm = TRUE)
  wave_no <- sub("^w", "", wave_cols)

  # --- wave strip: in which waves does the variable carry information ---
  waves_block <- if (v$by_wave && length(wave_cols) > 0) {
    n_on <- sum(valid_by_wave > 0)
    tags$div(
      class = "cb-section",
      tags$div(class = "cb-section-title",
               sprintf("Valid information in %d of %d waves", n_on, length(wave_cols))),
      tags$div(
        class = "cb-waves",
        lapply(seq_along(wave_cols), function(k) {
          on <- valid_by_wave[k] > 0
          tags$span(
            class = if (on) "cb-wave on" else "cb-wave off",
            `data-toggle` = "tooltip",
            title = if (on) {
              sprintf("Wave %s: %s valid cases", wave_no[k], codebook_num(valid_by_wave[k], lang))
            } else {
              sprintf("Wave %s: no valid cases (only missing codes)", wave_no[k])
            },
            wave_no[k]
          )
        })
      )
    )
  } else if (v$has_table && nrow(fr) > 0) {
    tags$div(class = "cb-section cb-note",
             "The codebook does not break this variable down by wave, so only totals are shown.")
  }

  # --- frequency table ---
  # counts, with the share among valid cases shaded behind each valid count; the
  # "Show percentages" switch swaps the counts for those shares
  cell <- function(n, share) {
    style <- if (!is.na(share) && share > 0) {
      sprintf("background: rgba(108, 195, 213, %.2f);", 0.08 + 0.6 * share)
    }
    tags$td(
      class = "cb-num", style = style,
      title = if (!is.na(share)) sprintf("%s of valid cases", codebook_pct(share, lang)),
      tags$span(class = "cb-n", codebook_num(n, lang)),
      tags$span(class = "cb-p", if (is.na(share)) "–" else codebook_pct(share, lang))
    )
  }

  table_block <- if (v$has_table && nrow(fr) > 0) {
    head <- tags$tr(
      tags$th("Value"), tags$th("Label"), tags$th(class = "cb-num", "Total"),
      tags$th(class = "cb-bar-col", "Share of valid cases"),
      if (length(wave_cols)) lapply(wave_no, function(w) tags$th(class = "cb-num", paste0("W", w)))
    )
    body <- lapply(seq_len(nrow(fr)), function(r) {
      miss <- fr$missing[r]
      share_total <- if (miss || valid_total == 0) NA else fr$n_total[r] / valid_total
      tags$tr(
        class = if (miss) "cb-missing",
        tags$td(class = "cb-value", fr$value[r]),
        tags$td(class = "cb-label", fr$label[r]),
        tags$td(class = "cb-num", codebook_num(fr$n_total[r], lang)),
        tags$td(
          class = "cb-bar-col",
          if (!is.na(share_total)) tags$div(
            class = "cb-bar",
            tags$div(class = "cb-bar-fill", style = sprintf("width: %.1f%%;", 100 * share_total)),
            tags$span(class = "cb-bar-text", codebook_pct(share_total, lang))
          )
        ),
        lapply(seq_along(wave_cols), function(k) {
          n <- fr[[wave_cols[k]]][r]
          share <- if (miss || valid_by_wave[k] == 0 || is.na(n)) NA else n / valid_by_wave[k]
          cell(n, share)
        })
      )
    })
    tags$div(
      class = "cb-section",
      tags$div(
        class = "cb-table-head",
        tags$div(class = "cb-section-title", "Frequencies"),
        if (length(wave_cols)) tags$label(
          class = "cb-pct-switch",
          tags$input(type = "checkbox",
                     onchange = "this.closest('.cb-body').classList.toggle('cb-show-pct', this.checked);"),
          " Show percentages by wave"
        )
      ),
      tags$div(class = "cb-table-wrap", tags$table(class = "cb-table", tags$thead(head), tags$tbody(body)))
    )
  } else {
    tags$div(class = "cb-section cb-note", "The codebook contains no frequency table for this variable.")
  }

  # --- notes and source ---
  notes <- c(
    "Note: ",
    if (v$has_table && nrow(fr) > 0)
      "Missing codes (in grey) are left out of the shares.",
    if (!is.na(v$omitted_values) && v$omitted_values > 0)
      sprintf("The codebook does not list all values: %d further values are left out.", v$omitted_values),
    if (grepl("^sp", entry$dataset))
      "The counts are data rows, not persons. Most spell datasets contain both harmonized episodes and their subspells, so one episode can be counted several times."
  )
  cohort_no <- sub("^SC", "", entry$cohort)
  source <- sprintf(
    "Source: NEPS Starting Cohort %s codebook, SUF version %s (LIfBi), licensed under CC BY 4.0. Counts as printed in the codebook.",
    cohort_no, entry$version
  )

  shiny::modalDialog(
    title = tags$div(
      class = "cb-title",
      tags$span(class = "cb-varname", entry$variable),
      if (!is.na(label) && nzchar(label)) tags$span(class = "cb-varlabel", label),
      tags$div(class = "cb-meta", sprintf("%s · %s · SUF %s", entry$cohort, entry$dataset, entry$version))
    ),
    tags$div(
      class = "cb-body",
      waves_block,
      table_block,
      tags$div(class = "cb-footnotes",
               lapply(notes, tags$p),
               tags$p(class = "cb-source", source))
    ),
    size = "xl",
    easyClose = TRUE,
    footer = shiny::modalButton("Close")
  )
}
