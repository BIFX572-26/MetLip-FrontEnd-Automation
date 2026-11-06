# =============================================================================
# fct_data.R - pure functions (no Shiny state) that touch data.
# Everything here is unit-tested in tests/testthat.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- small utilities -------------------------------------------------------

pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[[1]] else NA_character_
}

as_qc_flag <- function(x) {
  if (is.logical(x)) {
    x
  } else if (is.numeric(x)) {
    x == 1
  } else {
    grepl("^QC", as.character(x), ignore.case = TRUE)
  }
}

is_plot_like <- function(x) inherits(x, c("gg", "ggplot", "ggmatrix", "patchwork"))

label_for <- function(nm) {
  ifelse(nm %in% names(PLOT_LABELS), PLOT_LABELS[nm], nm)
}

## ---- reading the qcdrift result object -------------------------------------

get_tbl <- function(res, which) {
  tbl <- res$data[[which]]
  if (is.data.frame(tbl)) tbl else NULL
}

get_plot <- function(res, key) {
  if (is.null(res)) return(NULL)
  for (nm in PLOT_ALIASES[[key]]) {
    p <- res$plots[[nm]]
    if (!is.null(p)) return(p)
  }
  NULL
}

plots_found <- function(res) {
  vapply(names(PLOT_ALIASES), function(k) !is.null(get_plot(res, k)), logical(1))
}

## ---- upload validation -----------------------------------------------------

# Returns a data frame, or an error object (never throws).
read_upload <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls")) {
    return(simpleError("The file must be an Excel workbook (.xlsx or .xls)."))
  }
  tryCatch(
    suppressMessages(readxl::read_excel(path)),
    error = function(e) simpleError(paste("Could not read the file:", conditionMessage(e)))
  )
}

# Plain-language checks. `errors` block the run; `warnings` do not, because the
# exact header layout is owned by qcdrift and may differ from what we expect.
check_upload <- function(df, prefix = "QC") {
  out <- list(ok = FALSE, errors = character(), warnings = character(),
              n_rows = NA_integer_, n_cols = NA_integer_, n_qc = NA_integer_)

  if (inherits(df, "error")) {
    out$errors <- conditionMessage(df)
    return(out)
  }

  out$n_rows <- nrow(df)
  out$n_cols <- ncol(df)

  if (nrow(df) == 0 || ncol(df) < 3) {
    out$errors <- "The file looks empty or has too few columns (expected features by injections)."
    return(out)
  }

  cells <- c(names(df), as.character(unlist(utils::head(df, 5), use.names = FALSE)))
  cells <- toupper(trimws(cells))
  out$n_qc <- if (nzchar(prefix)) sum(startsWith(cells, toupper(prefix)), na.rm = TRUE) else NA_integer_

  if (nzchar(prefix) && isTRUE(out$n_qc == 0)) {
    out$warnings <- paste0(
      "No QC injections starting with '", prefix, "' were found in the header. ",
      "Drift correction needs QC samples - check the prefix under Advanced options.")
  } else if (isTRUE(out$n_qc < 3)) {
    out$warnings <- paste0(
      "Only ", out$n_qc, " QC marker(s) detected. At least 3 QC injections ",
      "are recommended for a reliable drift correction.")
  }

  out$ok <- TRUE
  out
}

## ---- running the package ---------------------------------------------------

# Builds the process_runs() call from its real formal arguments, so the app
# keeps working if argument names differ slightly between package versions.
# Runs inside a background worker, so it must not touch Shiny.
run_pipeline <- function(path, norm, prefix = "QC") {
  fm <- names(formals(qcdrift::process_runs))
  path_arg <- if ("filepath" %in% fm) "filepath" else fm[[1]]

  args <- stats::setNames(list(path), path_arg)
  if ("norm_method" %in% fm) {
    args$norm_method <- norm
  } else if ("method" %in% fm) {
    args$method <- norm
  }
  if (nzchar(prefix) && "qc_starts_with" %in% fm) args$qc_starts_with <- prefix

  do.call(qcdrift::process_runs, args)
}

## ---- long format for the drift explorer ------------------------------------

to_long <- function(tbl, stage, value_candidates) {
  if (is.null(tbl)) return(NULL)

  io  <- pick_col(tbl, c("io", "injection_order", "order"))
  met <- pick_col(tbl, c("metabolites", "metabolite", "feature"))
  val <- pick_col(tbl, value_candidates)
  qc  <- pick_col(tbl, c("qc", "is_qc"))
  smp <- pick_col(tbl, c("sample", "name", "sample_name"))
  if (anyNA(c(io, met, val))) return(NULL)

  data.frame(
    io         = as.numeric(tbl[[io]]),
    metabolite = as.character(tbl[[met]]),
    value      = as.numeric(tbl[[val]]),
    qc         = if (!is.na(qc)) as_qc_flag(tbl[[qc]]) else FALSE,
    sample     = if (!is.na(smp)) as.character(tbl[[smp]]) else NA_character_,
    stage      = stage,
    stringsAsFactors = FALSE
  )
}

build_drift_long <- function(res) {
  raw  <- to_long(get_tbl(res, "raw"), "Raw", c("abundance", "value", "intensity"))
  corr <- to_long(get_tbl(res, "corrected"), "Corrected",
                  c("abundance_corrected", "corrected_abundance", "abundance"))
  out <- rbind(raw, corr)
  if (is.null(out)) return(NULL)
  out$stage <- factor(out$stage, levels = c("Raw", "Corrected"))
  out
}

# QC coefficient of variation (%) per feature, one column per stage.
# abs(mean) keeps the CV meaningful if a feature has a negative mean.
qc_cv_table <- function(long) {
  cv <- long |>
    dplyr::filter(qc, is.finite(value)) |>
    dplyr::group_by(stage, metabolite) |>
    dplyr::summarise(cv = 100 * stats::sd(value) / abs(mean(value)), .groups = "drop") |>
    tidyr::pivot_wider(names_from = stage, values_from = cv)
  as.data.frame(cv)
}
