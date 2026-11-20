#' qcdrift_module.R, updated w/ui elements and server calls
# =============================================================================
# qcdrift_module.R  -  QC Drift for the MetLip dashboard

# #############################################################################
# 1. CONSTANTS
# #############################################################################

# =============================================================================
# qcd_config.R - constants for the QC Drift tabs of the MetLip dashboard.
# All QC Drift objects are prefixed qcd_ / QCD_ so they cannot collide with
# the TAS Accelerator or Sequence Generator code.
# =============================================================================

#' @import shiny
#' @importFrom ggplot2 ggplot aes geom_tile geom_line geom_point geom_vline
#' @importFrom ggplot2 scale_fill_gradient2 scale_colour_manual scale_shape_manual
#' @importFrom ggplot2 scale_x_discrete scale_y_log10 facet_wrap coord_cartesian
#' @importFrom ggplot2 labs theme_minimal theme element_blank element_text ggsave
#' @importFrom plotly ggplotly layout plotlyOutput renderPlotly
#' @importFrom DT DTOutput renderDT
#' @importFrom shinycssloaders withSpinner
#' @name qcdrift_helpers
NULL

utils::globalVariables(c( "qc", "value", "stage", "metabolite", "metabolites", "sample", "io", 
  "type", "tip", "rho", "CV", "Stage", "Share", "Raw", "Corrected", "Improvement", 
  "abundance", "scaled_abundance", "corrected_abundance"
))

QCD_CV_DEFAULT <- 20      # % - the usual acceptance limit for QC CV
QCD_PASS_SHARE   <- 80      # % of features that must be under the limit
                                # for a "successful" verdict (a heuristic)

# Okabe-Ito colour-blind-safe palette ----------------------------------------
QCD_PAL <- list(
  primary   = "#0072B2",
  ok        = "#009E73",
  warn      = "#E69F00",
  bad       = "#D55E00",
  info      = "#56B4E9",
  qc        = "#D55E00",
  sample    = "#0072B2",
  raw       = "#E69F00",
  corrected = "#009E73"
)

QCD_MSG_NO_RESULT <- "Run the pipeline on the QC Drift > Data Upload page to see this."

# Each dashboard slot can be filled by any one of several names found in
# results$plots; the first name present wins.
QCD_PLOT_ALIASES <- list(
  pca_raw            = c("pca_raw", "pca_before"),
  pca_corrected      = c("pca_corrected", "pca_after"),
  pca_pairs          = c("pca_pairs", "pca_matrix"),
  cv_raw             = c("cv_raw", "cv_before"),
  cv_corrected       = c("cv_corrected", "cv_after"),
  violin_raw         = c("violin_raw", "violin_before"),
  violin_corrected   = c("violin_corrected", "violin_after"),
  heatmap_raw        = c("heatmap_raw", "heatmap_before"),
  heatmap_corr       = c("heatmap_corr", "heatmap_corrected", "heatmap_after")
)

QCD_PLOT_LABELS <- c(
  pca_raw = "PCA - raw",            pca_corrected = "PCA - corrected",
  pca_pairs = "PCA pair matrix",
  cv_raw = "CV - raw",              cv_corrected = "CV - corrected",
  violin_raw = "Violin - raw",      violin_corrected = "Violin - corrected",
  heatmap_raw = "Heatmap - raw",    heatmap_corr = "Heatmap - corrected",
  heatmap_comparison = "Heatmap comparison",
  io_corr = "Injection order vs abundance correlation"
)

# Evidence tabs are generated from this list (see mod_plot_tab.R).
# Card defaults: width = 6, height = 420, interactive = TRUE.
QCD_TAB_SPECS <- list(
  pca = list(
    title = "PCA", icon = "chart-line",
    help = paste(
      "Each point is one injection. After correction, the QC injections",
      "(diamonds) should sit in a tight cluster. If they are still spread",
      "out, drift has not been fully removed."),
    cards = list(
      list(key = "pca_raw",       title = "Before correction"),
      list(key = "pca_corrected", title = "After correction"),
      list(key = "pca_pairs",     title = "PCA pair matrix",
           width = 12, height = 620, interactive = FALSE)
    )
  ),
  cv = list(
    title = "CV", icon = "chart-bar",
    help = paste(
      "The coefficient of variation (CV) shows how much each feature varies",
      "across QC injections. Lower is better, and 20% is the common limit.",
      "The corrected plot should sit clearly lower than the raw one."),
    cards = list(
      list(key = "cv_raw",       title = "Before correction"),
      list(key = "cv_corrected", title = "After correction")
    )
  ),
  heatmap = list(
    title = "QC Heatmap", icon = "th",
    help = paste(
      "Heatmaps: colour shows intensity across the injection order. Vertical",
      "'stripes' in the raw plot are the signature of drift; they should fade",
      "after correction. Correlation map: each row is a feature and the colour",
      "is how strongly its abundance rises or falls with injection order",
      "(Spearman correlation). The corrected column should be close to white."),
    cards = list(
      list(key = "heatmap_raw",  title = "Raw",
           height = 460, interactive = FALSE),
      list(key = "heatmap_corr", title = "Corrected",
           height = 460, interactive = FALSE)
    ),
    extra = "io_corr"   # adds the injection-order correlation map below the cards
  ),
  violin = list(
    title = "Violin", icon = "chart-area",
    help = paste(
      "Each violin is the intensity distribution of one injection. After",
      "normalization, medians and spreads should line up. A violin sitting",
      "far from the rest points to a sample that is off."),
    cards = list(
      list(key = "violin_raw",       title = "Before correction"),
      list(key = "violin_corrected", title = "After correction")
    )
  )
)

# #############################################################################
# 2. DATA FUNCTIONS
# #############################################################################

# =============================================================================
# qcd_fct_data.R - QC Drift: pure functions (no Shiny state) that touch data.
# =============================================================================

## ---- small utilities -------------------------------------------------------

qcd_pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[[1]] else NA_character_
}

qcd_as_flag <- function(x) {
  if (is.logical(x)) {
    x
  } else if (is.numeric(x)) {
    x == 1
  } else {
    grepl("^QC", as.character(x), ignore.case = TRUE)
  }
}

qcd_is_plot_like <- function(x) inherits(x, c("gg", "ggplot", "ggmatrix", "patchwork"))

qcd_label_for <- function(nm) {
  ifelse(nm %in% names(QCD_PLOT_LABELS), QCD_PLOT_LABELS[nm], nm)
}

## ---- reading the qcdrift result object -------------------------------------

qcd_get_tbl <- function(res, which) {
  tbl <- res$data[[which]]
  if (is.data.frame(tbl)) tbl else NULL
}

qcd_get_plot <- function(res, key) {
  if (is.null(res)) return(NULL)
  for (nm in QCD_PLOT_ALIASES[[key]]) {
    p <- res$plots[[nm]]
    if (!is.null(p)) return(p)
  }
  NULL
}

qcd_plots_found <- function(res) {
  vapply(names(QCD_PLOT_ALIASES), function(k) !is.null(qcd_get_plot(res, k)), logical(1))
}

## ---- upload validation -----------------------------------------------------

# Returns a data frame, or an error object (never throws).
qcd_read_upload <- function(path) {
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
qcd_check_upload <- function(df, prefix = "QC") {
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

# Autoscaling with the correct orientation. qcdrift::normalize_data(method = "auto")
# scales a samples-by-features matrix but then reads it back as features-by-samples,
# so sample and feature names are swapped and the join returns only NA.
# This is the same maths (per-feature mean 0, SD 1 across samples) with the matrix
# transposed back, returning the same long format as normalize_data().
qcd_autoscale_long <- function(corrected_data) {
  wide <- as.matrix(tibble::column_to_rownames(
    tidyr::pivot_wider(corrected_data, id_cols = metabolites,
                       names_from = sample, values_from = corrected_abundance),
    "metabolites"))
  scaled <- t(scale(t(wide), center = TRUE, scale = TRUE))   # features x samples

  dplyr::right_join(
    tidyr::pivot_longer(
      tibble::rownames_to_column(as.data.frame(scaled), "metabolites"),
      cols = -metabolites, names_to = "sample", values_to = "scaled_abundance"),
    corrected_data, by = c("metabolites", "sample"))
}

# Runs the qcdrift pipeline step by step, mirroring qcdrift::process_runs()
# (read -> correct -> normalize -> PCA -> plots) but with two differences:
#   * the QC prefix really reaches read_and_clean_data() and the PCA plots
#     (process_runs() has no argument for it);
#   * the three data steps must succeed, but each plot is built separately, so
#     one failing plot becomes a note instead of aborting the whole run.
# Returns list(data, plots, notes). Contains no Shiny code, so it can run in a
# background worker.
qcd_run_pipeline <- function(path, norm, prefix = "QC") {
  prefix <- if (nzchar(prefix)) prefix else "QC"
  notes  <- character()
  note   <- function(x) notes <<- c(notes, x)

  needed  <- c("read_and_clean_data", "qc_drift_correction", "normalize_data",
               "run_pca", "generate_pca_plot", "generate_pca_pairs",
               "generate_cv_plot", "generate_qc_violin", "generate_qc_heatmap")
  missing <- setdiff(needed, getNamespaceExports("qcdrift"))
  if (length(missing)) {
    stop("The installed qcdrift version does not provide: ",
         paste(missing, collapse = ", "), ". Please update the package.", call. = FALSE)
  }

  # A data step: any error stops the run, naming the qcdrift functions involved.
  must <- function(label, expr) {
    frames <- character()
    tryCatch(
      withCallingHandlers(
        expr, error = function(e) frames <<- qcd_frames(sys.calls())),
      error = function(e) {
        stop(paste0(label, " failed.\n", qcd_explain_error(conditionMessage(e), frames, norm)),
             call. = FALSE)
      })
  }
  # A plot step: an error becomes a note and a missing figure.
  try_plot <- function(label, expr) {
    tryCatch(expr, error = function(e) {
      note(paste0(label, " could not be drawn: ", qcd_strip_ansi(conditionMessage(e))))
      NULL
    })
  }

  long_data <- must("Reading the file",
    qcdrift::read_and_clean_data(filepath = path, qc_starts_with = prefix))
  long_corrected <- must("Drift correction", qcdrift::qc_drift_correction(long_data))
  long_scaled <- must("Normalization",
    qcdrift::normalize_data(long_corrected, method = norm))

  # Known qcdrift issue: its autoscaling returns only NA (see qcd_autoscale_long()).
  if (identical(norm, "auto") && all(!is.finite(long_scaled$scaled_abundance))) {
    long_scaled <- must("Autoscaling", qcd_autoscale_long(long_corrected))
    note(paste(
      "qcdrift's own autoscaling returned only missing values (it swaps sample and",
      "feature names before joining the result back). The dashboard applied the same",
      "autoscaling itself: each feature centred to mean 0 and scaled to SD 1 across samples."))
  }

  sa <- long_scaled$scaled_abundance
  if (!is.null(sa) && any(!is.finite(sa))) {
    note(sprintf("%.1f%% of the normalized (%s) values are missing or non-finite.",
                 100 * mean(!is.finite(sa)), norm))
  }

  # Bare column names (abundance, scaled_abundance) are tidy-eval arguments of
  # the qcdrift functions, exactly as in process_runs().
  pca_raw    <- tryCatch(qcdrift::run_pca(long_data, abundance), error = function(e) NULL)
  pca_scaled <- tryCatch(qcdrift::run_pca(long_scaled, scaled_abundance), error = function(e) NULL)

  pca_plot <- function(pcs, title, label) {
    if (is.null(pcs)) {
      note(paste0(label, " was skipped: fewer than 2 features had usable variance."))
      return(NULL)
    }
    try_plot(label, qcdrift::generate_pca_plot(pcs, title_prefix = title,
                                               qc_starts_with = prefix))
  }

  plots <- list(
    pca_raw       = pca_plot(pca_raw, "Raw Data", "PCA (raw)"),
    pca_corrected = pca_plot(pca_scaled, "Corrected & Scaled", "PCA (corrected)"),
    pca_pairs     = if (is.null(pca_scaled)) NULL else
      try_plot("PCA pair matrix", qcdrift::generate_pca_pairs(
        pca_scaled, npcs = min(4, ncol(pca_scaled$x)), qc_starts_with = prefix)),
    cv_raw = try_plot("CV (raw)",
      qcdrift::generate_cv_plot(long_data, abundance, title_prefix = "CV (Raw)")),
    cv_corrected = try_plot("CV (corrected)",
      qcdrift::generate_cv_plot(long_scaled, scaled_abundance, title_prefix = "CV (Corrected)")),
    violin_raw = try_plot("Violin (raw)",
      qcdrift::generate_qc_violin(long_data, abundance, title_prefix = "Raw QC Dist")),
    violin_corrected = try_plot("Violin (corrected)",
      qcdrift::generate_qc_violin(long_scaled, scaled_abundance, title_prefix = "Corrected QC Dist")),
    heatmap_raw = try_plot("Heatmap (raw)",
      qcdrift::generate_qc_heatmap(long_data, abundance, title_prefix = "Raw QC Heatmap")),
    heatmap_corr = try_plot("Heatmap (corrected)",
      qcdrift::generate_qc_heatmap(long_scaled, scaled_abundance, title_prefix = "Corrected QC Heatmap"))
  )

  list(
    data  = list(raw = long_data, corrected = long_corrected, scaled = long_scaled),
    plots = plots[!vapply(plots, is.null, logical(1))],
    notes = notes
  )
}

# Removes terminal colour codes (cli/rlang messages) so they read cleanly in the UI.
qcd_strip_ansi <- function(x) gsub("\033\\[[0-9;]*[A-Za-z]", "", x)

# Names of qcdrift's own functions found in a call stack, outermost first.
qcd_frames <- function(calls) {
  ns <- tryCatch(ls(asNamespace("qcdrift"), all.names = TRUE), error = function(e) character())
  nm <- vapply(calls, function(cl) {
    f <- cl[[1]]
    if (is.symbol(f)) as.character(f) else NA_character_
  }, character(1))
  unique(nm[!is.na(nm) & nm %in% ns])
}

# Turns a raw qcdrift error into a message a person can act on.
qcd_explain_error <- function(msg, frames, norm = NULL) {
  out <- qcd_strip_ansi(msg)
  if (length(frames)) {
    out <- paste0(out, "\n\nFailed inside qcdrift: ", paste0(frames, "()", collapse = " > "))
  }
  if (grepl("cannot coerce type 'closure'", out, fixed = TRUE)) {
    out <- paste0(
      out, "\n\nWhat this usually means: a table inside qcdrift is missing a column it ",
      "expects (for example 'sample'), so R found a function with the same name instead.",
      if (identical(norm, "auto"))
        " This was an Autoscaling run - try TSN. If TSN works, the problem is specific to qcdrift's autoscaling path and is worth reporting to the package authors with the line above."
      else "")
  }
  out
}

## ---- long format for the drift explorer ------------------------------------

qcd_to_long <- function(tbl, stage, value_candidates) {
  if (is.null(tbl)) return(NULL)

  io  <- qcd_pick_col(tbl, c("io", "injection_order", "order"))
  met <- qcd_pick_col(tbl, c("metabolites", "metabolite", "feature"))
  val <- qcd_pick_col(tbl, value_candidates)
  qc  <- qcd_pick_col(tbl, c("qc", "is_qc"))
  smp <- qcd_pick_col(tbl, c("sample", "name", "sample_name"))
  if (anyNA(c(io, met, val))) return(NULL)

  data.frame(
    io         = as.numeric(tbl[[io]]),
    metabolite = as.character(tbl[[met]]),
    value      = as.numeric(tbl[[val]]),
    qc         = if (!is.na(qc)) qcd_as_flag(tbl[[qc]]) else FALSE,
    sample     = if (!is.na(smp)) as.character(tbl[[smp]]) else NA_character_,
    stage      = stage,
    stringsAsFactors = FALSE
  )
}

qcd_build_long <- function(res) {
  raw  <- qcd_to_long(qcd_get_tbl(res, "raw"), "Raw", c("abundance", "value", "intensity"))
  corr <- qcd_to_long(qcd_get_tbl(res, "corrected"), "Corrected",
                  c("abundance_corrected", "corrected_abundance", "abundance"))
  out <- rbind(raw, corr)
  if (is.null(out)) return(NULL)
  out$stage <- factor(out$stage, levels = c("Raw", "Corrected"))
  out
}

# QC coefficient of variation (%) per feature, one column per stage.
# abs(mean) keeps the CV meaningful if a feature has a negative mean.
qcd_cv_table <- function(long) {
  cv <- long |>
    dplyr::filter(qc, is.finite(value)) |>
    dplyr::group_by(stage, metabolite) |>
    dplyr::summarise(cv = 100 * stats::sd(value) / abs(mean(value)), .groups = "drop") |>
    tidyr::pivot_wider(names_from = stage, values_from = cv)
  as.data.frame(cv)
}

# Correlation between injection order and abundance, per feature and stage.
# A drifting feature correlates strongly with injection order; after a good
# correction the value should be near 0. NA when fewer than 3 usable points
# or when either variable is constant.
qcd_io_correlation <- function(long, use = c("qc", "all"), method = "spearman") {
  use <- match.arg(use)
  d <- if (use == "qc") dplyr::filter(long, qc) else long
  d <- dplyr::filter(d, is.finite(io), is.finite(value))

  safe_cor <- function(x, y) {
    if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) return(NA_real_)
    stats::cor(x, y, method = method)
  }

  out <- d |>
    dplyr::group_by(stage, metabolite) |>
    dplyr::summarise(rho = safe_cor(io, value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = stage, values_from = rho)
  as.data.frame(out)
}

# #############################################################################
# 3. METRICS
# #############################################################################

# =============================================================================
# qcd_fct_metrics.R - turns qcdrift results into the numbers users care about.
# =============================================================================

qcd_kpis <- function(res) {
  out <- list(samples = NA_integer_, features = NA_integer_, qc = NA_integer_)
  raw <- qcd_get_tbl(res, "raw")
  if (is.null(raw)) return(out)

  sc <- qcd_pick_col(raw, c("sample", "name", "sample_name"))
  mc <- qcd_pick_col(raw, c("metabolites", "metabolite", "feature"))
  qc <- qcd_pick_col(raw, c("qc", "is_qc"))

  if (!is.na(sc)) {
    out$samples <- length(unique(raw[[sc]]))
    out$qc <- if (!is.na(qc)) {
      length(unique(raw[[sc]][qcd_as_flag(raw[[qc]])]))
    } else {
      sum(grepl("^QC", unique(raw[[sc]]), ignore.case = TRUE))
    }
  }
  if (!is.na(mc)) out$features <- length(unique(raw[[mc]]))
  out
}

# One-glance answer to "did the correction work?".
# Rules (documented in README):
#   success : corrected median QC CV <= threshold, at least QCD_PASS_SHARE %
#             of features under the threshold, and better than raw
#   danger  : corrected median still above threshold and no better than raw
#   warning : everything in between
qcd_verdict <- function(cv, thr = QCD_CV_DEFAULT) {
  if (is.null(cv) || !all(c("Raw", "Corrected") %in% names(cv))) {
    return(list(
      available = FALSE, level = "secondary",
      title = "Run complete",
      headline = "A numeric summary is not available for this result.",
      detail = paste(
        "The result tables do not contain the columns needed to compute QC",
        "precision (injection order, abundance, metabolite, QC flag). The",
        "diagnostic plots in the other tabs are still valid."),
      thr = thr
    ))
  }

  med_r <- stats::median(cv$Raw, na.rm = TRUE)
  med_c <- stats::median(cv$Corrected, na.rm = TRUE)
  ok_r  <- 100 * mean(cv$Raw <= thr, na.rm = TRUE)
  ok_c  <- 100 * mean(cv$Corrected <= thr, na.rm = TRUE)
  n_review <- sum(cv$Corrected > thr, na.rm = TRUE)

  level <- if (!is.finite(med_c)) {
    "secondary"
  } else if (med_c <= thr && ok_c >= QCD_PASS_SHARE && med_c < med_r) {
    "success"
  } else if (med_c > thr && med_c >= med_r) {
    "danger"
  } else {
    "warning"
  }

  title <- switch(level,
    success   = "Drift correction looks successful",
    warning   = "Partly corrected - review before use",
    danger    = "Correction did not bring QC precision into range",
    secondary = "Run complete")

  detail <- switch(level,
    success   = "You can go on to the figures, then export the corrected dataset.",
    warning   = paste0("Open the Drift Explorer to inspect the ", n_review,
                       " feature(s) still above the limit; consider excluding them."),
    danger    = paste("Check that QC injections are spread evenly through the run",
                      "and that the QC prefix is correct, then re-run."),
    secondary = "")

  list(
    available = TRUE, level = level, title = title,
    headline = sprintf(
      "Median QC CV went from %.1f%% to %.1f%%; %.0f%% of features are now under %s%% (was %.0f%%).",
      med_r, med_c, ok_c, format(thr), ok_r),
    detail = detail,
    med_raw = med_r, med_corr = med_c, ok_raw = ok_r, ok_corr = ok_c,
    n_features = nrow(cv), n_review = n_review, thr = thr
  )
}

# Features table with a Review flag, worst first.
qcd_review_table <- function(cv, thr = QCD_CV_DEFAULT) {
  if (is.null(cv)) return(NULL)
  has_corr <- "Corrected" %in% names(cv)
  has_raw  <- "Raw" %in% names(cv)
  key <- if (has_corr) cv$Corrected else cv$Raw

  out <- data.frame(Feature = cv$metabolite, stringsAsFactors = FALSE)
  if (has_raw)  out[["Raw CV (%)"]]       <- cv$Raw
  if (has_corr) out[["Corrected CV (%)"]] <- cv$Corrected
  if (has_raw && has_corr) out[["Change (pts)"]] <- cv$Corrected - cv$Raw
  out$Review <- ifelse(is.finite(key) & key > thr, "Yes", "No")

  out[order(-key, na.last = TRUE), , drop = FALSE]
}

# #############################################################################
# 4. PLOTS
# #############################################################################

# =============================================================================
# qcd_fct_plots.R - plots the dashboard draws itself (not returned by qcdrift).
# =============================================================================

# Correlation map: features (rows) x stage (columns), coloured by the
# correlation between injection order and abundance.
# `tooltip = TRUE` adds the hover text used by plotly; leave FALSE for files.
qcd_io_corr_plot <- function(long, use = "qc", method = "spearman", tooltip = FALSE) {
  cr   <- qcd_io_correlation(long, use, method)
  cols <- intersect(c("Raw", "Corrected"), names(cr))
  if (!length(cols) || all(is.na(unlist(cr[cols])))) return(NULL)

  ord <- cr$metabolite[order(cr[[cols[1]]], na.last = FALSE)]
  d <- tidyr::pivot_longer(cr, cols = dplyr::all_of(cols),
                           names_to = "Stage", values_to = "rho")
  d$Stage      <- factor(d$Stage, levels = cols)
  d$metabolite <- factor(d$metabolite, levels = ord)
  d$tip <- paste0("Feature: ", d$metabolite, "<br>", d$Stage, ": rho = ",
                  ifelse(is.na(d$rho), "n/a", sprintf("%.2f", d$rho)))

  n <- length(ord)
  aes_map <- if (tooltip) aes(Stage, metabolite, fill = rho, text = tip)
             else         aes(Stage, metabolite, fill = rho)

  ggplot(d, aes_map) +
    geom_tile(colour = if (n <= 40) "white" else NA) +
    scale_fill_gradient2(low = QCD_PAL$primary, mid = "white", high = QCD_PAL$bad,
                         midpoint = 0, limits = c(-1, 1), na.value = "grey85",
                         name = paste0(tools::toTitleCase(method), " rho")) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = if (n > 40) "Features (names hidden)" else NULL,
         title = paste("Injection order vs abundance -",
                       if (use == "qc") "QC injections" else "all injections")) +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank(),
          axis.text.y = if (n > 40) element_blank() else element_text(size = 8),
          plot.title = element_text(size = 12, face = "bold"))
}

# #############################################################################
# 5. EXPORT
# #############################################################################

# =============================================================================
# qcd_fct_export.R - everything that writes files: figures, workbook, report,
# and the one-click deliverables bundle.
# =============================================================================

## ---- figures ---------------------------------------------------------------

qcd_save_one <- function(p, file, dpi = 150, width = 9, height = 6.5) {
  ok <- tryCatch({
    ggplot2::ggsave(file, plot = p, width = width, height = height,
                    dpi = dpi, bg = "white")
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {                     # fallback for objects ggsave() cannot handle
    opened <- FALSE
    ok <- tryCatch({
      if (tolower(tools::file_ext(file)) == "pdf") {
        grDevices::pdf(file, width = width, height = height)
      } else {
        grDevices::png(file, width = width, height = height,
                       units = "in", res = dpi, bg = "white")
      }
      opened <- TRUE
      print(p)
      TRUE
    }, error = function(e) FALSE)
    if (opened) grDevices::dev.off()
  }
  ok && file.exists(file)
}

# Saves every plot-like element; returns a named vector of files written.
qcd_save_plots <- function(plots, dir, fmt = "png", dpi = 150) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  keep  <- Filter(qcd_is_plot_like, plots)
  paths <- vapply(names(keep), function(nm) {
    f <- file.path(dir, paste0(nm, ".", fmt))
    if (qcd_save_one(keep[[nm]], f, dpi)) f else NA_character_
  }, character(1))
  paths[!is.na(paths)]
}

## ---- reproducibility record --------------------------------------------------

qcd_run_info <- function(settings, kpis, thr) {
  pv  <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                              error = function(e) "n/a")
  fmt <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "n/a" else as.character(x)

  data.frame(
    Item = c("Input file", "Input file MD5", "Normalization", "QC prefix",
             "CV threshold (%)", "Samples", "Features", "QC injections",
             "MetLipAutomation version", "qcdrift version", "R version", "Generated"),
    Value = c(fmt(settings$name), fmt(settings$md5), toupper(fmt(settings$norm)),
              fmt(settings$prefix), fmt(thr), fmt(kpis$samples),
              fmt(kpis$features), fmt(kpis$qc), pv("MetLipAutomation"), pv("qcdrift"),
              paste(R.version$major, R.version$minor, sep = "."),
              format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    stringsAsFactors = FALSE
  )
}

## ---- dataset -----------------------------------------------------------------

qcd_workbook_sheets <- function(res, run_info, review) {
  sheets <- list(
    Corrected          = qcd_get_tbl(res, "corrected"),
    Scaled             = qcd_get_tbl(res, "scaled"),
    Features_to_review = if (!is.null(review)) review[review$Review == "Yes", , drop = FALSE],
    Run_info           = run_info
  )
  Filter(Negate(is.null), sheets)
}

## ---- PDF report -----------------------------------------------------------------

# A multi-page PDF using only base graphics: page 1 verdict and run summary,
# page 2 features to review and run notes, then every figure.
qcd_write_pdf_report <- function(res, run_info, verdict, review, out_file, extra_plots = list()) {
  grDevices::pdf(out_file, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))

  y <- 0.95
  new_page <- function() { graphics::plot.new(); y <<- 0.95 }
  put <- function(txt, cex = 1, font = 1, col = "black", family = "sans") {
    for (l in txt) {
      graphics::text(0.04, y, l, adj = c(0, 1), cex = cex, font = font, col = col, family = family)
      y <<- y - 0.045 * cex
    }
  }

  # ---- page 1: verdict + run summary
  new_page()
  put("QCDrift - Quality Control Report", cex = 1.9, font = 2, col = "#157b8d")
  y <- y - 0.02
  if (!is.null(verdict)) {
    col <- switch(verdict$level, success = "#00a65a", warning = "#e08e0b",
                  danger = "#dd4b39", "grey35")
    put(verdict$title, cex = 1.3, font = 2, col = col)
    put(strwrap(verdict$headline, 105), cex = 0.95)
    if (nzchar(verdict$detail)) put(strwrap(verdict$detail, 105), cex = 0.9, col = "grey30")
  }
  y <- y - 0.02
  put("Run summary", cex = 1.15, font = 2)
  put(paste0(run_info$Item, ": ", run_info$Value), cex = 0.8)

  # ---- page 2: features to review + notes
  flagged <- if (!is.null(review)) utils::head(review[review$Review == "Yes", , drop = FALSE], 14)
  has_flagged <- !is.null(flagged) && nrow(flagged) > 0 &&
    all(c("Raw CV (%)", "Corrected CV (%)") %in% names(flagged))
  if (has_flagged || length(res$notes)) {
    new_page()
    if (has_flagged) {
      put("Features above the CV limit after correction (worst first)", cex = 1.15, font = 2)
      put(sprintf("%-34s raw %6.1f%%    corrected %6.1f%%",
                  substr(flagged$Feature, 1, 34),
                  flagged[["Raw CV (%)"]], flagged[["Corrected CV (%)"]]),
          cex = 0.8, family = "mono")
      put("The full list is in the exported workbook (sheet Features_to_review).",
          cex = 0.75, col = "grey40")
      y <- y - 0.03
    }
    if (length(res$notes)) {
      put("Notes from this run", cex = 1.15, font = 2)
      for (n in res$notes) put(strwrap(paste("-", n), 110), cex = 0.8)
    }
  }

  # ---- figures
  plots <- c(res$plots, extra_plots)
  for (nm in names(plots)) {
    p <- plots[[nm]]
    if (qcd_is_plot_like(p)) tryCatch(print(p), error = function(e) NULL)
  }
  invisible(out_file)
}

## ---- deliverables bundle ---------------------------------------------------------

# Corrected dataset + figures + report + run info, zipped.
# Returns the names of the parts that could not be built.
qcd_build_bundle <- function(res, run_info, verdict, review, zip_file,
                         dpi = 150, extra_plots = list()) {
  dir <- file.path(tempdir(), paste0("bundle_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE)
  failed <- character()

  tryCatch(
    writexl::write_xlsx(qcd_workbook_sheets(res, run_info, review),
                        file.path(dir, "corrected_dataset.xlsx")),
    error = function(e) failed <<- c(failed, "dataset"))

  tryCatch(qcd_save_plots(c(res$plots, extra_plots), file.path(dir, "figures"), "png", dpi),
           error = function(e) failed <<- c(failed, "figures"))

  utils::write.csv(run_info, file.path(dir, "run_info.csv"), row.names = FALSE)

  tryCatch(
    qcd_write_pdf_report(res, run_info, verdict, review,
                         file.path(dir, "QCDrift_report.pdf"), extra_plots),
    error = function(e) failed <<- c(failed, "report"))

  zip::zip(zip_file, files = list.files(dir), root = dir)
  failed
}

# #############################################################################
# 6. UI HELPERS
# #############################################################################

# =============================================================================
# qcd_fct_ui.R - UI helpers for the QC Drift tabs (shinydashboard / AdminLTE 2).
# =============================================================================

# Soft callout box in MetLip's teal style. status: info, success, warning, danger.
qcd_callout <- function(title, ..., status = "info") {
  if (status %in% c("secondary", "primary")) status <- "info"
  div(class = paste("qcd-callout", paste0("qcd-callout-", status)), h4(title), ...)
}

# bs4Dash-style status names -> shinydashboard valueBox colours
qcd_vb_color <- function(x) {
  m <- c(primary = "light-blue", info = "aqua", danger = "red",
         success = "green", warning = "yellow")
  if (x %in% names(m)) m[[x]] else x
}

qcd_dt_view <- function(df, page_len = 10, selection = "none") {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  dt <- DT::datatable(
    df, rownames = FALSE, filter = "top", class = "compact stripe hover",
    selection = selection,
    options = list(scrollX = TRUE, pageLength = page_len, dom = "lfrtip")
  )
  if (length(num)) dt <- DT::formatSignif(dt, num, 4)
  dt
}

qcd_kpi_box <- function(value, subtitle, icon_name, color) {
  shown <- if (length(value) == 0 || is.na(value)) "-"
           else if (is.numeric(value)) format(value, big.mark = ",")
           else value
  shinydashboard::valueBox(value = shown, subtitle = subtitle,
                           icon = icon(icon_name), color = qcd_vb_color(color))
}

qcd_fmt_pct <- function(x) {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) "-" else sprintf("%.1f%%", x)
}

qcd_status_step <- function(done, label, detail = NULL) {
  tags$li(
    icon(if (done) "check-circle" else "circle",
         class = if (done) "text-green" else "text-muted"),
    tags$strong(label),
    if (!is.null(detail)) tags$span(class = "text-muted", style = "margin-left:8px;", detail)
  )
}

# Styles for the QC Drift tabs, matched to the MetLip teal theme.
qcd_css <- function() {
  "
  .qcd-steps { list-style: none; padding-left: 0; margin-bottom: 0; }
  .qcd-steps li { padding: .35rem 0; border-bottom: 1px solid #eef0f2; }
  .qcd-steps li:last-child { border-bottom: 0; }
  .qcd-callout { border-left: 5px solid #157b8d; background: #f4fafb; padding: 10px 14px;
                margin-bottom: 14px; border-radius: 3px; }
  .qcd-callout h4 { margin: 0 0 6px 0; font-size: 15px; font-weight: bold; }
  .qcd-callout p:last-child { margin-bottom: 0; }
  .qcd-callout-success { border-left-color: #00a65a; background: #f0faf5; }
  .qcd-callout-warning { border-left-color: #f39c12; background: #fff8ec; }
  .qcd-callout-danger  { border-left-color: #dd4b39; background: #fdf0ee; }
  .qcd-callout pre { background: transparent; border: 0; padding: 0; margin: 0;
                    white-space: pre-wrap; font-size: 12px; }
  .qcd-tabs details summary { cursor: pointer; font-weight: bold; margin: .5rem 0; }
  "
}

qcd_css_tag <- function() tags$style(HTML(qcd_css()))

# #############################################################################
# 7a. PLOT CARD, EVIDENCE TABS, CORRELATION MAP
# #############################################################################

# =============================================================================
# qcd_mod_plot.R - reusable plot card, an evidence tab built from QCD_TAB_SPECS,
# and the injection order vs abundance correlation map.
# A card shows an interactive plotly chart when the object converts cleanly and
# a static plot otherwise (patchwork, GGally matrices, heatmaps).
# =============================================================================

## ---- plot card -----------------------------------------------------------------

qcd_plot_card_ui <- function(id, title, width = 6, height = 420) {
  ns <- NS(id)
  shinydashboard::box(
    title = title, width = width, status = "primary", solidHeader = TRUE,
    collapsible = TRUE,
    div(style = paste0("min-height:", height, "px;"),
        withSpinner(uiOutput(ns("body")), type = 6, color = "#157b8d", size = .6))
  )
}

qcd_plot_card_server <- function(id, plot_fn, interactive = TRUE, height = 420) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    p <- reactive({
      pl <- plot_fn()
      validate(need(!is.null(pl), QCD_MSG_NO_RESULT))
      pl
    })

    ply <- reactive({
      pl <- p()
      if (interactive && inherits(pl, "ggplot") && !inherits(pl, "patchwork")) {
        tryCatch(plotly::ggplotly(pl), error = function(e) NULL)
      } else {
        NULL
      }
    })

    output$body <- renderUI({
      if (is.null(ply())) plotOutput(ns("static"), height = paste0(height, "px"))
      else                plotlyOutput(ns("interactive"), height = paste0(height, "px"))
    })
    output$static      <- renderPlot({ p() }, res = 96)
    output$interactive <- renderPlotly({ req(ply()); ply() })
  })
}

## ---- injection order vs abundance correlation map ---------------------------------

qcd_io_corr_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      title = "Injection order vs abundance - correlation map", width = 12,
      status = "primary", solidHeader = TRUE,
      radioButtons(ns("use"), "Injections used",
                   choices = c("QC injections only" = "qc", "All injections" = "all"),
                   inline = TRUE),
      p(class = "text-muted",
        "Orange = abundance rises with injection order, blue = it falls, white = no trend.",
        "QC-only is the cleanest drift check; 'All injections' also reflects sample",
        "composition. With hundreds of features the plot can take a few seconds to draw."),
      strong(textOutput(ns("summary"), inline = TRUE)),
      withSpinner(plotlyOutput(ns("plot"), height = "560px"),
                  type = 6, color = "#157b8d", size = .6)
    )
  )
}

qcd_io_corr_server <- function(id, long) {
  moduleServer(id, function(input, output, session) {

    output$summary <- renderText({
      lg <- long(); req(lg)
      cr   <- qcd_io_correlation(lg, input$use)
      cols <- intersect(c("Raw", "Corrected"), names(cr))
      req(length(cols) > 0)
      meds <- vapply(cols, function(cn) stats::median(abs(cr[[cn]]), na.rm = TRUE), numeric(1))
      if (all(is.na(meds))) return("")
      paste("Median |rho|:", paste0(cols, " ", sprintf("%.2f", meds), collapse = "  ->  "))
    })

    output$plot <- renderPlotly({
      lg <- long(); req(lg)
      g <- qcd_io_corr_plot(lg, input$use, tooltip = TRUE)
      validate(need(!is.null(g),
                    paste("Not enough injections to compute correlations (at least 3 are",
                          "needed per feature). Try 'All injections'.")))
      ggplotly(g, tooltip = "text")
    })
  })
}

## ---- evidence tab from a spec ---------------------------------------------------------

qcd_plot_tab_ui <- function(id, spec) {
  ns <- NS(id)
  cards <- lapply(spec$cards, function(cd) {
    qcd_plot_card_ui(ns(cd$key), cd$title, width = cd$width %||% 6, height = cd$height %||% 420)
  })
  tagList(
    qcd_callout("How to read this", spec$help, status = "info"),
    do.call(fluidRow, cards),
    if (identical(spec$extra, "io_corr")) qcd_io_corr_ui(ns("io_corr"))
  )
}

qcd_plot_tab_server <- function(id, res, spec, long = NULL) {
  moduleServer(id, function(input, output, session) {
    for (cd in spec$cards) {
      local({
        key   <- cd$key
        inter <- cd$interactive %||% TRUE
        h     <- cd$height %||% 420
        force(key); force(inter); force(h)
        qcd_plot_card_server(key, function() qcd_get_plot(res(), key),
                            interactive = inter, height = h)
      })
    }
    if (identical(spec$extra, "io_corr") && !is.null(long)) {
      qcd_io_corr_server("io_corr", long)
    }
  })
}

# #############################################################################
# 7b. VERDICT
# #############################################################################

# =============================================================================
# qcd_mod_verdict.R - did the correction work? (first tab after a run)
# =============================================================================

qcd_verdict_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    fluidRow(
      shinydashboard::valueBoxOutput(ns("vb_med_raw"),  width = 3),
      shinydashboard::valueBoxOutput(ns("vb_med_corr"), width = 3),
      shinydashboard::valueBoxOutput(ns("vb_ok_raw"),   width = 3),
      shinydashboard::valueBoxOutput(ns("vb_ok_corr"),  width = 3)
    ),
    fluidRow(
      shinydashboard::box(
        title = "QC precision across all features", width = 12,
        status = "primary", solidHeader = TRUE,
        p(class = "text-muted",
          "Each line shows the share of features (y) whose QC CV is at or below a",
          "given value (x). A curve that rises further to the left is better;",
          "the dashed line is your CV limit."),
        withSpinner(plotlyOutput(ns("curve"), height = "420px"),
                    type = 6, color = "#157b8d", size = .6)
      )
    )
  )
}

qcd_verdict_server <- function(id, res, verdict, cv_tbl, thr) {
  moduleServer(id, function(input, output, session) {

    v <- reactive(if (is.null(res())) NULL else verdict())

    output$banner <- renderUI({
      vd <- v()
      if (is.null(vd)) {
        return(qcd_callout("No results yet",
                          "Open 'Data Upload', choose a file and click Process Data.",
                          status = "info"))
      }
      qcd_callout(vd$title, tags$p(tags$strong(vd$headline)), tags$p(vd$detail),
                 status = vd$level)
    })

    vbox <- function(val, sub, ic, col) {
      shinydashboard::renderValueBox(
        shinydashboard::valueBox(val(), sub(), icon = icon(ic), color = qcd_vb_color(col)))
    }
    output$vb_med_raw  <- vbox(function() qcd_fmt_pct(v()$med_raw),  function() "Median QC CV - raw",       "percent", "warning")
    output$vb_med_corr <- vbox(function() qcd_fmt_pct(v()$med_corr), function() "Median QC CV - corrected", "percent", "success")
    output$vb_ok_raw   <- vbox(function() qcd_fmt_pct(v()$ok_raw),
                               function() paste0("Features under ", thr(), "% - raw"), "check", "warning")
    output$vb_ok_corr  <- vbox(function() qcd_fmt_pct(v()$ok_corr),
                               function() paste0("Features under ", thr(), "% - corrected"), "check-double", "success")

    output$curve <- renderPlotly({
      cv <- cv_tbl()
      validate(need(!is.null(cv) && any(c("Raw", "Corrected") %in% names(cv)), QCD_MSG_NO_RESULT))

      d <- cv |>
        tidyr::pivot_longer(cols = dplyr::any_of(c("Raw", "Corrected")),
                            names_to = "Stage", values_to = "CV") |>
        dplyr::filter(is.finite(CV)) |>
        dplyr::group_by(Stage) |>
        dplyr::arrange(CV, .by_group = TRUE) |>
        dplyr::mutate(Share = 100 * dplyr::row_number() / dplyr::n()) |>
        dplyr::ungroup()
      d$Stage <- factor(d$Stage, levels = c("Raw", "Corrected"))

      g <- ggplot(d, aes(CV, Share, colour = Stage)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = thr(), linetype = "dashed", colour = "grey40") +
        scale_colour_manual(values = c(Raw = QCD_PAL$raw, Corrected = QCD_PAL$corrected), name = NULL) +
        coord_cartesian(xlim = c(0, max(50, 2 * thr()))) +
        labs(x = "QC CV (%)", y = "Features at or below this CV (%)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())

      ggplotly(g) |> layout(legend = list(orientation = "h", y = -0.15))
    })
  })
}

# #############################################################################
# 7c. DRIFT EXPLORER
# #############################################################################

# =============================================================================
# qcd_mod_drift.R - inspect one feature at a time, and list features to review.
# =============================================================================

qcd_drift_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        title = "Choose a feature", width = 4, status = "primary", solidHeader = TRUE,
        selectizeInput(ns("feature"), "Metabolite / feature", choices = NULL),
        checkboxInput(ns("log"), "Log10 y-axis", FALSE),
        checkboxInput(ns("qcline"), "Connect QC injections", TRUE),
        helpText("Tip: click a row in the table below to jump to that feature.")
      ),
      shinydashboard::valueBoxOutput(ns("vb_raw"),  width = 3),
      shinydashboard::valueBoxOutput(ns("vb_corr"), width = 3),
      shinydashboard::valueBoxOutput(ns("vb_gain"), width = 2)
    ),
    fluidRow(
      shinydashboard::box(
        title = "Intensity vs injection order", width = 12, status = "primary",
        solidHeader = TRUE,
        p(class = "text-muted",
          "QC injections (diamonds) should lie on a flat line once drift is",
          "corrected. A slope or wave in the raw panel is instrument drift."),
        withSpinner(plotlyOutput(ns("plot"), height = "520px"),
                    type = 6, color = "#157b8d", size = .6)
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "Features ranked by QC CV", width = 12, status = "info",
        solidHeader = TRUE, collapsible = TRUE,
        p(class = "text-muted",
          "'Review = Yes' marks features whose corrected QC CV is above the limit",
          "set at the top of this page."),
        downloadButton(ns("dl_review"), "Download features to review (.csv)",
                       class = "btn-default btn-sm"),
        div(style = "height:.6rem;"),
        DTOutput(ns("table"))
      )
    )
  )
}

qcd_drift_server <- function(id, long, cv_tbl, thr) {
  moduleServer(id, function(input, output, session) {

    drift <- reactive({
      d <- long()
      validate(need(!is.null(d),
                    paste("The result tables do not contain the columns the drift",
                          "explorer needs (injection order, abundance, metabolite).")))
      d
    })

    review <- reactive(qcd_review_table(cv_tbl(), thr()))

    observeEvent(drift(), {
      feats <- sort(unique(drift()$metabolite))
      updateSelectizeInput(session, "feature", choices = feats,
                           selected = feats[1], server = TRUE)
    })

    output$plot <- renderPlotly({
      req(input$feature)
      d <- drift() |> dplyr::filter(metabolite == input$feature)
      if (isTRUE(input$log)) d <- dplyr::filter(d, value > 0)
      validate(need(nrow(d) > 0, "No data for this feature."))

      d$type <- ifelse(d$qc, "QC", "Sample")
      d$tip  <- paste0("Sample: ", d$sample, "<br>Injection: ", d$io,
                       "<br>Intensity: ", signif(d$value, 4))

      g <- ggplot(d, aes(io, value)) +
        geom_point(aes(colour = type, shape = type, text = tip), size = 2.2, alpha = .85) +
        scale_colour_manual(values = c(Sample = QCD_PAL$sample, QC = QCD_PAL$qc), name = NULL) +
        scale_shape_manual(values = c(Sample = 16, QC = 18), name = NULL) +
        facet_wrap(~stage, ncol = 1, scales = "free_y") +
        labs(x = "Injection order", y = "Intensity") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

      if (isTRUE(input$qcline)) {
        g <- g + geom_line(data = dplyr::filter(d, qc), colour = QCD_PAL$qc,
                           linewidth = .4, alpha = .6)
      }
      if (isTRUE(input$log)) g <- g + scale_y_log10()

      ggplotly(g, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.1))
    })

    # ---- per-feature CV boxes ------------------------------------------------------
    feature_cv <- function(col) {
      reactive({
        tb <- cv_tbl()
        if (is.null(tb) || !col %in% names(tb) || is.null(input$feature)) return(NA_real_)
        tb[[col]][tb$metabolite == input$feature][1]
      })
    }
    cv_raw  <- feature_cv("Raw")
    cv_corr <- feature_cv("Corrected")

    output$vb_raw  <- shinydashboard::renderValueBox(
      shinydashboard::valueBox(qcd_fmt_pct(cv_raw()), "QC CV - raw",
                               icon = icon("percent"), color = "yellow"))
    output$vb_corr <- shinydashboard::renderValueBox(
      shinydashboard::valueBox(qcd_fmt_pct(cv_corr()), "QC CV - corrected",
                               icon = icon("percent"), color = "green"))
    output$vb_gain <- shinydashboard::renderValueBox({
      g <- cv_corr() - cv_raw()
      shinydashboard::valueBox(if (is.finite(g)) sprintf("%+.1f", g) else "-",
                               "Change (pts)", icon = icon("arrow-down"), color = "aqua")
    })

    # ---- ranked table and cross-selection ---------------------------------------------
    output$table <- renderDT({
      rv <- review()
      validate(need(!is.null(rv), QCD_MSG_NO_RESULT))
      qcd_dt_view(rv, page_len = 8, selection = "single")
    })

    observeEvent(input$table_rows_selected, {
      sel <- input$table_rows_selected
      req(length(sel) == 1)
      updateSelectizeInput(session, "feature", selected = review()$Feature[sel], server = TRUE)
    })

    output$dl_review <- downloadHandler(
      filename = function() paste0("features_to_review_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        rv <- review(); req(rv)
        utils::write.csv(rv[rv$Review == "Yes", , drop = FALSE], file, row.names = FALSE)
      }
    )
  })
}

# #############################################################################
# 7d. DATA TABLES AND EXPORT
# #############################################################################

# =============================================================================
# qcd_mod_data_export.R - data-table tab and export tab for QC Drift.
# =============================================================================

## ---- data tables ------------------------------------------------------------------

qcd_data_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("tbl_tabs"),
    tabPanel("Raw",       div(style = "margin-top:10px;", DTOutput(ns("raw")))),
    tabPanel("Corrected", div(style = "margin-top:10px;", DTOutput(ns("corrected")))),
    tabPanel("Scaled",    div(style = "margin-top:10px;", DTOutput(ns("scaled"))))
  )
}

qcd_data_server <- function(id, res) {
  moduleServer(id, function(input, output, session) {
    table_output <- function(which) {
      renderDT({
        tb <- qcd_get_tbl(res(), which)
        validate(need(!is.null(tb), QCD_MSG_NO_RESULT))
        qcd_dt_view(tb)
      })
    }
    output$raw       <- table_output("raw")
    output$corrected <- table_output("corrected")
    output$scaled    <- table_output("scaled")
  })
}

## ---- export -------------------------------------------------------------------------

qcd_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "!output.has_results", ns = ns,
      qcd_callout("Nothing to export yet", "Run the pipeline on the Data Upload page first.",
                 status = "warning")
    ),
    conditionalPanel(
      "output.has_results", ns = ns,
      fluidRow(
        shinydashboard::box(
          title = "Everything in one click", width = 12, status = "success", solidHeader = TRUE,
          p("A single zip with the corrected dataset (Excel), all figures (PNG),",
            "the PDF QC report and a run-info file recording the settings,",
            "file checksum and package versions."),
          downloadButton(ns("dl_bundle"), "Download deliverables bundle (.zip)",
                         class = "btn-success")
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "Figures", width = 4, status = "primary", solidHeader = TRUE,
          radioButtons(ns("fig_fmt"), "Format", c("PNG" = "png", "PDF" = "pdf"), inline = TRUE),
          sliderInput(ns("fig_dpi"), "Resolution (PNG)", 72, 400, 150, step = 10),
          downloadButton(ns("dl_figs"), "All figures (.zip)", class = "btn-primary btn-block"),
          tags$hr(),
          selectInput(ns("single_fig"), "Single figure", choices = NULL),
          downloadButton(ns("dl_single"), "Download PNG", class = "btn-default btn-block")
        ),
        shinydashboard::box(
          title = "Corrected dataset", width = 4, status = "success", solidHeader = TRUE,
          p("Excel workbook with the corrected data, scaled data, features to",
            "review and run info - or the corrected table alone as CSV."),
          downloadButton(ns("dl_xlsx"), "Excel (.xlsx)", class = "btn-success btn-block"),
          div(style = "height:.6rem;"),
          downloadButton(ns("dl_csv"), "Corrected CSV", class = "btn-default btn-block")
        ),
        shinydashboard::box(
          title = "QC report", width = 4, status = "info", solidHeader = TRUE,
          p("Multi-page PDF: verdict, run info, features to review",
            "and every diagnostic figure."),
          downloadButton(ns("dl_report"), "Download PDF Report", class = "btn-info btn-block")
        )
      )
    )
  )
}

qcd_export_server <- function(id, res, long, cv_tbl, verdict, thr, kpis) {
  moduleServer(id, function(input, output, session) {

    output$has_results <- reactive(!is.null(res()))
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # Plots the dashboard draws itself (qcdrift does not return them).
    extra_plots <- reactive({
      lg <- tryCatch(long(), error = function(e) NULL)
      if (is.null(lg)) return(list())
      g <- tryCatch(qcd_io_corr_plot(lg, "qc"), error = function(e) NULL)
      if (is.null(g)) list() else list(io_corr = g)
    })
    all_plots <- reactive(c(res()$plots, extra_plots()))

    observeEvent(all_plots(), {
      figs <- Filter(qcd_is_plot_like, all_plots())
      if (length(figs) == 0) return()   # nothing to list before the first run
      updateSelectInput(session, "single_fig",
                        choices = stats::setNames(names(figs), qcd_label_for(names(figs))))
    })

    run_info <- reactive(qcd_run_info(res()$settings, kpis(), thr()))
    review   <- reactive(qcd_review_table(cv_tbl(), thr()))
    stamp    <- function() format(Sys.Date(), "%Y%m%d")

    output$dl_bundle <- downloadHandler(
      filename = function() paste0("QCDrift_deliverables_", stamp(), ".zip"),
      content = function(file) {
        req(res())
        withProgress(message = "Building deliverables...", value = 0.3, {
          failed <- qcd_build_bundle(res(), run_info(), verdict(), review(), file,
                                    dpi = input$fig_dpi, extra_plots = extra_plots())
          if (length(failed)) {
            showNotification(
              paste("Bundle created, but these parts could not be built:",
                    paste(failed, collapse = ", ")),
              type = "warning", duration = 12)
          }
        })
      }
    )

    output$dl_figs <- downloadHandler(
      filename = function() paste0("QCDrift_figures_", stamp(), ".zip"),
      content = function(file) {
        req(res())
        withProgress(message = "Saving figures...", value = 0.4, {
          dir   <- file.path(tempdir(), paste0("figs_", as.integer(Sys.time())))
          files <- qcd_save_plots(all_plots(), dir, input$fig_fmt, input$fig_dpi)
          validate(need(length(files) > 0, "No figures could be saved."))
          zip::zipr(file, unname(files), mode = "cherry-pick")
        })
      }
    )

    output$dl_single <- downloadHandler(
      filename = function() paste0(input$single_fig, ".png"),
      content = function(file) {
        req(res(), input$single_fig)
        qcd_save_one(all_plots()[[input$single_fig]], file, dpi = input$fig_dpi)
      }
    )

    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("QCDrift_corrected_", stamp(), ".xlsx"),
      content = function(file) {
        req(res())
        writexl::write_xlsx(qcd_workbook_sheets(res(), run_info(), review()), file)
      }
    )

    output$dl_csv <- downloadHandler(
      filename = function() paste0("QCDrift_corrected_", stamp(), ".csv"),
      content = function(file) {
        req(res())
        utils::write.csv(qcd_get_tbl(res(), "corrected"), file, row.names = FALSE)
      }
    )

    output$dl_report <- downloadHandler(
      filename = function() paste0("Metabolomics_QC_Report_", Sys.Date(), ".pdf"),
      content = function(file) {
        req(res())
        withProgress(message = "Building report...", value = 0.3, {
          ok <- tryCatch({
            qcd_write_pdf_report(res(), run_info(), verdict(), review(), file,
                                 extra_plots = extra_plots())
            TRUE
          }, error = function(e) {
            showNotification(paste0("Report failed: ", conditionMessage(e)),
                             type = "error", duration = NULL)
            NULL
          })
          req(ok)
        })
      }
    )
  })
}

# #############################################################################
# 8. PUBLIC API
# #############################################################################
# ---- background runs (future) -------------------------------------------------

.qcd_env <- new.env(parent = emptyenv())

# A dev-loaded package (devtools::load_all) is not installed, so background R
# workers could not load it; in that case runs happen in the main process.
qcd_is_dev_package <- function() {
  requireNamespace("pkgload", quietly = TRUE) &&
    isTRUE(tryCatch(pkgload::is_dev_package("MetLipAutomation"), error = function(e) FALSE))
}

qcd_use_async <- function() {
  isTRUE(getOption("metlip.qc_async", TRUE)) &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("promises", quietly = TRUE) &&
    !qcd_is_dev_package()
}

# Sets the future plan once per R process (called when the upload server starts).
qcd_setup_async <- function() {
  if (qcd_use_async() && !isTRUE(.qcd_env$plan_set)) {
    future::plan(future::multisession, workers = 2)
    .qcd_env$plan_set <- TRUE
  }
  invisible(NULL)
}

# =============================================================================
# PUBLIC API 1/4 - qcdrift_upload_ui()
# =============================================================================

#' QC Drift Upload UI
#' @param id Shiny module session ID.
#' @return A Shiny tabItem UI.
qcdrift_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "qc_upload",
    qcd_css_tag(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "1 - Choose data", width = 4, status = "primary", solidHeader = TRUE,
        shiny::p("Upload an Excel workbook containing your mass spectrometry data."),
        shiny::fileInput(
          ns("file"), "Select Excel Workbook",
          accept = c(".xlsx", ".xls"),
          buttonLabel = "Browse...",
          placeholder = "Raw data (.xlsx/.xls)"
        ),
        shiny::actionButton(
          ns("use_example"), "Try the example dataset",
          icon = shiny::icon("flask"),
          class = "btn-default btn-sm btn-block"
        ),
        shiny::br(),
        shiny::uiOutput(ns("file_summary")),
        shiny::uiOutput(ns("validation"))
      ),
      shinydashboard::box(
        title = "2 - Settings", width = 4, status = "primary", solidHeader = TRUE,
        shiny::radioButtons(
          ns("norm"), "Normalization",
          choiceNames = list("Total sum normalization (TSN)",
                             "Autoscaling (mean 0, SD 1)"),
          choiceValues = c("tsn", "auto")
        ),
        shiny::helpText("TSN is the usual choice. It corrects differences in sample",
                        "volume or dilution between injections."),
        shiny::div(
          class = "qcd-tabs",
          shiny::tags$details(
            shiny::tags$summary("Advanced options"),
            shiny::textInput(ns("prefix"), "QC sample prefix", value = "QC"),
            shiny::helpText("Samples whose names start with this text are treated as QC.")
          )
        ),
        shiny::actionButton(
          ns("process"), "Process Data",
          icon = shiny::icon("play"),
          class = "btn-primary btn-block"
        )
      ),
      shinydashboard::box(
        title = "Status", width = 4, status = "info", solidHeader = TRUE,
        shiny::uiOutput(ns("status"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("vb_samples"),  width = 3),
      shinydashboard::valueBoxOutput(ns("vb_features"), width = 3),
      shinydashboard::valueBoxOutput(ns("vb_qc"),       width = 3),
      shinydashboard::valueBoxOutput(ns("vb_norm"),     width = 3)
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Data preview", width = 12, status = "info", solidHeader = TRUE,
        shiny::textOutput(ns("dims")),
        shinycssloaders::withSpinner(DT::DTOutput(ns("preview")),
                                     type = 6, color = "#157b8d", size = .6)
      )
    )
  )
}

# =============================================================================
# PUBLIC API 3/4 - qcdrift_upload_server()
# Reads and checks the file, runs the pipeline (in the background when
# possible) and returns a reactive holding the results list. The list has
# data, plots, notes and settings (what produced it).
# =============================================================================

#' QC Drift Upload Server
#' @param id Shiny module session ID.
#' @param parent_session The host app session, used to switch sidebar tabs.
#' @return A reactive expression containing QC processing results.
qcdrift_upload_server <- function(id, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    state <- shiny::reactiveValues(res = NULL, settings = NULL, pending = NULL,
                                   error = NULL, status = "idle")
    kpis <- shiny::reactive(qcd_kpis(state$res))

    # ---- which file is selected (uploaded or the example) ---------------------
    chosen <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      f <- input$file
      chosen(list(path = f$datapath, name = f$name, size = f$size,
                  example = FALSE, md5 = unname(tools::md5sum(f$datapath))))
    })

    shiny::observeEvent(input$use_example, {
      ex <- system.file("extdata", "RawMassSpec.xlsx", package = "qcdrift")
      if (!nzchar(ex)) {
        shiny::showNotification(
          "The example file was not found in the installed qcdrift package.",
          type = "warning")
        return()
      }
      chosen(list(path = ex, name = "RawMassSpec.xlsx (example)",
                  size = file.size(ex), example = TRUE,
                  md5 = unname(tools::md5sum(ex))))
    })

    # ---- validation + preview ------------------------------------------------------
    raw   <- shiny::reactive({ shiny::req(chosen()); qcd_read_upload(chosen()$path) })
    check <- shiny::reactive(qcd_check_upload(raw(), trimws(input$prefix %||% "")))

    output$file_summary <- shiny::renderUI({
      ch <- chosen(); shiny::req(ch)
      shiny::tagList(
        shiny::h4("Selected file"),
        shiny::p(ch$name),
        shiny::p(paste("File size:", round(ch$size / 1024, 1), "KB"))
      )
    })

    output$validation <- shiny::renderUI({
      if (is.null(chosen())) {
        return(shiny::tags$span(class = "text-muted", "No file selected yet."))
      }
      ck <- check()
      shiny::tagList(
        if (length(ck$errors))
          qcd_callout("Problem with this file", ck$errors, status = "danger"),
        if (length(ck$warnings))
          qcd_callout("Worth a look", ck$warnings, status = "warning"),
        if (ck$ok)
          shiny::tags$p(class = "text-green", shiny::icon("check-circle"),
                        sprintf(" %s rows x %s columns read successfully.",
                                format(ck$n_rows, big.mark = ","),
                                format(ck$n_cols, big.mark = ",")))
      )
    })

    output$dims <- shiny::renderText({
      ch <- chosen(); shiny::req(ch)
      paste("File:", ch$name)
    })

    output$preview <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(chosen()),
                                  "Choose a file or try the example dataset to preview it."))
      df <- raw()
      # (need() evaluates its message eagerly, so only ask for it when df is an error)
      if (inherits(df, "error")) shiny::validate(conditionMessage(df))
      qcd_dt_view(df, page_len = 8)
    })

    # ---- background pipeline ----------------------------------------------------------------
    use_async <- qcd_use_async()
    if (use_async) qcd_setup_async()

    task <- shiny::ExtendedTask$new(function(path, norm, prefix, async) {
      if (async) {
        promises::future_promise(qcd_run_pipeline(path, norm, prefix))
      } else {
        promises::promise(function(resolve, reject) {
          tryCatch(resolve(qcd_run_pipeline(path, norm, prefix)), error = reject)
        })
      }
    })

    shiny::observeEvent(input$process, {
      ch <- chosen()
      if (is.null(ch)) {
        shiny::showNotification("Choose a file first.", type = "warning")
        return()
      }
      if (!check()$ok) {
        shiny::showNotification("Fix the problem shown under the file box first.", type = "error")
        return()
      }
      if (identical(task$status(), "running")) {
        shiny::showNotification("A run is already in progress.", type = "warning")
        return()
      }
      rq <- list(path = ch$path, name = ch$name, example = ch$example, md5 = ch$md5,
                 norm = input$norm, prefix = trimws(input$prefix %||% ""))
      state$pending <- rq
      state$error   <- NULL
      shiny::showNotification("Processing Mass Spec Data...", id = "qcd_running",
                              duration = NULL, type = "message")
      task$invoke(path = rq$path, norm = rq$norm, prefix = rq$prefix, async = use_async)
    })

    shiny::observeEvent(task$status(), {
      st <- task$status()
      state$status <- st
      if (identical(st, "running")) return()
      shiny::removeNotification("qcd_running")

      if (identical(st, "success")) {
        out <- task$result()
        out$settings <- state$pending      # what produced this result
        state$res      <- out
        state$settings <- state$pending

        miss <- names(which(!qcd_plots_found(out)))
        if (length(miss)) {
          shiny::showNotification(
            paste("Finished, but qcdrift did not return:",
                  paste(qcd_label_for(miss), collapse = ", ")),
            type = "warning", duration = 10)
        } else if (length(out$notes)) {
          shiny::showNotification("Processing finished with notes - see the status panel.",
                                  type = "warning", duration = 10)
        } else {
          shiny::showNotification("Processing completed successfully!", type = "message")
        }
        # The sidebar lives outside this module, so use the host session.
        if (!is.null(parent_session)) {
          shinydashboard::updateTabItems(parent_session, "main_tabs", selected = "qc_report")
        }
      }

      if (identical(st, "error")) {
        state$res   <- NULL
        state$error <- qcd_strip_ansi(tryCatch({ task$result(); "" },
                                               error = function(e) conditionMessage(e)))
        shiny::showNotification(
          paste("Processing failed:", strsplit(state$error, "\n", fixed = TRUE)[[1]][1],
                "- details are in the status panel."),
          type = "error", duration = NULL)
      }
    }, ignoreInit = TRUE)

    # ---- is the displayed result out of date? ----------------------------------------------
    stale <- shiny::reactive({
      s  <- state$settings
      ch <- chosen()
      if (is.null(s) || is.null(ch)) return(FALSE)
      cur <- list(md5 = ch$md5, norm = input$norm, prefix = trimws(input$prefix %||% ""))
      !identical(s[c("md5", "norm", "prefix")], cur)
    })

    # ---- status checklist ---------------------------------------------------------------------
    output$status <- shiny::renderUI({
      ch <- chosen()
      ck <- if (!is.null(ch)) check() else NULL
      has_file <- !is.null(ch)
      valid    <- !is.null(ck) && isTRUE(ck$ok)
      st       <- state$status
      done     <- !is.null(state$res)
      found    <- if (done) qcd_plots_found(state$res) else NULL

      shiny::tagList(
        shiny::tags$ul(
          class = "qcd-steps",
          qcd_status_step(has_file, "File selected", if (has_file) ch$name),
          qcd_status_step(valid, "File checks passed"),
          qcd_status_step(done, "Pipeline run",
                          if (identical(st, "running")) "running..."
                          else if (done) paste("normalization:", toupper(state$settings$norm))),
          qcd_status_step(done && all(found), "Figures generated",
                          if (done) sprintf("%d of %d available", sum(found), length(found)))
        ),
        if (!is.null(state$error))
          qcd_callout("Last run failed", shiny::tags$pre(state$error), status = "danger"),
        if (done && length(state$res$notes))
          qcd_callout("Notes from this run",
                      shiny::tags$ul(style = "margin-bottom:0;",
                                     lapply(state$res$notes, shiny::tags$li)),
                      status = "warning"),
        if (stale())
          qcd_callout("Settings changed",
                      "The file or settings differ from the last run. Click 'Process Data' to refresh.",
                      status = "warning")
      )
    })

    # ---- KPI boxes -------------------------------------------------------------------------------
    output$vb_samples  <- shinydashboard::renderValueBox(qcd_kpi_box(kpis()$samples,  "Samples",       "vial",  "primary"))
    output$vb_features <- shinydashboard::renderValueBox(qcd_kpi_box(kpis()$features, "Features",      "dna",   "info"))
    output$vb_qc       <- shinydashboard::renderValueBox(qcd_kpi_box(kpis()$qc,       "QC injections", "flask", "danger"))
    output$vb_norm     <- shinydashboard::renderValueBox({
      qcd_kpi_box(if (is.null(state$settings)) NA else toupper(state$settings$norm),
                  "Normalization", "balance-scale", "success")
    })

    shiny::reactive(state$res)
  })
}


# =============================================================================
# PUBLIC API 2/4 - qcdrift_report_ui()
# =============================================================================

#' QC Drift Report UI
#' @param id Shiny module session ID.
#' @return A Shiny tabItem UI.
qcdrift_report_ui <- function(id) {
  ns <- shiny::NS(id)

  panels <- c(
    list(shiny::tabPanel("Verdict", qcd_verdict_ui(ns("verdict")))),
    lapply(names(QCD_TAB_SPECS), function(k) {
      shiny::tabPanel(QCD_TAB_SPECS[[k]]$title, qcd_plot_tab_ui(ns(k), QCD_TAB_SPECS[[k]]))
    }),
    list(
      shiny::tabPanel("Drift Explorer", qcd_drift_ui(ns("drift"))),
      shiny::tabPanel("Data Tables",    qcd_data_ui(ns("data"))),
      shiny::tabPanel("Export",         qcd_export_ui(ns("export")))
    )
  )

  shinydashboard::tabItem(
    tabName = "qc_report",
    qcd_css_tag(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "QC Drift Correction Report", width = 12, status = "primary",
        shiny::fluidRow(
          shiny::column(5, shiny::uiOutput(ns("run_summary"))),
          shiny::column(7, shiny::sliderInput(ns("cv_thr"), "QC CV limit (%)",
                                              min = 5, max = 50,
                                              value = QCD_CV_DEFAULT, step = 1,
                                              width = "100%"))
        ),
        shiny::helpText("The limit drives the verdict, the CV curve, the 'Review' flags and the",
                        "exported list of features to review. qcdrift's own plots keep their",
                        "built-in 20% line.")
      )
    ),
    shiny::div(class = "qcd-tabs",
               do.call(shinydashboard::tabBox, c(list(id = ns("tabs"), width = 12), panels)))
  )
}

# =============================================================================
# PUBLIC API 4/4 - qcdrift_report_server()
# =============================================================================

#' QC Drift Report Server
#' @param id Shiny module session ID.
#' @param qc_results_reactive Reactive expression containing QC processing results
#'   (the value returned by \code{qcdrift_upload_server()}).
#' @return A Shiny module server result.
qcdrift_report_server <- function(id, qc_results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {

    res <- shiny::reactive(qc_results_reactive())
    thr <- shiny::reactive(input$cv_thr %||% QCD_CV_DEFAULT)

    # ---- shared derived values (computed once, used by several tabs) -------------
    kpis    <- shiny::reactive(qcd_kpis(res()))
    long    <- shiny::reactive({ shiny::req(res()); qcd_build_long(res()) })
    cv_tbl  <- shiny::reactive({ lg <- long(); if (is.null(lg)) NULL else qcd_cv_table(lg) })
    verdict <- shiny::reactive({ shiny::req(res()); qcd_verdict(cv_tbl(), thr()) })

    output$run_summary <- shiny::renderUI({
      r <- res()
      if (is.null(r)) {
        return(shiny::tags$span(class = "text-muted",
                                "No results yet - upload a file and click Process Data."))
      }
      shiny::tagList(
        shiny::tags$span(class = "label label-success", style = "font-size:13px;", "Results ready"),
        shiny::tags$span(class = "text-muted", style = "margin-left:8px;",
                         paste0(r$settings$name %||% "", " | ", toupper(r$settings$norm %||% "")))
      )
    })

    # a new result always opens on the Verdict tab
    shiny::observeEvent(res(), {
      shiny::updateTabsetPanel(session, "tabs", selected = "Verdict")
    })

    qcd_verdict_server("verdict", res, verdict, cv_tbl, thr)
    for (k in names(QCD_TAB_SPECS)) local({
      this <- k
      qcd_plot_tab_server(this, res, QCD_TAB_SPECS[[this]], long)
    })
    qcd_drift_server("drift", long, cv_tbl, thr)
    qcd_data_server("data", res)
    qcd_export_server("export", res, long, cv_tbl, verdict, thr, kpis)
  })
}