# =============================================================================
# fct_metrics.R - turns qcdrift results into the numbers users care about.
# =============================================================================

compute_kpis <- function(res) {
  out <- list(samples = NA_integer_, features = NA_integer_, qc = NA_integer_)
  raw <- get_tbl(res, "raw")
  if (is.null(raw)) return(out)

  sc <- pick_col(raw, c("sample", "name", "sample_name"))
  mc <- pick_col(raw, c("metabolites", "metabolite", "feature"))
  qc <- pick_col(raw, c("qc", "is_qc"))

  if (!is.na(sc)) {
    out$samples <- length(unique(raw[[sc]]))
    out$qc <- if (!is.na(qc)) {
      length(unique(raw[[sc]][as_qc_flag(raw[[qc]])]))
    } else {
      sum(grepl("^QC", unique(raw[[sc]]), ignore.case = TRUE))
    }
  }
  if (!is.na(mc)) out$features <- length(unique(raw[[mc]]))
  out
}

# One-glance answer to "did the correction work?".
# Rules (documented in README):
#   success : corrected median QC CV <= threshold, at least VERDICT_PASS_SHARE %
#             of features under the threshold, and better than raw
#   danger  : corrected median still above threshold and no better than raw
#   warning : everything in between
compute_verdict <- function(cv, thr = CV_THRESHOLD_DEFAULT) {
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
  } else if (med_c <= thr && ok_c >= VERDICT_PASS_SHARE && med_c < med_r) {
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
review_table <- function(cv, thr = CV_THRESHOLD_DEFAULT) {
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
