# =============================================================================
# fct_export.R - everything that writes files: figures, workbook, report,
# and the one-click deliverables bundle.
# =============================================================================

## ---- figures ---------------------------------------------------------------

save_one <- function(p, file, dpi = 150, width = 9, height = 6.5) {
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
save_plots <- function(plots, dir, fmt = "png", dpi = 150) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  keep  <- Filter(is_plot_like, plots)
  paths <- vapply(names(keep), function(nm) {
    f <- file.path(dir, paste0(nm, ".", fmt))
    if (save_one(keep[[nm]], f, dpi)) f else NA_character_
  }, character(1))
  paths[!is.na(paths)]
}

## ---- reproducibility record --------------------------------------------------

make_run_info <- function(settings, kpis, thr) {
  pv  <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                              error = function(e) "n/a")
  fmt <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "n/a" else as.character(x)

  data.frame(
    Item = c("Input file", "Input file MD5", "Normalization", "QC prefix",
             "CV threshold (%)", "Samples", "Features", "QC injections",
             "Dashboard version", "qcdrift version", "R version", "Generated"),
    Value = c(fmt(settings$name), fmt(settings$md5), toupper(fmt(settings$norm)),
              fmt(settings$prefix), fmt(thr), fmt(kpis$samples),
              fmt(kpis$features), fmt(kpis$qc), APP_VERSION, pv("qcdrift"),
              paste(R.version$major, R.version$minor, sep = "."),
              format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    stringsAsFactors = FALSE
  )
}

## ---- dataset -----------------------------------------------------------------

workbook_sheets <- function(res, run_info, review) {
  sheets <- list(
    Corrected          = get_tbl(res, "corrected"),
    Scaled             = get_tbl(res, "scaled"),
    Features_to_review = if (!is.null(review)) review[review$Review == "Yes", , drop = FALSE],
    Run_info           = run_info
  )
  Filter(Negate(is.null), sheets)
}

## ---- HTML report ---------------------------------------------------------------

render_qc_report <- function(res, run_info, verdict, review, app_dir, out_file, dpi = 150) {
  work <- file.path(tempdir(), paste0("report_", as.integer(Sys.time()), "_", sample.int(1e5, 1)))
  dir.create(work, recursive = TRUE)

  figs_abs <- save_plots(res$plots, file.path(work, "figs"), "png", dpi)
  figs <- as.list(file.path("figs", basename(figs_abs)))
  names(figs) <- label_for(tools::file_path_sans_ext(basename(figs_abs)))

  template <- file.path(app_dir, "report", "report.Rmd")
  if (!file.exists(template)) stop("Report template not found: ", template)
  file.copy(template, file.path(work, "report.Rmd"))

  review_flagged <- if (!is.null(review)) utils::head(review[review$Review == "Yes", , drop = FALSE], 25)

  out <- rmarkdown::render(
    file.path(work, "report.Rmd"),
    output_file = "QCDrift_report.html", output_dir = work,
    params = list(meta = run_info, verdict = verdict, figs = figs,
                  review = review_flagged),
    envir = new.env(parent = globalenv()), quiet = TRUE
  )
  file.copy(out, out_file, overwrite = TRUE)
  invisible(out_file)
}

## ---- deliverables bundle ---------------------------------------------------------

# Corrected dataset + figures + report + run info, zipped.
# Returns the names of the parts that could not be built.
build_bundle <- function(res, run_info, verdict, review, app_dir, zip_file, dpi = 150) {
  dir <- file.path(tempdir(), paste0("bundle_", as.integer(Sys.time())))
  dir.create(dir, recursive = TRUE)
  failed <- character()

  tryCatch(
    writexl::write_xlsx(workbook_sheets(res, run_info, review),
                        file.path(dir, "corrected_dataset.xlsx")),
    error = function(e) failed <<- c(failed, "dataset"))

  tryCatch(save_plots(res$plots, file.path(dir, "figures"), "png", dpi),
           error = function(e) failed <<- c(failed, "figures"))

  utils::write.csv(run_info, file.path(dir, "run_info.csv"), row.names = FALSE)

  tryCatch(
    render_qc_report(res, run_info, verdict, review, app_dir,
                     file.path(dir, "QCDrift_report.html"), dpi),
    error = function(e) failed <<- c(failed, "report"))

  zip::zip(zip_file, files = list.files(dir), root = dir)
  failed
}
