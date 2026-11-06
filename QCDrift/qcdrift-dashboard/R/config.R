# =============================================================================
# config.R - every constant the app uses lives here.
# To add a tab, add an entry to TAB_SPECS. To support a renamed qcdrift plot,
# add its name to PLOT_ALIASES.
# =============================================================================

APP_VERSION          <- "1.0.0"
CV_THRESHOLD_DEFAULT <- 20      # % - the usual acceptance limit for QC CV
VERDICT_PASS_SHARE   <- 80      # % of features that must be under the limit
                                # for a "successful" verdict (a heuristic)

# Okabe-Ito colour-blind-safe palette ----------------------------------------
PAL <- list(
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

MSG_NO_RESULT <- "Run the pipeline on the 'Upload & Run' tab to see this."

# Each dashboard slot can be filled by any one of several names found in
# results$plots; the first name present wins.
PLOT_ALIASES <- list(
  pca_raw            = c("pca_raw", "pca_before"),
  pca_corrected      = c("pca_corrected", "pca_after"),
  pca_pairs          = c("pca_pairs", "pca_matrix"),
  cv_raw             = c("cv_raw", "cv_before"),
  cv_corrected       = c("cv_corrected", "cv_after"),
  violin_raw         = c("violin_raw", "violin_before"),
  violin_corrected   = c("violin_corrected", "violin_after"),
  heatmap_raw        = c("heatmap_raw", "heatmap_before"),
  heatmap_corr       = c("heatmap_corr", "heatmap_corrected", "heatmap_after"),
  heatmap_comparison = c("heatmap_comparison", "heatmap_compare")
)

PLOT_LABELS <- c(
  pca_raw = "PCA - raw",            pca_corrected = "PCA - corrected",
  pca_pairs = "PCA pair matrix",
  cv_raw = "CV - raw",              cv_corrected = "CV - corrected",
  violin_raw = "Violin - raw",      violin_corrected = "Violin - corrected",
  heatmap_raw = "Heatmap - raw",    heatmap_corr = "Heatmap - corrected",
  heatmap_comparison = "Heatmap comparison"
)

# Evidence tabs are generated from this list (see mod_plot_tab.R).
# Card defaults: width = 6, height = 420, interactive = TRUE.
TAB_SPECS <- list(
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
      "Colour shows intensity across the injection order. Vertical colour",
      "'stripes' in the raw plot are the signature of drift; they should",
      "fade after correction."),
    cards = list(
      list(key = "heatmap_raw",  title = "Raw",
           height = 460, interactive = FALSE),
      list(key = "heatmap_corr", title = "Corrected",
           height = 460, interactive = FALSE),
      list(key = "heatmap_comparison", title = "Side-by-side comparison",
           width = 12, height = 520, interactive = FALSE)
    )
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
