# QCDrift Dashboard

A bs4Dash app around the [`qcdrift`](https://github.com/Hood-BIFX/qcdrift) package for metabolomics QC drift correction.

## Run it

```r
install.packages(c("shiny", "bs4Dash", "plotly", "DT", "ggplot2", "dplyr", "tidyr",
                   "readxl", "writexl", "shinycssloaders", "zip", "rmarkdown",
                   "knitr", "future", "fresh", "testthat"))
devtools::install_github("Hood-BIFX/qcdrift")

shiny::runApp()   # from this folder
```

`future` (background runs) and `fresh` (theme) are optional; the app works without them. Report generation needs Pandoc (bundled with RStudio). Shiny >= 1.8.1 is required.

## Layout

| Path | Role |
|---|---|
| `app.R` | Page layout, shared reactives, background pipeline task, module wiring |
| `R/config.R` | All constants: palette, plot-name aliases, tab specs, thresholds |
| `R/fct_data.R` | Upload checks, pipeline call, long format, QC CV (pure functions) |
| `R/fct_metrics.R` | KPIs, verdict rules, features-to-review table |
| `R/fct_export.R` | Figures, workbook, report, deliverables bundle, run info |
| `R/fct_ui.R` | Shared UI helpers and theme |
| `R/mod_*.R` | One Shiny module per screen |
| `report/report.Rmd` | HTML report template |
| `tests/testthat/` | Unit tests for the pure functions |

Add an evidence tab by adding an entry to `TAB_SPECS` in `R/config.R`; the menu, page and server wiring are generated from it.

## Verdict rules

Computed from the QC coefficient of variation (CV) per feature, using the limit in the settings panel (default 20%):

- **Success** - corrected median CV is under the limit, at least 80% of features are under it, and it beats the raw median.
- **Danger** - corrected median is above the limit and no better than raw.
- **Warning** - anything in between.

The 80% share is a pragmatic heuristic (`VERDICT_PASS_SHARE` in `config.R`), not a published standard.

## Assumptions to verify on first run

1. `process_runs()` returns `data$raw`, `data$corrected`, `data$scaled` and `plots$<name>` (as in the original script). Differently named plots: edit `PLOT_ALIASES`.
2. The result tables have injection order (`io`), a metabolite column, an abundance column and a QC flag (as described in the qcdrift README). If not, the verdict and drift explorer degrade gracefully and say so.
3. The example button uses `inst/extdata/RawMassSpec.xlsx` from the installed qcdrift package.

## Tests

```r
testthat::test_dir("tests/testthat")
```

## Not included

Linked brushing between plots, saved session recipes, a cancel button for running jobs, `shinytest2` UI tests and CI.
