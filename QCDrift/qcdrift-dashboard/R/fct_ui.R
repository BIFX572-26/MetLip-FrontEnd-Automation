# =============================================================================
# fct_ui.R - small UI helpers shared by several modules, plus the theme.
# =============================================================================

dt_view <- function(df, page_len = 10, selection = "none") {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  dt <- DT::datatable(
    df, rownames = FALSE, filter = "top", class = "compact stripe hover",
    selection = selection,
    options = list(scrollX = TRUE, pageLength = page_len, dom = "lfrtip")
  )
  if (length(num)) dt <- DT::formatSignif(dt, num, 4)
  dt
}

kpi_box <- function(value, subtitle, icon_name, color) {
  shown <- if (length(value) == 0 || is.na(value)) {
    "-"
  } else if (is.numeric(value)) {
    format(value, big.mark = ",")
  } else {
    value
  }
  valueBox(value = shown, subtitle = subtitle, icon = icon(icon_name), color = color)
}

fmt_pct <- function(x) {
  if (is.null(x) || length(x) == 0 || !is.finite(x)) "-" else sprintf("%.1f%%", x)
}

status_step <- function(done, label, detail = NULL) {
  tags$li(
    icon(if (done) "check-circle" else "circle",
         class = if (done) "text-success" else "text-muted"),
    tags$strong(label),
    if (!is.null(detail)) tags$span(class = "text-muted ml-2", detail)
  )
}

app_css <- "
  .card { border-radius: .6rem; box-shadow: 0 1px 3px rgba(0,0,0,.08); }
  .small-box { border-radius: .6rem; }
  .qc-steps { list-style: none; padding-left: 0; margin-bottom: 0; }
  .qc-steps li { padding: .35rem 0; border-bottom: 1px solid #eef0f2; }
  .qc-steps li:last-child { border-bottom: 0; }
  .btn-run { font-weight: 600; letter-spacing: .02em; }
  details summary { cursor: pointer; font-weight: 600; margin: .5rem 0; }
"

# Optional fresh theme; the app works (default look) if fresh is missing.
app_theme <- function() {
  if (!requireNamespace("fresh", quietly = TRUE)) return(NULL)
  tryCatch(
    fresh::create_theme(
      fresh::bs4dash_status(
        primary = PAL$primary, success = PAL$ok, info = PAL$info,
        warning = PAL$warn, danger = PAL$bad),
      fresh::bs4dash_layout(main_bg = "#f5f7fa")
    ),
    error = function(e) NULL
  )
}
