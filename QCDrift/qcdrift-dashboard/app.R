###############################################################################
#  QCDrift Dashboard - app.R
#  Wires the modules in R/ into a bs4Dash interface around the qcdrift package.
#
#  Run with:  shiny::runApp()          (from this folder)
#
#  Flow:  Upload & Run -> Verdict -> Evidence (PCA, CV, heatmap, violin)
#         -> Drift Explorer -> Data tables -> Export & report
###############################################################################

# ---- 0. setup ---------------------------------------------------------------

options(shiny.autoload.r = FALSE,              # we source R/ ourselves, once
        shiny.maxRequestSize = 100 * 1024^2)   # allow uploads up to 100 MB

APP_DIR <- normalizePath(getwd())

required_pkgs <- c("shiny", "bs4Dash", "qcdrift", "plotly", "DT", "ggplot2",
                   "dplyr", "tidyr", "readxl", "writexl", "shinycssloaders",
                   "zip", "rmarkdown", "knitr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Missing R packages: ", paste(missing_pkgs, collapse = ", "), "\n",
       "Install CRAN packages with install.packages(); qcdrift with\n",
       "devtools::install_github('Hood-BIFX/qcdrift')", call. = FALSE)
}
if (utils::packageVersion("shiny") < "1.8.1") {
  stop("This app needs shiny >= 1.8.1 (for background tasks).", call. = FALSE)
}

suppressPackageStartupMessages({
  library(shiny); library(bs4Dash); library(plotly); library(DT)
  library(ggplot2); library(shinycssloaders)
})

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

# Run the pipeline in a background R process when `future` is installed, so the
# interface stays responsive during long runs. Falls back to in-process.
USE_ASYNC <- requireNamespace("future", quietly = TRUE) &&
             requireNamespace("promises", quietly = TRUE)
if (USE_ASYNC) future::plan(future::multisession, workers = 2)


# ---- 1. UI -------------------------------------------------------------------

menu_evidence <- lapply(names(TAB_SPECS), function(id) {
  menuItem(TAB_SPECS[[id]]$title, tabName = id, icon = icon(TAB_SPECS[[id]]$icon))
})

tabs_evidence <- lapply(names(TAB_SPECS), function(id) {
  tabItem(tabName = id, mod_plot_tab_ui(id, TAB_SPECS[[id]]))
})

sidebar_menu <- do.call(sidebarMenu, c(
  list(id = "tabs",
       sidebarHeader("Start"),
       menuItem("Upload & Run", tabName = "upload",  icon = icon("upload")),
       menuItem("Verdict",      tabName = "verdict", icon = icon("clipboard-check")),
       sidebarHeader("Evidence")),
  menu_evidence,
  list(sidebarHeader("Investigate"),
       menuItem("Drift Explorer", tabName = "drift", icon = icon("search")),
       menuItem("Data Tables",    tabName = "data",  icon = icon("table")),
       sidebarHeader("Deliver"),
       menuItem("Export & Report", tabName = "export", icon = icon("download")))
))

body_tabs <- do.call(tabItems, c(
  list(tabItem(tabName = "upload",  mod_upload_ui("upload")),
       tabItem(tabName = "verdict", mod_verdict_ui("verdict"))),
  tabs_evidence,
  list(tabItem(tabName = "drift",  mod_drift_ui("drift")),
       tabItem(tabName = "data",   mod_data_ui("data")),
       tabItem(tabName = "export", mod_export_ui("export")))
))

ui <- dashboardPage(
  title = "QCDrift Dashboard",
  freshTheme = app_theme(),
  dark = NULL, help = NULL, fullscreen = TRUE, scrollToTop = TRUE,

  header = dashboardHeader(
    title = dashboardBrand(title = "QCDrift", color = "primary"),
    skin = "light", border = TRUE,
    rightUi = tags$li(class = "nav-item dropdown", style = "padding:.5rem 1rem;",
                      uiOutput("header_status", inline = TRUE))
  ),

  sidebar = dashboardSidebar(skin = "light", status = "primary", elevation = 2, sidebar_menu),

  body = dashboardBody(tags$head(tags$style(HTML(app_css))), body_tabs),

  controlbar = dashboardControlbar(
    id = "controlbar", skin = "light", pinned = FALSE, overlay = TRUE,
    div(
      style = "padding:1rem;",
      h5("Settings"),
      sliderInput("cv_thr", "QC CV limit (%)", min = 5, max = 50,
                  value = CV_THRESHOLD_DEFAULT, step = 1),
      helpText("Used for the verdict, the CV curve, the 'Review' flag and the",
               "exported list of features to review. The qcdrift plots keep",
               "their own built-in 20% line.")
    )
  ),

  footer = dashboardFooter(
    left  = paste("QCDrift Dashboard", APP_VERSION),
    right = "Powered by the qcdrift R package"
  )
)


# ---- 2. server ---------------------------------------------------------------

server <- function(input, output, session) {

  # One place for everything that describes the current result.
  state <- reactiveValues(res = NULL, settings = NULL, pending = NULL,
                          error = NULL, status = "idle")

  res <- reactive(state$res)
  thr <- reactive(input$cv_thr %||% CV_THRESHOLD_DEFAULT)

  # ---- shared derived values (computed once, used by several modules) ----------
  kpis    <- reactive(compute_kpis(res()))
  long    <- reactive({ req(res()); build_drift_long(res()) })
  cv_tbl  <- reactive({ lg <- long(); if (is.null(lg)) NULL else qc_cv_table(lg) })
  verdict <- reactive({ req(res()); compute_verdict(cv_tbl(), thr()) })

  # ---- the pipeline runs as a background task ----------------------------------
  task <- ExtendedTask$new(function(path, norm, prefix, async) {
    if (async) {
      promises::future_promise(run_pipeline(path, norm, prefix))
    } else {
      promises::promise(function(resolve, reject) {
        tryCatch(resolve(run_pipeline(path, norm, prefix)), error = reject)
      })
    }
  })

  up <- mod_upload_server("upload", state, kpis)

  observeEvent(up$run(), {
    if (identical(task$status(), "running")) {
      showNotification("A run is already in progress.", type = "warning")
      return()
    }
    rq <- up$run()
    state$pending <- rq
    state$error   <- NULL
    showNotification("Running QCDrift...", id = "running", duration = NULL, type = "message")
    task$invoke(path = rq$path, norm = rq$norm, prefix = rq$prefix, async = USE_ASYNC)
  })

  observeEvent(task$status(), {
    st <- task$status()
    state$status <- st
    if (identical(st, "running")) return()
    removeNotification("running")

    if (identical(st, "success")) {
      out <- task$result()
      state$res      <- out
      state$settings <- state$pending

      miss <- names(which(!plots_found(out)))
      if (length(miss)) {
        showNotification(
          paste("Finished, but qcdrift did not return:", paste(label_for(miss), collapse = ", ")),
          type = "warning", duration = 10)
      } else {
        showNotification("QCDrift finished successfully.", type = "message")
      }
      updateTabItems(session, "tabs", "verdict")
    }

    if (identical(st, "error")) {
      state$res   <- NULL
      state$error <- tryCatch({ task$result(); "" }, error = function(e) conditionMessage(e))
      showNotification(paste("QCDrift failed:", state$error), type = "error", duration = NULL)
    }
  }, ignoreInit = TRUE)

  # ---- header badge ------------------------------------------------------------------
  output$header_status <- renderUI({
    if (identical(state$status, "running")) tags$span(class = "badge badge-info", "Running...")
    else if (!is.null(state$error))         tags$span(class = "badge badge-danger", "Error")
    else if (is.null(state$res))            tags$span(class = "badge badge-secondary", "No results")
    else if (up$stale())                    tags$span(class = "badge badge-warning", "Re-run needed")
    else                                    tags$span(class = "badge badge-success", "Results ready")
  })

  # ---- modules --------------------------------------------------------------------------
  mod_verdict_server("verdict", res, verdict, cv_tbl, thr)
  for (id in names(TAB_SPECS)) local({
    this <- id
    mod_plot_tab_server(this, res, TAB_SPECS[[this]])
  })
  mod_drift_server("drift", long, cv_tbl, thr)
  mod_data_server("data", res)
  mod_export_server("export", state, cv_tbl, verdict, thr, kpis, APP_DIR)
}

shinyApp(ui, server)
