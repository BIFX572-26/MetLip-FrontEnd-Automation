# =============================================================================
# mod_data_export.R - the data-table tab and the export tab.
# =============================================================================

## ---- data tables ---------------------------------------------------------------

mod_data_ui <- function(id) {
  ns <- NS(id)
  tabBox(
    id = ns("tabs"), width = 12, type = "tabs", status = "primary",
    tabPanel("Raw",       DTOutput(ns("raw"))),
    tabPanel("Corrected", DTOutput(ns("corrected"))),
    tabPanel("Scaled",    DTOutput(ns("scaled")))
  )
}

mod_data_server <- function(id, res) {
  moduleServer(id, function(input, output, session) {
    table_output <- function(which) {
      renderDT({
        tb <- get_tbl(res(), which)
        validate(need(!is.null(tb), MSG_NO_RESULT))
        dt_view(tb)
      })
    }
    output$raw       <- table_output("raw")
    output$corrected <- table_output("corrected")
    output$scaled    <- table_output("scaled")
  })
}

## ---- export ----------------------------------------------------------------------

mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      "!output.has_results", ns = ns,
      callout(title = "Nothing to export yet", status = "warning",
              "Run the pipeline on the 'Upload & Run' tab first.")
    ),
    conditionalPanel(
      "output.has_results", ns = ns,
      fluidRow(
        box(
          title = "Everything in one click", width = 12, status = "success",
          solidHeader = TRUE, headerBorder = FALSE,
          p("A single zip with the corrected dataset (Excel), all figures (PNG),",
            "the HTML QC report and a run-info file recording the settings,",
            "file checksum and package versions."),
          downloadButton(ns("dl_bundle"), "Download deliverables bundle (.zip)",
                         class = "btn-success")
        )
      ),
      fluidRow(
        box(
          title = "Figures", width = 4, status = "primary",
          solidHeader = TRUE, headerBorder = FALSE,
          radioButtons(ns("fig_fmt"), "Format", c("PNG" = "png", "PDF" = "pdf"), inline = TRUE),
          sliderInput(ns("fig_dpi"), "Resolution (PNG)", 72, 400, 150, step = 10),
          downloadButton(ns("dl_figs"), "All figures (.zip)", class = "btn-primary btn-block"),
          tags$hr(),
          selectInput(ns("single_fig"), "Single figure", choices = NULL),
          downloadButton(ns("dl_single"), "Download PNG", class = "btn-outline-primary btn-block")
        ),
        box(
          title = "Corrected dataset", width = 4, status = "success",
          solidHeader = TRUE, headerBorder = FALSE,
          p("Excel workbook with the corrected data, scaled data, features to",
            "review and run info - or the corrected table alone as CSV."),
          downloadButton(ns("dl_xlsx"), "Excel (.xlsx)", class = "btn-success btn-block"),
          tags$div(style = "height:.6rem;"),
          downloadButton(ns("dl_csv"), "Corrected CSV", class = "btn-outline-success btn-block")
        ),
        box(
          title = "QC report", width = 4, status = "info",
          solidHeader = TRUE, headerBorder = FALSE,
          p("Self-contained HTML report: verdict, run info, features to review",
            "and every diagnostic figure."),
          downloadButton(ns("dl_report"), "Generate report (.html)", class = "btn-info btn-block")
        )
      )
    )
  )
}

mod_export_server <- function(id, state, cv_tbl, verdict, thr, kpis, app_dir) {
  moduleServer(id, function(input, output, session) {

    output$has_results <- reactive(!is.null(state$res))
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    observeEvent(state$res, {
      figs <- Filter(is_plot_like, state$res$plots)
      updateSelectInput(session, "single_fig",
                        choices = stats::setNames(names(figs), label_for(names(figs))))
    })

    run_info <- reactive(make_run_info(state$settings, kpis(), thr()))
    review   <- reactive(review_table(cv_tbl(), thr()))
    stamp    <- function() format(Sys.Date(), "%Y%m%d")

    fail <- function(prefix, e) {
      showNotification(paste0(prefix, ": ", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    }

    output$dl_bundle <- downloadHandler(
      filename = function() paste0("QCDrift_deliverables_", stamp(), ".zip"),
      content = function(file) {
        req(state$res)
        withProgress(message = "Building deliverables...", value = 0.3, {
          failed <- build_bundle(state$res, run_info(), verdict(), review(),
                                 app_dir, file, dpi = input$fig_dpi)
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
        req(state$res)
        withProgress(message = "Saving figures...", value = 0.4, {
          dir   <- file.path(tempdir(), paste0("figs_", as.integer(Sys.time())))
          files <- save_plots(state$res$plots, dir, input$fig_fmt, input$fig_dpi)
          validate(need(length(files) > 0, "No figures could be saved."))
          zip::zipr(file, unname(files), mode = "cherry-pick")
        })
      }
    )

    output$dl_single <- downloadHandler(
      filename = function() paste0(input$single_fig, ".png"),
      content = function(file) {
        req(state$res, input$single_fig)
        save_one(state$res$plots[[input$single_fig]], file, dpi = input$fig_dpi)
      }
    )

    output$dl_xlsx <- downloadHandler(
      filename = function() paste0("QCDrift_corrected_", stamp(), ".xlsx"),
      content = function(file) {
        req(state$res)
        writexl::write_xlsx(workbook_sheets(state$res, run_info(), review()), file)
      }
    )

    output$dl_csv <- downloadHandler(
      filename = function() paste0("QCDrift_corrected_", stamp(), ".csv"),
      content = function(file) {
        req(state$res)
        utils::write.csv(get_tbl(state$res, "corrected"), file, row.names = FALSE)
      }
    )

    output$dl_report <- downloadHandler(
      filename = function() paste0("QCDrift_report_", stamp(), ".html"),
      content = function(file) {
        req(state$res)
        withProgress(message = "Building report...", value = 0.3, {
          ok <- tryCatch({
            render_qc_report(state$res, run_info(), verdict(), review(), app_dir, file)
            TRUE
          }, error = function(e) fail("Report failed", e))
          req(ok)
        })
      }
    )
  })
}
