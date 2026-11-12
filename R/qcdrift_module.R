#' qcdrift_module.R, updated w/ui elements and server calls

#' QC Drift Upload UI
#' @param id Shiny module session ID.
#' @return A Shiny tabItem UI.
qcdrift_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(tabName = "qc_upload",
    shiny::fluidRow(
      shinydashboard::box(
        title = "QC Drift Data Upload",
        width = 12,
        shiny::p(
          "Upload an Excel workbook containing your mass spectrometry data."
        ),
        shiny::fileInput(
          ns("file"),
          "Select Excel Workbook",
          accept = c(".xlsx", ".xls")
        ),
        shiny::uiOutput(ns("file_summary")),
        shiny::actionButton(ns("process"),
          "Process Data",
          icon = shiny::icon("play"),
          class = "btn-primary"
        ),
        shiny::br(),
        shiny::br(),
        shiny::textOutput(ns("status"))
      )
    )
  )
}


#' QC Drift Report UI
#' @param id Shiny module session ID.
#' @return A Shiny tabItem UI.
qcdrift_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = "qc_report",
    shiny::fluidRow(
      shinydashboard::box(
        title = "QC Drift Correction Report",
        width = 12,
        shiny::downloadButton(ns("download_report"),
          "Download PDF Report",
          class = "btn-success"
        )
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "PCA Comparison",
        width = 6,
        shiny::plotOutput(ns("pca_before"), height = "350px")
      ),
      shinydashboard::box(
        title = "Corrected PCA",
        width = 6,
        shiny::plotOutput(ns("pca_after"), height = "350px")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "CV Before Correction",
        width = 6,
        shiny::plotOutput(ns("cv_before"), height = "350px")
      ),
      shinydashboard::box(
        title = "CV After Correction",
        width = 6,
        shiny::plotOutput(ns("cv_after"), height = "350px")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Violin Plots Before Correction",
        width = 6,
        shiny::plotOutput(ns("violin_before"), height = "350px")
      ),
      shinydashboard::box(
        title = "Violin Plots After Correction",
        width = 6,
        shiny::plotOutput(ns("violin_after"), height = "350px")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Heatmap Before Correction",
        width = 6,
        shiny::plotOutput(ns("heatmap_before"), height = "350px")
      ),
      shinydashboard::box(
        title = "Heatmap After Correction",
        width = 6,
        shiny::plotOutput(ns("heatmap_after"), height = "350px")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "PCA Pairs",
        width = 6,
        shiny::plotOutput(ns("pairs_before"), height = "350px")
      ),
      shinydashboard::box(
        title = "Heatmap Comparison",
        width = 6,
        shiny::plotOutput(ns("pairs_after"), height = "350px")
      )
    )
  )
}


#' QC Drift Upload Server
#' @param id Shiny module session ID.
#' @return A reactive expression containing QC processing results.
qcdrift_upload_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    qc_results <- shiny::reactiveVal(NULL)
    qc_status <- shiny::reactiveVal(
      "Upload an Excel workbook to begin."
    )
    output$status <- shiny::renderText({
      qc_status()
    })
    output$file_summary <- shiny::renderUI({
      shiny::req(input$file)
      shiny::tagList(
        shiny::h4("Selected file"),
        shiny::p(input$file$name),
        shiny::p(
          paste(
            "File size:",
            round(input$file$size / 1024, 1),
            "KB"
          )
        )
      )
    })
    # Excel preview
    shiny::observeEvent(input$file, {
      preview <- tryCatch(
        readxl::read_excel(
          input$file$datapath,
          col_names = FALSE,
          n_max = 100
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Preview failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(preview)) return()
      output$preview_table <- DT::renderDT({
        DT::datatable(
          preview,
          options = list(scrollX = TRUE)
        )
      })
      shiny::showModal(
        shiny::modalDialog(
          title = "File Preview",
          DT::DTOutput(session$ns("preview_table")),
          size = "l",
          easyClose = TRUE
        )
      )
    })

    # Process the uploaded workbook
    shiny::observeEvent(input$process, {
      shiny::req(input$file)
      shiny::withProgress(
        message = "Processing Mass Spec Data...",
        value = 0, {
          shiny::incProgress(
            0.3,
            detail = "Reading Excel file..."
          )
          tryCatch({
            shiny::incProgress(
              0.6,
              detail = "Running qcdrift correction pipeline..."
            )
            result <- qcdrift::process_runs(
              input$file$datapath
            )
            qc_results(result)
            shiny::incProgress(
              1.0,
              detail = "Completed!"
            )
            qc_status(
              "Processing completed successfully!"
            )
            # Navigate to report tab
            # Use rootScope because the sidebar is outside this module's namespace
            shinydashboard::updateTabItems(
              parent_session,
              "main_tabs",
              selected = "qc_report"
            )
          }, error = function(e) {
            qc_results(NULL)
            qc_status(
              paste(
                "Processing failed:",
                conditionMessage(e)
              )
            )
            shiny::showNotification(
              paste("Error:", conditionMessage(e)),
              type = "error"
            )
          })
        }
      )
    })
    return(qc_results)
  })
}


#' QC Drift Report Server
#' @param id Shiny module session ID.
#' @param qc_results_reactive Reactive expression containing QC processing results.
#' @return A Shiny module server result.
qcdrift_report_server <- function(id, qc_results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    render_specific_plot <- function(plot_key) {
      shiny::renderPlot({
        res <- qc_results_reactive()
        shiny::req(res)
        plots_list <- if (!is.null(res$plots)) {
          res$plots
        } else {
          res
        }
        if (
          is.list(plots_list) &&
          plot_key %in% names(plots_list)
        ) {
          p <- plots_list[[plot_key]]
          if (
            inherits(p, "ggplot") ||
            inherits(p, "recordedplot") ||
            inherits(p, "gtable") ||
            inherits(p, "ggmatrix")
          ) {
            print(p)
          } else {
            graphics::plot.new()
            graphics::text(0.5, 0.5,
              paste("Object found for:", plot_key,
                "\n(Not a standard renderable plot)"
              )
            )
          }
        } else {
          graphics::plot.new()
          graphics::text(0.5, 0.5,
            paste("Plot", plot_key, "not found in results."),
            col = "firebrick"
          )
          graphics::box(
            col = "firebrick",
            lwd = 1.5
          )
        }
      })
    }

    # Map qcdrift output names to dashboard outputs
    output$pca_before <- render_specific_plot("pca_raw")
    output$pca_after <- render_specific_plot("pca_corrected")

    output$cv_before <- render_specific_plot("cv_raw")
    output$cv_after <- render_specific_plot("cv_corrected")

    output$violin_before <- render_specific_plot("violin_raw")
    output$violin_after <- render_specific_plot("violin_corrected")

    output$heatmap_before <- render_specific_plot("heatmap_raw")
    output$heatmap_after <- render_specific_plot("heatmap_corr")

    output$pairs_before <- render_specific_plot("pca_pairs")
    output$pairs_after <- render_specific_plot("heatmap_comparison")

    # PDF report download
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("Metabolomics_QC_Report_",
          Sys.Date(), ".pdf"
        )
      },
      content = function(file) {
        res <- qc_results_reactive()
        shiny::req(res)
        plots_list <- if (!is.null(res$plots)) {
          res$plots
        } else {
          res
        }
        grDevices::pdf(file,
          width = 11,
          height = 8.5,
          onefile = TRUE
        )
        on.exit(grDevices::dev.off())
        if (is.list(plots_list)) {
          for (p_name in names(plots_list)) {
            p <- plots_list[[p_name]]
            if (
              inherits(p, "ggplot") ||
              inherits(p, "recordedplot") ||
              inherits(p, "gtable") ||
              inherits(p, "ggmatrix")
            ) {
              print(p)
            }
          }
        }
      }
    )
  })
}
