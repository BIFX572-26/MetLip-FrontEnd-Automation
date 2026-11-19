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
        shiny::radioButtons(
          inputId = ns("upload_type"),
          label = "Select Input Data Format",
          choices = c(
            "Excel Workbook (.xlsx / .xls)" = "excel",
            "CSV Pair (rawdata.csv + metadata.csv)" = "csv"
          ),
          selected = "excel",
          inline = TRUE
        ),
        shiny::uiOutput(ns("upload_inputs")),
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
        shiny::radioButtons(
          inputId = ns("report_format"),
          label = "Report Format",
          choices = c("Wide" = "wide", "Long" = "long"),
          selected = "wide",
          inline = TRUE
        ),
        shiny::downloadButton(
          ns("download_report"),
          "Download Report",
          class = "btn-success"
        ),
        shiny::br(),
        shiny::br(),
        shiny::textOutput(ns("selected_format_text"))
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
      "Choose an upload format and select your file(s) to begin."
    )

    output$status <- shiny::renderText({
      qc_status()
    })

    # Dynamic file inputs based on selected upload type
    output$upload_inputs <- shiny::renderUI({
      upload_mode <- input$upload_type %||% "excel"

      if (identical(upload_mode, "csv")) {
        shiny::tagList(
          shiny::p("Upload both rawdata.csv and metadata.csv."),
          shiny::fileInput(
            session$ns("raw_csv"),
            "Select Raw Data CSV (rawdata.csv)",
            accept = c(".csv", "text/csv")
          ),
          shiny::fileInput(
            session$ns("meta_csv"),
            "Select Metadata CSV (metadata.csv)",
            accept = c(".csv", "text/csv")
          )
        )
      } else {
        shiny::tagList(
          shiny::p(
            "Upload an Excel workbook containing your mass spectrometry data."
          ),
          shiny::fileInput(
            session$ns("file"),
            "Select Excel Workbook",
            accept = c(".xlsx", ".xls")
          )
        )
      }
    })

    # File summary for either Excel or CSV pair
    output$file_summary <- shiny::renderUI({
      upload_mode <- input$upload_type %||% "excel"

      if (identical(upload_mode, "csv")) {
        shiny::req(any(!is.null(input$raw_csv), !is.null(input$meta_csv)))
        items <- list()
        if (!is.null(input$raw_csv)) {
          items <- c(items, list(shiny::tags$li(
            paste0(
              "Raw data: ", input$raw_csv$name,
              " (", round(input$raw_csv$size / 1024, 1), " KB)"
            )
          )))
        }
        if (!is.null(input$meta_csv)) {
          items <- c(items, list(shiny::tags$li(
            paste0(
              "Metadata: ", input$meta_csv$name,
              " (", round(input$meta_csv$size / 1024, 1), " KB)"
            )
          )))
        }
        shiny::tagList(
          shiny::h4("Selected files"),
          shiny::tags$ul(items)
        )
      } else {
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
      }
    })

    show_data_preview <- function(df, modal_title) {
      output$preview_table <- DT::renderDT({
        DT::datatable(
          df,
          options = list(scrollX = TRUE)
        )
      })
      shiny::showModal(
        shiny::modalDialog(
          title = modal_title,
          DT::DTOutput(session$ns("preview_table")),
          size = "l",
          easyClose = TRUE
        )
      )
    }

    # Excel preview
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
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
      show_data_preview(preview, paste("File Preview:", input$file$name))
    })

    # CSV previews
    shiny::observeEvent(input$raw_csv, {
      shiny::req(input$raw_csv)
      preview <- tryCatch(
        utils::read.csv(input$raw_csv$datapath, nrows = 100, check.names = FALSE),
        error = function(e) {
          shiny::showNotification(
            paste("Raw CSV preview failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(preview)) return()
      show_data_preview(preview, paste("Preview:", input$raw_csv$name))
    })

    shiny::observeEvent(input$meta_csv, {
      shiny::req(input$meta_csv)
      preview <- tryCatch(
        utils::read.csv(input$meta_csv$datapath, nrows = 100, check.names = FALSE),
        error = function(e) {
          shiny::showNotification(
            paste("Metadata CSV preview failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(preview)) return()
      show_data_preview(preview, paste("Preview:", input$meta_csv$name))
    })

    # Process the uploaded workbook or CSV pair
    shiny::observeEvent(input$process, {
      upload_mode <- input$upload_type %||% "excel"

      if (identical(upload_mode, "csv")) {
        if (is.null(input$raw_csv) || is.null(input$meta_csv)) {
          shiny::showNotification(
            "Please select both rawdata.csv and metadata.csv before processing.",
            type = "warning"
          )
          return()
        }
      } else {
        shiny::req(input$file)
      }

      shiny::withProgress(
        message = "Processing Mass Spec Data...",
        value = 0, {
          shiny::incProgress(
            0.3,
            detail = if (identical(upload_mode, "csv")) {
              "Reading rawdata.csv and metadata.csv..."
            } else {
              "Reading Excel file..."
            }
          )
          tryCatch({
            shiny::incProgress(
              0.6,
              detail = "Running qcdrift correction pipeline..."
            )

            result <- if (identical(upload_mode, "csv")) {
              raw_df <- utils::read.csv(
                input$raw_csv$datapath,
                check.names = FALSE
              )
              meta_df <- utils::read.csv(
                input$meta_csv$datapath,
                check.names = FALSE
              )

              # Bundle both CSVs into a temporary Excel workbook so
              # qcdrift::process_runs(filepath = ...) can read them as sheets
              tmp_xlsx <- tempfile(fileext = ".xlsx")
              writexl::write_xlsx(
                list(
                  rawdata = raw_df,
                  metadata = meta_df
                ),
                path = tmp_xlsx
              )
              on.exit(unlink(tmp_xlsx), add = TRUE)

              qcdrift::process_runs(
                filepath = tmp_xlsx,
                sheet_name = 1
              )
            } else {
              qcdrift::process_runs(
                filepath = input$file$datapath
              )
            }

            qc_results(result)
            shiny::incProgress(
              1.0,
              detail = "Completed!"
            )
            qc_status(
              "Processing completed successfully!"
            )
            # Navigate to report tab
            # Use parent_session because the sidebar is outside this module's namespace
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
    output$selected_format_text <- shiny::renderText({
      fmt <- input$report_format
      if (is.null(fmt) || !nzchar(fmt)) fmt <- "wide"
      paste("Current format choice:", toupper(fmt))
    })

    output$download_report <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$report_format
        if (is.null(fmt) || !nzchar(fmt)) fmt <- "wide"
        paste0("Metabolomics_QC_Report_", fmt, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        res <- qc_results_reactive()
        shiny::req(res)

        fmt <- input$report_format
        if (is.null(fmt) || !nzchar(fmt)) fmt <- "wide"
        fmt_upper <- toupper(fmt)

        plots_list <- if (!is.null(res$plots)) res$plots else res
        shiny::req(is.list(plots_list))

        if (identical(fmt, "long")) {
          grDevices::pdf(file, width = 8.5, height = 11, onefile = TRUE)
        } else {
          grDevices::pdf(file, width = 13, height = 8.5, onefile = TRUE)
        }
        on.exit(grDevices::dev.off(), add = TRUE)

        # First page explicitly shows the chosen format
        graphics::plot.new()
        graphics::text(0.5, 0.58, "Metabolomics QC Drift Report", cex = 2, font = 2)
        graphics::text(0.5, 0.45, paste("Format:", fmt_upper), cex = 1.6, font = 2)

        for (p_name in names(plots_list)) {
          p <- plots_list[[p_name]]
          if (
            inherits(p, "ggplot") ||
            inherits(p, "recordedplot") ||
            inherits(p, "gtable") ||
            inherits(p, "ggmatrix")
          ) {
            if (inherits(p, "ggplot")) {
              p <- p + ggplot2::labs(subtitle = paste("Format:", fmt_upper))
            }
            print(p)
          }
        }
      }
    )
  })
}
