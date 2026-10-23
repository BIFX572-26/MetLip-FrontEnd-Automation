# install qcdrift packages, if required
# install.packages(c("devtools", "dplyr", "ggplot2", "patchwork", "ggrepel", "GGally","readxl","tidyr"))

# qcdrift can be installed directly form GitHub with devtools
# devtools::install_github('Hood-BIFX/qcdrift')

qcdrift_server <- function(input, output, session) {
  qc_results <- shiny::reactiveVal(NULL)
  qc_status <- shiny::reactiveVal("Upload an Excel workbook to begin.")

  output$qc_status <- shiny::renderText({qc_status()})

  output$qc_file_summary <- shiny::renderUI({req(input$qc_file)
    shiny::tagList(
      shiny::h4("Selected file"),
      shiny::p(input$qc_file$name),
      shiny::p(paste(
          "File size:",
          round(input$qc_file$size / 1024, 1),
          "KB"
        )
      )
    )
  })

  shiny::observeEvent(input$qc_process, {
    req(input$qc_file)

    # Clear previous results so an unsuccessful run cannot
    #  leave an older report looking like the latest result.
    qc_results(NULL)
    qc_status("Processing QC data...")

    tryCatch({result <- qcdrift::process_runs(
                      input$qc_file$datapath
      )
      qc_results(result)
      qc_status("Processing completed. Open the QC Report tab.")

    }, error = function(e) {
      qc_results(NULL)
      qc_status(
        paste("Processing failed:", conditionMessage(e))
      )
    })
  })

  # Temporary plotting scaffold.
  # Replace each placeholder with the appropriate qcdrift
  #  plotting function and the corresponding result object.
  qc_placeholder <- function(label) {
    graphics::plot.new()
    graphics::text(0.5, 0.5,
                   label,
                   cex = 1.2
    )
  }

  output$qc_pca <- shiny::renderPlot({
    req(qc_results())
    qc_placeholder("PCA plot: connect actual QC plotting function")
  })

  output$qc_cv <- shiny::renderPlot({
    req(qc_results())
    qc_placeholder("CV precision: connect actual QC plotting function")
  })

  output$qc_violin <- shiny::renderPlot({
    req(qc_results())
    qc_placeholder("QC distributions: connect actual QC plotting function")
  })

  output$qc_heatmap <- shiny::renderPlot({
    req(qc_results())
    qc_placeholder("QC heatmap: connect actual QC plotting function")
  })

  output$qc_download_report <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "QC_Report_",
        Sys.Date(),
        ".pdf"
      )
    },

    content = function(file) {
      req(qc_results())

      # Temporary PDF scaffold. Replace the four placeholder
      # pages with the actual plots and report content.
      grDevices::pdf(file,
                     width = 11,
                     height = 8.5
      )

      on.exit(grDevices::dev.off())

      plot_titles <- c("Principal Component Analysis",
                       "CV Precision",
                       "QC Distributions",
                       "QC Heatmap"
      )

      for (plot_title in plot_titles) {
        graphics::plot.new()
        graphics::title(
          main = paste("QC Report:", plot_title)
        )
        graphics::text(0.5, 0.5,
          "Replace with the corresponding QC plot."
        )
      }
    }
  )
}
