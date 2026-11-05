# newer version w/qcdrift_ui and qcdrift_server in single fxn
# install qcdrift packages, if required
# install.packages(c("devtools", "dplyr", "ggplot2", "patchwork", "ggrepel", "GGally","readxl","tidyr"))
# qcdrift can be installed directly form GitHub with devtools
# devtools::install_github('Hood-BIFX/qcdrift')

# Server for the Upload Tab
qcdrift_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    qc_results <- shiny::reactiveVal(NULL)
    qc_status <- shiny::reactiveVal("Upload an Excel workbook to begin.")

    output$status <- shiny::renderText({ qc_status() })

    output$file_summary <- shiny::renderUI({
      req(input$file)
      shiny::tagList(
        shiny::h4("Selected file"),
        shiny::p(input$file$name),
        shiny::p(paste(
          "File size:",
          round(input$file$size / 1024, 1),
          "KB"
        ))
      )
    })

    shiny::observeEvent(input$process, {
      req(input$file)

      shiny::withProgress(message = 'Processing Mass Spec Data...', value = 0, {
        shiny::incProgress(0.3, detail = "Reading Excel file...")
        Sys.sleep(0.5) 
        
        tryCatch({
          shiny::incProgress(0.6, detail = "Running qcdrift correction pipeline...")
          
          # Run core qcdrift processing pipeline
          result <- qcdrift::process_runs(input$file$datapath)
          qc_results(result)
          
          shiny::incProgress(1.0, detail = "Completed!")
          qc_status("Processing completed successfully!")

          # Automatically switch sidebar to the report tab
          updateTabItems(session, "qc_tab", selected = "qc_report")

        }, error = function(e) {
          qc_results(NULL)
          qc_status(paste("Processing failed:", conditionMessage(e)))
          showNotification(paste("Error:", conditionMessage(e)), type = "error")
        })
      })
    })
    
    return(qc_results) 
  })
}

# Server for the Report Tab (Mapped to exact qcdrift plot names)
qcdrift_report_server <- function(id, qc_results_reactive) {
  moduleServer(id, function(input, output, session) {
    
    render_specific_plot <- function(plot_key) {
      shiny::renderPlot({
        res <- qc_results_reactive()
        req(res)
        
        plots_list <- if (!is.null(res$plots)) res$plots else res
        
        if (is.list(plots_list) && plot_key %in% names(plots_list)) {
          p <- plots_list[[plot_key]]
          if (inherits(p, "ggplot") || inherits(p, "recordedplot") || inherits(p, "gtable")) {
            print(p)
          } else {
            plot.new()
            text(0.5, 0.5, paste("Object found for:", plot_key, "\n(Not a standard renderable plot)"))
          }
        } else {
          plot.new()
          text(0.5, 0.5, paste("Plot '", plot_key, "' not found in results."), col = "firebrick", cex = 1.1)
          box(col = "firebrick", lwd = 1.5)
        }
      })
    }

    # Map the exact names returned by names(test_res$plots)
    output$pca_before    <- render_specific_plot("pca_raw")
    output$pca_after     <- render_specific_plot("pca_corrected")
    
    output$cv_before     <- render_specific_plot("cv_raw")
    output$cv_after      <- render_specific_plot("cv_corrected")
    
    output$violin_before <- render_specific_plot("violin_raw")
    output$violin_after  <- render_pop <- render_specific_plot("violin_corrected")
    
    output$heatmap_before<- render_specific_plot("heatmap_raw")
    output$heatmap_after <- render_specific_plot("heatmap_corr") # Note: named heatmap_corr in your list
    
    # Since pca_pairs and heatmap_comparison are single combined plots, map them cleanly
    output$pairs_before  <- render_specific_plot("pca_pairs")
    output$pairs_after   <- render_specific_plot("heatmap_comparison")

    # PDF Report Download Handler
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        paste0("Metabolomics_QC_Report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        res <- qc_results_reactive()
        req(res)

        grDevices::pdf(file, width = 11, height = 8.5)
        on.exit(grDevices::dev.off())

        plots_list <- if (!is.null(res$plots)) res$plots else res

        if (is.list(plots_list)) {
          for (p_name in names(plots_list)) {
            p <- plots_list[[p_name]]
            if (inherits(p, "ggplot") || inherits(p, "recordedplot")) {
              print(p)
            }
          }
        }
      }
    )
  })
}