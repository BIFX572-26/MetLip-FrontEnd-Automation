############################################################
# app.R
# Mass Spectrometry QC Dashboard
# Upload Excel -> Normalize -> PCA -> QC Metrics -> Reports
############################################################

#############################
# Load Packages
#############################

library(shiny)          # Shiny web application framework
library(readxl)         # Read Excel files
library(dplyr)          # Data manipulation
library(tidyr)          # Data reshaping
library(ggplot2)        # Plotting
library(FactoMineR)     # PCA calculations
library(factoextra)     # PCA visualization
library(pheatmap)       # Heatmap generation
library(tibble)         # Data frame utilities
library(writexl)        # Excel exportlibrary()
library(qcdrift)        #QCdrift Package from Hood/QcDrift
############################################################
# UI
############################################################
ui <- fluidPage(
  titlePanel("QCDrift Correction Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_raw", "Upload Excel File", accept = ".xlsx"),
      actionButton("process_runs", "Process Data"),
      hr(),
      downloadButton("download_csv", "Download CSV"),
      br(), br(),
      downloadButton("download_excel", "Download Excel"),
      br(), br(),
      downloadButton("download_report", "Download QC Report"),
      hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Data", tableOutput("raw_table")),
        tabPanel("Normalized Data", tableOutput("normalized_table")),
        tabPanel("PCA", plotOutput("pca_plot", height = "600px")),
        tabPanel("CV Plot", plotOutput("cv_plot", height = "600px")),
        tabPanel("Heatmap", plotOutput("heatmap_plot", height = "700px")),
        tabPanel("Violin Plot", plotOutput("violin_plot", height = "600px"))
      )
    )
  )
)

############################################################
# SERVER
############################################################
server <- function(input, output, session) {

  raw_data <- reactive({
    req(input$file_raw)
    df <- read_excel(input$file_raw$datapath) |> 
    required_cols <- c("sample_id", "injection_order", "feature_id", "intensity")
    missing_cols <- setdiff(required_cols, names(df))
    
    validate(
      need(
        length(missing_cols) == 0,
        paste("Missing required columns:", paste(missing_cols, collapse = ", "))
      )
    )
    df
  })

  processed <- eventReactive(input$process_runs, {
    df <- raw_data() %>%
      mutate(
        sample_id = as.character(sample_id),
        feature_id = as.character(feature_id),
        intensity = as.numeric(intensity)
      )

    corrected_df <- df %>% mutate(intensity_corr = intensity)

    tsn_df <- corrected_df %>%
      group_by(sample_id) %>%
      mutate(
        total_intensity = sum(intensity_corr, na.rm = TRUE),
        intensity_tsn = intensity_corr / total_intensity
      ) %>%
      ungroup()

    scaled_df <- tsn_df %>%
      group_by(feature_id) %>%
      mutate(
        intensity_z = {
          sd_val <- sd(intensity_tsn, na.rm = TRUE)
          if (is.na(sd_val) || sd_val == 0) rep(0, n()) else as.numeric(scale(intensity_tsn))
        }
      ) %>%
      ungroup()

    list(raw = df, tsn = tsn_df, scaled = scaled_df)
  })

  output$status <- renderText({
    req(processed())
    paste0(
      "Samples: ", length(unique(processed()$raw$sample_id)),
      "\nFeatures: ", length(unique(processed()$raw$feature_id)),
      "\nNormalization: TSN + Z-score"
    )
  })

  output$raw_table <- renderTable({
    req(processed())
    head(processed()$raw, 100)
  })

  output$normalized_table <- renderTable({
    req(processed())
    head(processed()$scaled, 100)
  })

  output$pca_plot <- renderPlot({
    req(processed())
    pca_df <- processed()$scaled %>%
      select(sample_id, feature_id, intensity_z) %>%
      pivot_wider(names_from = feature_id, values_from = intensity_z, values_fn = mean)

    sample_ids <- pca_df$sample_id
    pca_mat <- as.matrix(select(pca_df, -sample_id))
    rownames(pca_mat) <- sample_ids
    pca_mat[is.na(pca_mat)] <- 0

    # Remove zero-variance features to prevent PCA crash
    pca_mat <- pca_mat[, apply(pca_mat, 2, var, na.rm = TRUE) > 0, drop = FALSE]

    pca_res <- FactoMineR::PCA(pca_mat, graph = FALSE)
    factoextra::fviz_pca_ind(pca_res, geom = "point") + ggtitle("PCA Score Plot")
  })

  output$cv_plot <- renderPlot({
    req(processed())
    cv_stats <- processed()$tsn %>%
      group_by(feature_id) %>%
      summarise(
        Mean = mean(intensity_tsn, na.rm = TRUE),
        SD = sd(intensity_tsn, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CV = 100 * SD / Mean)

    ggplot(cv_stats, aes(x = CV)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      theme_bw() +
      labs(title = "CV Distribution", x = "Coefficient of Variation (%)", y = "Count")
  })

  output$heatmap_plot <- renderPlot({
    req(processed())
    heat_df <- processed()$scaled %>%
      select(sample_id, feature_id, intensity_z) %>%
      pivot_wider(names_from = feature_id, values_from = intensity_z, values_fn = mean)

    sample_ids <- heat_df$sample_id
    heat_mat <- as.matrix(select(heat_df, -sample_id))
    rownames(heat_mat) <- sample_ids
    heat_mat[is.na(heat_mat)] <- 0

    pheatmap(heat_mat, main = "Feature Intensity Heatmap")
  })

  output$violin_plot <- renderPlot({
    req(processed())
    ggplot(processed()$scaled, aes(x = sample_id, y = intensity_z)) +
      geom_violin(fill = "lightblue") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Sample Intensity Distributions", x = "Sample ID", y = "Z-Score Intensity")
  })

  output$download_csv <- downloadHandler(
    filename = function() { "normalized_data.csv" },
    content = function(file) {
      write.csv(processed()$scaled, file, row.names = FALSE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() { "normalized_data.xlsx" },
    content = function(file) {
      write_xlsx(processed()$scaled, file)
    }
  )

  output$download_report <- downloadHandler(
    filename = function() { "qc_summary.txt" },
    content = function(file) {
      writeLines(c("QC Report", paste0("Generated on: ", Sys.time())), file)
    }
  )
}

shinyApp(ui = ui, server = server)