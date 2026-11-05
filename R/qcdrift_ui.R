# functions for qc section ui on the dashboard

qc_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h2("QC Drift Data Upload"),
    shiny::fileInput(
      ns("file"), # Matches input$file in server
      "Upload Mass Spectrometry Excel File",
      accept = ".xlsx"
    ),
    shiny::actionButton(
      ns("process"), # Matches input$process in server
      "Process Drift Data",
      icon = shiny::icon("play"),
      class = "btn-success"
    ),
    shiny::verbatimTextOutput(ns("status")), # Matches output$status in server
    shiny::uiOutput(ns("file_summary"))      # Matches output$file_summary in server
  )
}


qc_report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h2("QC Drift Report (Before vs. After Correction)"),
    shiny::downloadButton(
      ns("download_report"), 
      "Download Full QC Report (PDF)",
      class = "btn-primary",
      style = "margin-bottom: 20px;"
    ),
    
    # 1. PCA Comparison
    shiny::fluidRow(
      shiny::column(6, shiny::h4("PCA: Before"), shiny::plotOutput(ns("pca_before"), height = "300px")),
      shiny::column(6, shiny::h4("PCA: After"), shiny::plotOutput(ns("pca_after"), height = "300px"))
    ),
    
    # 2. CV Precision Comparison
    shiny::fluidRow(
      shiny::column(6, shiny::h4("CV Precision: Before"), shiny::plotOutput(ns("cv_before"), height = "300px")),
      shiny::column(6, shiny::h4("CV Precision: After"), shiny::plotOutput(ns("cv_after"), height = "300px"))
    ),
    
    # 3. Violin Distribution Comparison
    shiny::fluidRow(
      shiny::column(6, shiny::h4("Violin Distributions: Before"), shiny::plotOutput(ns("violin_before"), height = "300px")),
      shiny::column(6, shiny::h4("Violin Distributions: After"), shiny::plotOutput(ns("violin_after"), height = "300px"))
    ),
    
    # 4. Heatmap Comparison
    shiny::fluidRow(
      shiny::column(6, shiny::h4("QC Heatmap: Before"), shiny::plotOutput(ns("heatmap_before"), height = "300px")),
      shiny::column(6, shiny::h4("QC Heatmap: After"), shiny::plotOutput(ns("heatmap_after"), height = "300px"))
    ),
    
    # 5. PCA Pairs Comparison
    shiny::fluidRow(
      shiny::column(6, shiny::h4("PCA Pairs: Before"), shiny::plotOutput(ns("pairs_before"), height = "350px")),
      shiny::column(6, shiny::h4("PCA Pairs: After"), shiny::plotOutput(ns("pairs_after"), height = "350px"))
    )
  )
}