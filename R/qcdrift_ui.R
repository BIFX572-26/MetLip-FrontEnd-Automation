# functions for qc section ui on the dashboard, made in separate file for testing and
#  to keep modularity of the dashboard structure.

qc_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  # Use ns() for every input/output ID
  shiny::tagList(
    shiny::h2("QC Drift Data Upload"),
    shiny::fileInput(
      ns("file"),
      "Upload Mass Spectrometry Excel File",
      accept = ".xlsx"
    ),
    shiny::actionButton(
      ns("process"),
      "Process Drift Data",
      icon = shiny::icon("play"),
      class = "btn-success"
    ),
    shiny::verbatimTextOutput(ns("status")),
    shiny::uiOutput(ns("file_summary"))
  )
}


qc_report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h2("QC Drift Report"),
    shiny::plotOutput(ns("pca"), height = "300px"),
    shiny::plotOutput(ns("cv"), height = "300px"),
    shiny::plotOutput(ns("violin"), height = "300px"),
    shiny::plotOutput(ns("heatmap"), height = "300px"),
    shiny::downloadButton(
      ns("report"),
      "Download Full QC Report"
    )
  )
}
