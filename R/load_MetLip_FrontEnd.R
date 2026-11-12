#' Load MetLip Front-End Shiny dashboard
#' 
#' @export
#' @importFrom shiny observeEvent
load_MetLip_FrontEnd <- function() {
  # Enable modules, constuct dashboard UI's
  sortable::enable_modules()
  ui <- dashboard_ui(
    tas_ui = tas_accelerator_ui(
      "tas_accelerator"
    ),

    sequence_ui = sequence_generator_ui(
      "sequence_generator"
    ),

    qc_upload_ui = qcdrift_upload_ui(
      "qc_upload"
    ),

    qc_report_ui = qcdrift_report_ui(
      "qc_report"
    )
  )

  # Initialize dashboard servers
  server <- function(input, output, session) {
    app_state <- shiny::reactiveValues(
      request_id = NULL,
      df_submitted = NULL,
      df_generated = NULL,
      df_confirmed = NULL,
      submitted_id_col = NULL
    )

    # TAS Accelerator
    tas_accelerator_server(
      "tas_accelerator",
      app_state
    )

    # Sequence Generator
    sequence_generator_server(
      "sequence_generator",
      app_state
    )

    # QC Drift Upload
    qc_results <- qcdrift_upload_server(
      "qc_upload",
      parent_session = session
    )

    # QC Drift Report
    qcdrift_report_server(
      "qc_report",
      qc_results
    )
  }

  # Launch the application
  shiny::shinyApp(ui = ui, server = server)
}
