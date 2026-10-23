library(shiny)
library(shinydashboard)
library(jsonlite)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(readxl)
library(zip)
library(sortable)
library(qcdrift)


if (FALSE)
  devtools::install_github("IDSS-NIAID/MetLip-FrontEnd-Automation")
library(MetLipAutomation)


source(file.path("R", "modules","tas_client.R"))
source(file.path("R", "dashboard_shell.R"))
source(file.path("R", "modules", "tas_accelerator_module.R"))
source(file.path("R", "modules", "sequence_generator_module.R"))
source(file.path("R", "modules", "qcdrift_ui.R"))
source(file.path("R", "modules", "qcdrift_server.R"))

tas_api <- new_tas_client()

ui <- dashboard_shell(
  tas_ui = tas_accelerator_ui("tas"),
  sequence_ui = sequence_generator_ui("sequence"),
  qc_upload_ui = qc_upload_ui("qc_upload"),
  qc_report_ui = qc_report_ui("qc_report")
)

server <- function(input, output, session) {
  tas_samples <- tas_accelerator_server("tas", tas_api)
  sequence_generator_server("sequence", tas_samples, tas_api$detect_sub_id_col)
  qcdrift_server("", )
}

shinyApp(ui, server)
