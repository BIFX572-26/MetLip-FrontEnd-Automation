#' Launches the dashboard
#' package load for live edits/testing, remove when done test,
#'   run devtools sequence: devtools::(document(), check(), install())
#'   then restart R session
pkgload::load_all()
app <- load_MetLip_FrontEnd()
shiny::runApp(app)
