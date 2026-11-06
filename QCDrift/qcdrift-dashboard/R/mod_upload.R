# =============================================================================
# mod_upload.R - choose data, validate it, pick settings, request a run.
# Returns list(run = <reactive run request>, stale = <reactive logical>).
# The module never runs the pipeline itself; app.R does that in the background.
# =============================================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "1 - Choose data", width = 4, status = "primary",
        solidHeader = TRUE, headerBorder = FALSE,
        fileInput(ns("file"), NULL, accept = c(".xlsx", ".xls"),
                  buttonLabel = "Browse...", placeholder = "Raw data (.xlsx)"),
        actionButton(ns("use_example"), "Try the example dataset",
                     icon = icon("flask"), class = "btn-outline-primary btn-sm btn-block"),
        tags$div(style = "height:.8rem;"),
        uiOutput(ns("validation"))
      ),
      box(
        title = "2 - Settings", width = 4, status = "primary",
        solidHeader = TRUE, headerBorder = FALSE,
        radioButtons(
          ns("norm"), "Normalization",
          choiceNames = list("Total sum normalization (TSN)",
                             "Autoscaling (mean 0, SD 1)"),
          choiceValues = c("tsn", "auto")),
        helpText("TSN is the usual choice. It corrects differences in sample",
                 "volume or dilution between injections."),
        tags$details(
          tags$summary("Advanced options"),
          textInput(ns("prefix"), "QC sample prefix", value = "QC"),
          helpText("Samples whose names start with this text are treated as QC.")
        ),
        actionButton(ns("run"), "Run QCDrift", icon = icon("play"),
                     class = "btn-primary btn-block btn-run")
      ),
      box(
        title = "Status", width = 4, status = "secondary",
        solidHeader = TRUE, headerBorder = FALSE,
        uiOutput(ns("status"))
      )
    ),
    fluidRow(
      valueBoxOutput(ns("vb_samples"),  width = 3),
      valueBoxOutput(ns("vb_features"), width = 3),
      valueBoxOutput(ns("vb_qc"),       width = 3),
      valueBoxOutput(ns("vb_norm"),     width = 3)
    ),
    fluidRow(
      box(
        title = "Data preview", width = 12, status = "info", solidHeader = TRUE,
        headerBorder = FALSE, maximizable = TRUE,
        textOutput(ns("dims")),
        withSpinner(DTOutput(ns("preview")), type = 6, color = PAL$primary, size = .6)
      )
    )
  )
}

mod_upload_server <- function(id, state, kpis) {
  moduleServer(id, function(input, output, session) {

    # ---- which file is selected ------------------------------------------------
    chosen <- reactiveVal(NULL)

    observeEvent(input$file, {
      f <- input$file
      chosen(list(path = f$datapath, name = f$name, size = f$size,
                  example = FALSE, md5 = unname(tools::md5sum(f$datapath))))
    })

    observeEvent(input$use_example, {
      ex <- system.file("extdata", "RawMassSpec.xlsx", package = "qcdrift")
      if (!nzchar(ex)) {
        showNotification("The example file was not found in the installed qcdrift package.",
                         type = "warning")
        return()
      }
      chosen(list(path = ex, name = "RawMassSpec.xlsx (example)",
                  size = file.size(ex), example = TRUE,
                  md5 = unname(tools::md5sum(ex))))
    })

    # ---- validation + preview --------------------------------------------------
    raw   <- reactive({ req(chosen()); read_upload(chosen()$path) })
    check <- reactive(check_upload(raw(), trimws(input$prefix %||% "")))

    output$validation <- renderUI({
      if (is.null(chosen())) {
        return(tags$span(class = "text-muted", "No file selected yet."))
      }
      ck <- check()
      tagList(
        if (length(ck$errors))
          callout(title = "Problem with this file", status = "danger", ck$errors),
        if (length(ck$warnings))
          callout(title = "Worth a look", status = "warning", ck$warnings),
        if (ck$ok)
          tags$p(class = "text-success", icon("check-circle"),
                 sprintf(" %s rows x %s columns read successfully.",
                         format(ck$n_rows, big.mark = ","),
                         format(ck$n_cols, big.mark = ",")))
      )
    })

    output$dims <- renderText({
      ch <- chosen(); req(ch)
      paste("File:", ch$name)
    })

    output$preview <- renderDT({
      validate(need(!is.null(chosen()), "Choose a file or try the example dataset to preview it."))
      df <- raw()
      validate(need(is.data.frame(df), conditionMessage(df)))
      dt_view(df, page_len = 8)
    })

    # ---- run request -------------------------------------------------------------
    run_request <- eventReactive(input$run, {
      ch <- chosen()
      if (is.null(ch)) {
        showNotification("Choose a file first.", type = "warning")
        req(FALSE)
      }
      if (!check()$ok) {
        showNotification("Fix the problem shown under the file box first.", type = "error")
        req(FALSE)
      }
      list(path = ch$path, name = ch$name, example = ch$example, md5 = ch$md5,
           norm = input$norm, prefix = trimws(input$prefix %||% ""))
    })

    # ---- is the displayed result out of date? ---------------------------------------
    stale <- reactive({
      s  <- state$settings
      ch <- chosen()
      if (is.null(s) || is.null(ch)) return(FALSE)
      cur <- list(md5 = ch$md5, norm = input$norm, prefix = trimws(input$prefix %||% ""))
      !identical(s[c("md5", "norm", "prefix")], cur)
    })

    # ---- status checklist --------------------------------------------------------------
    output$status <- renderUI({
      ch <- chosen()
      ck <- if (!is.null(ch)) check() else NULL
      has_file <- !is.null(ch)
      valid    <- !is.null(ck) && isTRUE(ck$ok)
      st       <- state$status
      done     <- !is.null(state$res)
      found    <- if (done) plots_found(state$res) else NULL

      tagList(
        tags$ul(
          class = "qc-steps",
          status_step(has_file, "File selected", if (has_file) ch$name),
          status_step(valid, "File checks passed"),
          status_step(done, "Pipeline run",
                      if (identical(st, "running")) "running in the background..."
                      else if (done) paste("normalization:", toupper(state$settings$norm))),
          status_step(done && all(found), "Figures generated",
                      if (done) sprintf("%d of %d available", sum(found), length(found)))
        ),
        if (!is.null(state$error))
          callout(title = "Last run failed", status = "danger", state$error),
        if (stale())
          callout(title = "Settings changed", status = "warning",
                  "The file or settings differ from the last run. Click 'Run QCDrift' to refresh.")
      )
    })

    # ---- KPI boxes ------------------------------------------------------------------------
    output$vb_samples  <- renderValueBox(kpi_box(kpis()$samples,  "Samples",       "vial",  "primary"))
    output$vb_features <- renderValueBox(kpi_box(kpis()$features, "Features",      "dna",   "info"))
    output$vb_qc       <- renderValueBox(kpi_box(kpis()$qc,       "QC injections", "flask", "danger"))
    output$vb_norm     <- renderValueBox({
      kpi_box(if (is.null(state$settings)) NA else toupper(state$settings$norm),
              "Normalization", "balance-scale", "success")
    })

    list(run = run_request, stale = stale)
  })
}
