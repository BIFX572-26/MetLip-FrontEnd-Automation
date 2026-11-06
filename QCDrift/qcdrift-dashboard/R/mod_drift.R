# =============================================================================
# mod_drift.R - inspect one feature at a time, and list features to review.
# =============================================================================

mod_drift_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Choose a feature", width = 4, status = "primary",
        solidHeader = TRUE, headerBorder = FALSE,
        selectizeInput(ns("feature"), "Metabolite / feature", choices = NULL),
        checkboxInput(ns("log"), "Log10 y-axis", FALSE),
        checkboxInput(ns("qcline"), "Connect QC injections", TRUE),
        helpText("Tip: click a row in the table below to jump to that feature.")
      ),
      valueBoxOutput(ns("vb_raw"),  width = 3),
      valueBoxOutput(ns("vb_corr"), width = 3),
      valueBoxOutput(ns("vb_gain"), width = 2)
    ),
    fluidRow(
      box(
        title = "Intensity vs injection order", width = 12, status = "primary",
        solidHeader = TRUE, headerBorder = FALSE, maximizable = TRUE,
        p(class = "text-muted",
          "QC injections (diamonds) should lie on a flat line once drift is",
          "corrected. A slope or wave in the raw panel is instrument drift."),
        withSpinner(plotlyOutput(ns("plot"), height = "520px"),
                    type = 6, color = PAL$primary, size = .6)
      )
    ),
    fluidRow(
      box(
        title = "Features ranked by QC CV", width = 12, status = "secondary",
        solidHeader = TRUE, headerBorder = FALSE, collapsible = TRUE,
        p(class = "text-muted",
          "'Review = Yes' marks features whose corrected QC CV is above the limit",
          "set in the settings panel (gear icon, top right)."),
        downloadButton(ns("dl_review"), "Download features to review (.csv)",
                       class = "btn-outline-primary btn-sm"),
        tags$div(style = "height:.6rem;"),
        DTOutput(ns("table"))
      )
    )
  )
}

mod_drift_server <- function(id, long, cv_tbl, thr) {
  moduleServer(id, function(input, output, session) {

    drift <- reactive({
      d <- long()
      validate(need(!is.null(d),
                    paste("The result tables do not contain the columns the drift",
                          "explorer needs (injection order, abundance, metabolite).")))
      d
    })

    review <- reactive(review_table(cv_tbl(), thr()))

    observeEvent(drift(), {
      feats <- sort(unique(drift()$metabolite))
      updateSelectizeInput(session, "feature", choices = feats,
                           selected = feats[1], server = TRUE)
    })

    output$plot <- renderPlotly({
      req(input$feature)
      d <- drift() |> dplyr::filter(metabolite == input$feature)
      if (isTRUE(input$log)) d <- dplyr::filter(d, value > 0)
      validate(need(nrow(d) > 0, "No data for this feature."))

      d$type <- ifelse(d$qc, "QC", "Sample")
      d$tip  <- paste0("Sample: ", d$sample, "<br>Injection: ", d$io,
                       "<br>Intensity: ", signif(d$value, 4))

      g <- ggplot(d, aes(io, value)) +
        geom_point(aes(colour = type, shape = type, text = tip), size = 2.2, alpha = .85) +
        scale_colour_manual(values = c(Sample = PAL$sample, QC = PAL$qc), name = NULL) +
        scale_shape_manual(values = c(Sample = 16, QC = 18), name = NULL) +
        facet_wrap(~stage, ncol = 1, scales = "free_y") +
        labs(x = "Injection order", y = "Intensity") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

      if (isTRUE(input$qcline)) {
        g <- g + geom_line(data = dplyr::filter(d, qc), colour = PAL$qc,
                           linewidth = .4, alpha = .6)
      }
      if (isTRUE(input$log)) g <- g + scale_y_log10()

      ggplotly(g, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.1))
    })

    # ---- per-feature CV boxes ---------------------------------------------------
    feature_cv <- function(col) {
      reactive({
        tb <- cv_tbl()
        if (is.null(tb) || !col %in% names(tb) || is.null(input$feature)) return(NA_real_)
        tb[[col]][tb$metabolite == input$feature][1]
      })
    }
    cv_raw  <- feature_cv("Raw")
    cv_corr <- feature_cv("Corrected")

    output$vb_raw  <- renderValueBox(valueBox(fmt_pct(cv_raw()),  "QC CV - raw",
                                              icon = icon("percent"), color = "warning"))
    output$vb_corr <- renderValueBox(valueBox(fmt_pct(cv_corr()), "QC CV - corrected",
                                              icon = icon("percent"), color = "success"))
    output$vb_gain <- renderValueBox({
      g <- cv_corr() - cv_raw()
      valueBox(if (is.finite(g)) sprintf("%+.1f", g) else "-", "Change (pts)",
               icon = icon("arrow-down"), color = "info")
    })

    # ---- ranked table and cross-selection ---------------------------------------------
    output$table <- renderDT({
      rv <- review()
      validate(need(!is.null(rv), MSG_NO_RESULT))
      dt_view(rv, page_len = 8, selection = "single")
    })

    observeEvent(input$table_rows_selected, {
      sel <- input$table_rows_selected
      req(length(sel) == 1)
      updateSelectizeInput(session, "feature", selected = review()$Feature[sel], server = TRUE)
    })

    output$dl_review <- downloadHandler(
      filename = function() paste0("features_to_review_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(file) {
        rv <- review(); req(rv)
        utils::write.csv(rv[rv$Review == "Yes", , drop = FALSE], file, row.names = FALSE)
      }
    )
  })
}
