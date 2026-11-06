# =============================================================================
# mod_verdict.R - the first screen after a run: did the correction work?
# =============================================================================

mod_verdict_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, uiOutput(ns("banner")))),
    fluidRow(
      valueBoxOutput(ns("vb_med_raw"),  width = 3),
      valueBoxOutput(ns("vb_med_corr"), width = 3),
      valueBoxOutput(ns("vb_ok_raw"),   width = 3),
      valueBoxOutput(ns("vb_ok_corr"),  width = 3)
    ),
    fluidRow(
      box(
        title = "QC precision across all features", width = 12,
        status = "primary", solidHeader = TRUE, headerBorder = FALSE,
        maximizable = TRUE,
        p(class = "text-muted",
          "Each line shows the share of features (y) whose QC CV is at or below a",
          "given value (x). A curve that rises further to the left is better;",
          "the dashed line is your CV limit."),
        withSpinner(plotlyOutput(ns("curve"), height = "420px"),
                    type = 6, color = PAL$primary, size = .6)
      )
    )
  )
}

mod_verdict_server <- function(id, res, verdict, cv_tbl, thr) {
  moduleServer(id, function(input, output, session) {

    v <- reactive(if (is.null(res())) NULL else verdict())

    output$banner <- renderUI({
      vd <- v()
      if (is.null(vd)) {
        return(callout(title = "No results yet", status = "secondary",
                       "Go to 'Upload & Run', choose a file and click Run QCDrift."))
      }
      callout(title = vd$title, status = vd$level,
              tags$p(tags$strong(vd$headline)), tags$p(vd$detail))
    })

    output$vb_med_raw  <- renderValueBox(valueBox(fmt_pct(v()$med_raw),  "Median QC CV - raw",
                                                  icon = icon("percent"), color = "warning"))
    output$vb_med_corr <- renderValueBox(valueBox(fmt_pct(v()$med_corr), "Median QC CV - corrected",
                                                  icon = icon("percent"), color = "success"))
    output$vb_ok_raw   <- renderValueBox(valueBox(fmt_pct(v()$ok_raw),
                                                  paste0("Features under ", thr(), "% - raw"),
                                                  icon = icon("check"), color = "warning"))
    output$vb_ok_corr  <- renderValueBox(valueBox(fmt_pct(v()$ok_corr),
                                                  paste0("Features under ", thr(), "% - corrected"),
                                                  icon = icon("check-double"), color = "success"))

    output$curve <- renderPlotly({
      cv <- cv_tbl()
      validate(need(!is.null(cv) && any(c("Raw", "Corrected") %in% names(cv)), MSG_NO_RESULT))

      d <- cv |>
        tidyr::pivot_longer(cols = dplyr::any_of(c("Raw", "Corrected")),
                            names_to = "Stage", values_to = "CV") |>
        dplyr::filter(is.finite(CV)) |>
        dplyr::group_by(Stage) |>
        dplyr::arrange(CV, .by_group = TRUE) |>
        dplyr::mutate(Share = 100 * dplyr::row_number() / dplyr::n()) |>
        dplyr::ungroup()
      d$Stage <- factor(d$Stage, levels = c("Raw", "Corrected"))

      g <- ggplot(d, aes(CV, Share, colour = Stage)) +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = thr(), linetype = "dashed", colour = "grey40") +
        scale_colour_manual(values = c(Raw = PAL$raw, Corrected = PAL$corrected), name = NULL) +
        coord_cartesian(xlim = c(0, max(50, 2 * thr()))) +
        labs(x = "QC CV (%)", y = "Features at or below this CV (%)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())

      ggplotly(g) |> layout(legend = list(orientation = "h", y = -0.15))
    })
  })
}
