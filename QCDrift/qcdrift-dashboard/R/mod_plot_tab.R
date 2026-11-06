# =============================================================================
# mod_plot_tab.R - (1) a reusable plot card and (2) a tab built from a spec.
# A card shows an interactive plotly chart when the object converts cleanly,
# and a static plot otherwise (patchwork, GGally matrices, heatmaps).
# =============================================================================

## ---- plot card ---------------------------------------------------------------

plotCardUI <- function(id, title, width = 6, height = 420) {
  ns <- NS(id)
  box(
    title = title, width = width, status = "primary", solidHeader = TRUE,
    maximizable = TRUE, collapsible = TRUE, headerBorder = FALSE,
    div(style = paste0("min-height:", height, "px;"),
        withSpinner(uiOutput(ns("body")), type = 6, color = PAL$primary, size = .6))
  )
}

plotCardServer <- function(id, plot_fn, interactive = TRUE, height = 420) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    p <- reactive({
      pl <- plot_fn()
      validate(need(!is.null(pl), MSG_NO_RESULT))
      pl
    })

    ply <- reactive({
      pl <- p()
      if (interactive && inherits(pl, "ggplot") && !inherits(pl, "patchwork")) {
        tryCatch(plotly::ggplotly(pl), error = function(e) NULL)
      } else {
        NULL
      }
    })

    output$body <- renderUI({
      if (is.null(ply())) plotOutput(ns("static"), height = paste0(height, "px"))
      else                plotlyOutput(ns("interactive"), height = paste0(height, "px"))
    })
    output$static      <- renderPlot({ p() }, res = 96)
    output$interactive <- renderPlotly({ req(ply()); ply() })
  })
}

## ---- tab from a spec (see TAB_SPECS in config.R) ----------------------------------

mod_plot_tab_ui <- function(id, spec) {
  ns <- NS(id)
  cards <- lapply(spec$cards, function(cd) {
    plotCardUI(ns(cd$key), cd$title, width = cd$width %||% 6, height = cd$height %||% 420)
  })
  tagList(
    fluidRow(column(12, callout(title = "How to read this", status = "info", spec$help))),
    do.call(fluidRow, cards)
  )
}

mod_plot_tab_server <- function(id, res, spec) {
  moduleServer(id, function(input, output, session) {
    for (cd in spec$cards) {
      local({
        key   <- cd$key
        inter <- cd$interactive %||% TRUE
        h     <- cd$height %||% 420
        force(key); force(inter); force(h)
        plotCardServer(key, function() get_plot(res(), key),
                       interactive = inter, height = h)
      })
    }
  })
}
