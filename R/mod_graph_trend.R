#' Graph trends UI
#'
#' @description `mod_graph_trend_ui()` produces the UI code for a graph and
#' table showing long term trends for a single site, parameter, and depth.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_trend_server()`.
#'
#' @export
mod_graph_trend_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Enable javascript ----
    shinyjs::useShinyjs(),
    # UI ----
    tabsetPanel(
      id = ns("hide_show"),
      type = "hidden",
      # * Graph/table ----
      tabPanelBody(
        "show_graph",
        tabsetPanel(
          id = ns("graph_table"),
          tabPanel(
            "Graph",
            plotly::plotlyOutput(outputId = ns("plot")),
            div(
              style = "text-align:center;margin:1rem;display:inline-block;",
              actionButton(
                ns("toggle_trends"),
                label = "Hide Trendline",
                width = "fit-content"
              ),
              actionButton(
                ns("toggle_thresh"),
                label = "Hide Thresholds",
                width = "fit-content"
              )
            ),
            conditionalPanel(
              condition = paste0('output["', ns("hide_error"), '"] == "FALSE"'),
              style = "text-align:center; font-style:italic",
              "Unable to calculate trend. At least ten years of data required."
            )
          ),
          tabPanel(
            "Table",
            htmlOutput(ns("fig_title")),
            reactable::reactableOutput(ns("table"))
          )
        ),
        # * Thresholds ----
        htmlOutput(ns("caption"), inline = TRUE)
      ),
      # * No data message ----
      tabPanelBody(
        "hide_graph",
        "No data found. Please change selection."
      )
    )
  )
}

#' Graph trends server
#'
#' `mod_graph_trend_server()` produces the server code for a graph and
#' table showing long term trends for a single site, parameter, and depth.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_trend_ui()`.
#' @param df Dataframe. Data to graph.
#'
#' @export
mod_graph_trend_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Select tab ----
    hide_graph <- reactive({
      if (nrow(df()) == 0) {
        return("hide")
      } else {
        return("show")
      }
    }) |>
      bindEvent(df())

    observe({
      if (hide_graph() == "show") {
        updateTabsetPanel(inputId = "hide_show", selected = "show_graph")
      } else {
        updateTabsetPanel(inputId = "hide_show", selected = "hide_graph")
      }
    }) |>
      bindEvent(hide_graph())

    # Graph options ----
    val <- reactiveValues(
      threshold = TRUE,
      trendline = TRUE
    )

    # Thresholds ----
    # * Toggle visibility ----
    observe({
      if (val$threshold == TRUE) {
        val$threshold <- FALSE
        updateActionButton(
          session, "toggle_thresh",
          label = "Show Thresholds"
        )
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = FALSE),
            c(
              "Does Not Meet Criteria",
              "Lowest Acceptable Value",
              "Highest Acceptable Value"
            )
          )
      } else {
        val$threshold <- TRUE
        updateActionButton(
          session, "toggle_thresh",
          label = "Hide Thresholds"
        )
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = TRUE),
            c(
              "Does Not Meet Criteria",
              "Lowest Acceptable Value",
              "Highest Acceptable Value"
            )
          )
      }
    }) |>
      bindEvent(input$toggle_thresh)

    # * Calc thresholds ----
    thresh <- reactive({
      thresh_min <- df()$Min[1]
      thresh_max <- df()$Max[1]
      thresh_best <- df()$Best[1]

      chk <- is.na(c(thresh_min, thresh_max, thresh_best))
      if (all(chk)) {
        return(NULL)
      }

      unit <- df()$Unit[1]
      if (is.na(unit)) {
        unit <- ""
      }

      list(
        thresh_min = thresh_min,
        thresh_max = thresh_max,
        thresh_exc = df()$Excellent[1],
        thresh_best = thresh_best,
        unit = unit
      )
    })

    # Trend line ----
    # * Check if valid ----
    show_fit <- reactive({
      len_years <- length(unique(df()$Year))

      if (len_years < 10) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    })

    output$hide_error <- renderText({
      paste(show_fit())
    })
    outputOptions(output, "hide_error", suspendWhenHidden = FALSE)

    shinyjs::disable("toggle_trends")

    observe({
      if (show_fit()) {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_desc")
        shinyjs::enable("toggle_trends")
      } else {
        updateTabsetPanel(inputId = "tabset_trends", selected = "trend_error")
        shinyjs::disable("toggle_trends")
      }
    }) |>
      bindEvent(show_fit())

    # * Toggle visibility ----
    observe({
      if (val$trendline == TRUE) {
        val$trendline <- FALSE
        updateActionButton(
          session, "toggle_trends",
          label = "Show Trendline"
        )
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = FALSE),
            c(
              "Trend Line (GAM)",
              "95% Confidence Interval"
            )
          )
      } else {
        val$trendline <- TRUE
        updateActionButton(
          session, "toggle_trends",
          label = "Hide Trendline"
        )
        plotly::plotlyProxy("plot", session) |>
          plotly::plotlyProxyInvoke(
            "restyle",
            list(visible = TRUE),
            c(
              "Trend Line (GAM)",
              "95% Confidence Interval"
            )
          )
      }
    }) |>
      bindEvent(input$toggle_trends)

    # Graph ----
    output$plot <- plotly::renderPlotly({
      graph_trends(
        df(),
        thresholds = thresh(),
        show_thresh = val$threshold,
        create_trend = show_fit(),
        show_trend = val$trendline
      )
    }) |>
      bindEvent(df(), thresh(), show_fit())

    # Caption ----
    thresh_desc <- reactive({
      if (is.null(thresh())) {
        return(NULL)
      }

      thresh <- thresh_text(thresh())
      paste("<h3>Thresholds</h3>", thresh)
    }) |>
      bindEvent(thresh())

    output$caption <- renderUI({
      HTML(thresh_desc())
    })

    # Table ----
    fig_header <- reactive({
      param <- df()$Parameter[1]
      unit <- df()$Unit[1]

      paste0("<h2>", pretty_unit(param, unit), "</h2>")
    })

    output$fig_title <- renderUI({
      HTML(fig_header())
    })

    output$table <- reactable::renderReactable({
      graph_table(df(), "Site_Name")
    })
  })
}
