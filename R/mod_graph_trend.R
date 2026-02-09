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
#' @param in_var List with three items: style, trend, thresh
#' * `style`: String. Sets mode for scatterplot.
#' * `trend`: Boolean. If `TRUE`, adds trendline.
#' * `thresh`: Boolean. If `TRUE`, adds red bar to indicate values outside
#' acceptable range and a blue bar to indicate excellent values. Default
#' `FALSE`. description
#'
#' @export
mod_graph_trend_server <- function(
  id, df, in_var = list(style = "lines+markers", trend = TRUE, tresh = TRUE)
) {
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

    # Thresholds ----
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

    # Graph ----
    output$plot <- plotly::renderPlotly({
      graph_trends(
        df(),
        thresholds = thresh(),
        trendline = show_fit(),
        display = in_var()
      )
    })

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
