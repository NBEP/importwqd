#' graphs_trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graphs_trends_ui <- function(id) {
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

#' graphs_trends Server Functions
#'
#' @noRd
mod_graphs_trends_server <- function(id, df) {
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
      thresh_min = df()$Min[1]
      thresh_max = df()$Max[1]
      thresh_excellent = df()$Excellent[1]
      thresh_best = df()$Best[1]

      chk <- is.na(thresh_min) & is.na(thresh_max) & is.na(thresh_excellent) &
        is.na(thresh_best)
      if (chk) {
        return(NULL)
      }

      thresh <- list(
        thresh_min = thresh_min,
        thresh_max = thresh_max,
        thresh_excellent = thresh_excellent,
        thresh_best = thresh_best
      )

      return(thresh)
    })

    # Trend line ----
    # * Check if valid ----
    len_years <- reactive({
      length(unique(df()$Year))
    })
    show_fit <- reactive({
      if (len_years() < 10) {
        FALSE
      } else {
        TRUE
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
        df = df(),
        fig_title = df()$Parameter[1],
        thresholds = thresh(),
        show_thresh = val$threshold,
        create_trend = show_fit(),
        show_trend = val$trendline
      )
    })

    # Caption ----
    thresh_desc <- reactive({
      desc <- thresh_text(df())

      if (is.null(desc)) {
        return(desc)
      }

      paste("<h3>Thresholds</h3>", desc)
    })

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

## To be copied in the UI
# mod_graphs_trends_ui("graphs_graph_1")

## To be copied in the server
# mod_graphs_trends_server("graphs_graph_1")
