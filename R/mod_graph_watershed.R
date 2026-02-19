#' Graph watershed trends UI
#'
#' @description `mod_graph_watershed_ui()` produces the UI code for a graph and
#' table showing long term trends for a single watershed, parameter, and depth.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_watershed_server()`.
#'
#' @noRd
mod_graph_watershed_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
        )
      ),
      # * No data message ----
      tabPanelBody(
        "hide_graph",
        "No data found. Please change selection."
      )
    )
  )
}

#' Graph watershed trends server
#'
#' `mod_graph_watershed_server()` produces the server code for a graph and
#' table showing long term trends for a single watershed, depth, and parameter.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_watershed_ui()`.
#' @param df Dataframe. Data to graph.
#' @param trendline Boolean. If `TRUE`, adds trendline.
#'
#' @noRd
mod_graph_watershed_server <- function(id, df, trendline = TRUE) {
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

    # Graph ----
    output$plot <- plotly::renderPlotly({
      graph_trends(
        df(),
        col_name = "Watershed",
        trendline = trendline()
      )
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
