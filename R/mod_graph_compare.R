#' Comparison graph UI
#'
#' @description `mod_graph_compare_ui()` produces the UI code for a graph and
#' table comparing trends by group.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_compare_server()`.
#'
#' @export
mod_graph_compare_ui <- function(id) {
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
            plotly::plotlyOutput(outputId = ns("plot"))
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

#' Comparison graph server
#'
#' @description `mod_graph_compare_server()` produces the server code for a
#' graph and table comparing trends by group.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_compare_ui()`.
#' @param df Dataframe. Data to graph.
#' @param group String. Column used to group data.
#' @param add_lines Boolean. If `TRUE`, displays scatter plot as lines +
#' markers. If `FALSE`, displays scatter plot as markers only. Default `FALSE`.
#'
#' @export
mod_graph_compare_server <- function(
  id, df, group = "Site_Name", add_lines = FALSE
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

    # Figure Title ----
    fig_title <- reactive({
      df <- df()
      param <- df$Parameter[1]
      unit <- df$Unit[1]
      site <- df$Site_Name[1]

      if (group == "Site_Name") {
        graph_title <- param
        table_title <- pretty_unit(param, unit)
      } else if (group == "Depth") {
        graph_title <- paste(param, "at", site)
        table_title <- paste(pretty_unit(param, unit), "at", site)
      } else {
        graph_title <- site
        table_title <- graph_title
      }

      return(
        list(
          graph_title = graph_title,
          table_title = table_title
        )
      )
    })

    # Graph ----
    output$plot <- plotly::renderPlotly({
      if (group == "Parameter") {
        graph_param(
          df(),
          fig_title = fig_title()$graph_title,
          add_lines = add_lines
        )
      } else {
        graph_compare(
          df(),
          fig_title = fig_title()$graph_title,
          group = group,
          add_lines = add_lines
        )
      }
    })

    # Table ----
    output$fig_title <- renderUI({
      h2(fig_title()$table_title)
    })

    output$table <- reactable::renderReactable({
      graph_table(df(), group)
    })
  })
}
