#' #' graphs_graph UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_graphs_graph_ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     # Enable javascript ----
#'     shinyjs::useShinyjs(),
#'     # UI ----
#'     tabsetPanel(
#'       id = ns("hide_show"),
#'       type = "hidden",
#'       # * Graph/table ----
#'       tabPanelBody(
#'         "show_graph",
#'         tabsetPanel(
#'           id = ns("graph_table"),
#'           tabPanel(
#'             "Graph",
#'             plotly::plotlyOutput(outputId = ns("plot"))
#'           ),
#'           tabPanel(
#'             "Table",
#'             htmlOutput(ns("fig_title")),
#'             reactable::reactableOutput(ns("table"))
#'           )
#'         )
#'       ),
#'       # * No data message ----
#'       tabPanelBody(
#'         "hide_graph",
#'         "No data found. Please change selection."
#'       )
#'     )
#'   )
#' }
#'
#' #' graphs_graph Server Functions
#' #'
#' #' @noRd
#' mod_graphs_graph_server <- function(id, df, group = "Site_Name") {
#'   moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'
#'     # Select tab ----
#'     hide_graph <- reactive({
#'       if (nrow(df()) == 0) {
#'         return("hide")
#'       } else {
#'         return("show")
#'       }
#'     }) |>
#'       bindEvent(df())
#'
#'     observe({
#'       if (hide_graph() == "show") {
#'         updateTabsetPanel(inputId = "hide_show", selected = "show_graph")
#'       } else {
#'         updateTabsetPanel(inputId = "hide_show", selected = "hide_graph")
#'       }
#'     }) |>
#'       bindEvent(hide_graph())
#'
#'     # Figure Title ----
#'     fig_title <- reactive({
#'       df <- df()
#'
#'       if (group == "Site_Name") {
#'         graph_title <- df$Parameter[1]
#'         table_title <- pretty_unit(df$Parameter[1], df$Unit[1])
#'       } else if (group == "Depth") {
#'         graph_title <- paste(df$Parameter[1], "at", df$Site_Name[1])
#'         table_title <- paste(
#'           pretty_unit(df$Parameter[1], df$Unit[1]), "at", df$Site_Name[1]
#'         )
#'       } else {
#'         graph_title <- df$Site_Name[1]
#'         table_title <- graph_title
#'       }
#'
#'       return(
#'         list(
#'           graph_title = graph_title,
#'           table_title = table_title
#'         )
#'       )
#'     })
#'
#'     # Graph ----
#'     output$plot <- plotly::renderPlotly({
#'       if (group == "Parameter") {
#'         graph_two_var(df(), fig_title()$graph_title)
#'       } else {
#'         graph_one_var(
#'           df = df(),
#'           fig_title = fig_title()$graph_title,
#'           group = group
#'         )
#'       }
#'     })
#'
#'     # Table ----
#'     output$fig_title <- renderUI({
#'       h2(fig_title()$table_title)
#'     })
#'
#'     output$table <- reactable::renderReactable({
#'       graph_table(df(), group)
#'     })
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_graphs_graph_ui("graphs_graph_1")
#'
#' ## To be copied in the server
#' # mod_graphs_graph_server("graphs_graph_1")
