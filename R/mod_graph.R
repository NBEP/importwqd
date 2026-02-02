#' #' graphs UI Function
#' #'
#' #' @description A shiny Module.
#' #'
#' #' @param id,input,output,session Internal parameters for {shiny}.
#' #'
#' #' @noRd
#' #'
#' #' @importFrom shiny NS tagList
#' mod_graphs_ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     bslib::navset_card_tab(
#'       id = "graphs_tabset",
#'       full_screen = FALSE,
#'       # title = "Graphs",
#'       # Trends -----
#'       bslib::nav_panel(
#'         "Long Term Trends",
#'         mod_graphs_trends_ui(ns("graph_trends"))
#'       ),
#'       # Depth ----
#'       if (length(unique(df_data$Depth)) > 1) {
#'         bslib::nav_panel(
#'           "Compare Depths",
#'           select_dropdown(
#'             ns("extra_depth"),
#'             label = h2("Select Depths"),
#'             choices = unique(df_data$Depth)
#'           ),
#'           mod_graphs_graph_ui(ns("graph_depth"))
#'         )
#'       },
#'       # Sites ----
#'       bslib::nav_panel(
#'         "Compare Sites",
#'         select_dropdown(
#'           ns("extra_sites"),
#'           label = HTML(paste(
#'             h2("Select Sites"),
#'             "Select up to five sites"
#'           )),
#'           choices = df_sites$Site_ID,
#'           choice_names = df_sites$Site_Name,
#'           max_options = 5
#'         ),
#'         mod_graphs_graph_ui(ns("graph_sites"))
#'       ),
#'       # Parameters ----
#'       bslib::nav_panel(
#'         "Compare Parameters",
#'         select_dropdown(
#'           ns("extra_param"),
#'           label = HTML(paste(
#'             h2("Select Indicators"),
#'             "Select two indicators"
#'           )),
#'           choices = unique(df_data$Parameter),
#'           max_options = 2
#'         ),
#'         mod_graphs_graph_ui(ns("graph_param"))
#'       )
#'     )
#'   )
#' }
#'
#' #' graphs Server Functions
#' #'
#' #' @noRd
#' mod_graphs_server <- function(id, selected_var) {
#'   moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'
#'     # Filter data ----
#'     df_filter <- reactive({
#'       req(selected_var$year_range())
#'       req(selected_var$month_range())
#'
#'       selected_months <- importwqd:::sort_months(selected_var$month_range())
#'
#'       df <- df_data |>
#'         dplyr::filter(
#'           Year >= selected_var$year_range()[1] &
#'             Year <= selected_var$year_range()[2]
#'         ) |>
#'         dplyr::filter(Month %in% selected_months) |>
#'         dplyr::select(!Month)
#'
#'       return(df)
#'     })
#'
#'     # Graph: Trends ----
#'     df_trends <- reactive({
#'       req(selected_var$sites_n())
#'       req(selected_var$param_n())
#'
#'       sites <- selected_var$sites_n()
#'       param <- selected_var$param_n()
#'       depth <- c(NA, selected_var$depth_n())
#'
#'       df <- df_filter() |>
#'         dplyr::filter(
#'           Site_ID == sites &
#'             Parameter == param &
#'             Depth %in% depth
#'         )
#'
#'       return(df)
#'     })
#'
#'     mod_graphs_trends_server("graph_trends", df = reactive({
#'       df_trends()
#'     }))
#'
#'     # Graph: Compare Sites ----
#'     # * Update picker input ----
#'     observe({
#'       shinyWidgets::updatePickerInput(
#'         session = session,
#'         inputId = "extra_sites",
#'         choices = selected_var$site_list(),
#'         selected = selected_var$site_list()[1]
#'       )
#'     }) |>
#'       bindEvent(selected_var$site_list())
#'
#'
#'     # * Create graph ----
#'     df_comp_sites <- reactive({
#'       req(selected_var$param_n())
#'
#'       sites <- input$extra_sites
#'       param <- selected_var$param_n()
#'       depth <- c(NA, selected_var$depth_n())
#'
#'       df <- df_filter() |>
#'         dplyr::filter(
#'           Site_ID %in% sites &
#'             Parameter == param &
#'             Depth %in% depth
#'         )
#'
#'       return(df)
#'     })
#'
#'     mod_graphs_graph_server("graph_sites", df = reactive({
#'       df_comp_sites()
#'     }))
#'
#'     # Graph: Compare Depths ----
#'     df_comp_depth <- reactive({
#'       req(selected_var$sites_n())
#'       req(selected_var$param_n())
#'
#'       sites <- selected_var$sites_n()
#'       param <- selected_var$param_n()
#'       depth <- c(NA, input$extra_depth)
#'
#'       df <- df_filter() |>
#'         dplyr::filter(
#'           Site_ID == sites &
#'             Parameter == param &
#'             !is.na(Depth) &
#'             Depth %in% depth
#'         )
#'
#'       return(df)
#'     })
#'
#'     mod_graphs_graph_server("graph_depth",
#'                             df = reactive({
#'                               df_comp_depth()
#'                             }),
#'                             group = "Depth"
#'     )
#'
#'     # Graph: Compare Parameters ----
#'     df_comp_par <- reactive({
#'       req(selected_var$sites_n())
#'
#'       sites <- selected_var$sites_n()
#'       param <- input$extra_param
#'       depth <- c(NA, selected_var$depth_n())
#'
#'       df <- df_filter() |>
#'         dplyr::filter(
#'           Site_ID == sites &
#'             Parameter %in% param &
#'             Depth %in% depth
#'         )
#'
#'       return(df)
#'     })
#'
#'     mod_graphs_graph_server("graph_param",
#'                             df = reactive({
#'                               df_comp_par()
#'                             }),
#'                             group = "Parameter"
#'     )
#'   })
#' }
#'
#' ## To be copied in the UI
#' # mod_graphs_ui("graphs_1")
#'
#' ## To be copied in the server
#' # mod_graphs_server("graphs_1")
