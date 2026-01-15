#' Sidebar UI
#'
#' @description `mod_sidebar_ui()` produces the sidebar UI for the companion app
#' `wqdashboard`.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_sidebar_server()`.
#' @param df_data Dataframe with result data. Must have been processed
#' by `format_results()` and include columns Parameter, Month, Year.
#' @param df_score Dataframe with data scores. Must have been processed
#' by `score_results()` and include column Parameter.
#' @param df_sites Dataframe with site data. Must have been processed
#' by `format_sites()` and  include columns Site_ID, Site_Name.
#'
#' @export
mod_sidebar_ui <- function(id, df_data, df_score, df_sites) {
  ns <- NS(id)

  # Set variables
  list_param <- unique(df_data$Parameter)
  list_param_score <- unique(df_score$Parameter)
  list_depth <- sort_depth(df_data$Depth)
  list_month <- sort_months(df_data$Month)
  list_year <- sort(unique(df_data$Year))

  tagList(
    bslib::accordion(
      multiple = FALSE,
      # Select location ----
      bslib::accordion_panel(
        title = h2("1. Location"),
        value = "location",
        mod_sidebar_location_ui(ns("select_location"), df_sites)
      ),
      # Select data ----
      bslib::accordion_panel(
        title = h2("2. Data"),
        value = "indicators",
        tabsetPanel(
          id = ns("tabset_param"),
          type = "hidden",
          tabPanelBody(
            "param_n",
            dropdown(
              ns("select_param_n"),
              label = h3("Select Indicator"),
              choices = list_param,
              multiple = FALSE
            )
          ),
          tabPanelBody(
            "param_score",
            dropdown(
              ns("select_param_score"),
              label = h3("Select Indicators"),
              choices = list_param_score
            )
          ),
          tabPanelBody(
            "param_download",
            dropdown(
              ns("select_param_download"),
              label = h3("Select Indicators"),
              choices = list_param
            )
          )
        ),
        tabsetPanel(
          id = ns("tabset_score"),
          type = "hidden",
          tabPanelBody(
            "show_score",
            checkboxInput(
              ns("chk_nascore"),
              label = "Include missing scores",
              value = TRUE
            )
          ),
          tabPanelBody("hide_score")
        ),
        tabsetPanel(
          id = ns("tabset_depth"),
          type = "hidden",
          tabPanelBody(
            "depth_n",
            dropdown(
              ns("select_depth_n"),
              label = h3("Select Depth"),
              choices = list_depth,
              sorted = FALSE,
              multiple = FALSE
            )
          ),
          tabPanelBody(
            "depth_all",
            dropdown(
              ns("select_depth_all"),
              label = h3("Select Depths"),
              choices = list_depth,
              sorted = FALSE
            )
          ),
          tabPanelBody("depth_null")
        )
      ),
      # Select date -----
      bslib::accordion_panel(
        title = h2("3. Date"),
        value = "dates",
        tabsetPanel(
          id = ns("tabset_dates"),
          type = "hidden",
          tabPanelBody(
            "by_year",
            dropdown(
              ns("select_year"),
              label = h3("Select Year"),
              choices = list_year,
              decreasing = TRUE,
              multiple = FALSE
            )
          ),
          tabPanelBody(
            "by_range",
            sliderInput(
              ns("select_year_range"),
              label = h3("Select Years"),
              min = list_year[1],
              max = list_year[length(list_year)],
              value = c(
                list_year[1],
                list_year[length(list_year)]
              ),
              sep = ""
            ),
            shinyWidgets::sliderTextInput(
              ns("select_month"),
              label = h3("Select Months"),
              choices = list_month,
              selected = c(
                list_month[1],
                list_month[length(list_month)]
              )
            )
          )
        )
      )
    )
  )
}

#' sidebar Server Functions
#'
#' @description `mod_sidebar_server()` produces the sidebar server for the
#' companion app`wqdashboard`.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_sidebar_ui()`.
#' @param selected_tab Reactive variable. Name of selected tab.
#' @param selected_site Reactive variable. Site_ID for site selected in
#' `mod_map_ui()`.
#' @param df_data_all Dataframe with raw result data. Must have been processed
#' by `qaqc_results()`.
#' @param df_sites_all Dataframe with raw site data. Must have been processed
#' by `qaqc_sites()`.
#'
#' @inheritParams mod_sidebar_ui
#'
#' @returns TO DO.
#'
#' @export
mod_sidebar_server <- function(
  id, df_data_all, df_data, df_score, df_sites_all, df_sites, selected_tab,
  selected_site
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Add modules ----
    loc_server <- mod_sidebar_location_server(
      "select_location",
      df_sites,
      selected_tab,
      selected_site
    )

    # Update tabs ----
    observe({
      if (selected_tab() == "map") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_n")
        updateTabsetPanel(inputId = "tabset_score", selected = "show_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_year")
      } else if (selected_tab() == "report_card") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_score")
        updateTabsetPanel(inputId = "tabset_score", selected = "show_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_year")
      } else if (selected_tab() == "graphs") {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_n")
        updateTabsetPanel(inputId = "tabset_score", selected = "hide_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_range")
      } else {
        updateTabsetPanel(inputId = "tabset_param", selected = "param_download")
        updateTabsetPanel(inputId = "tabset_score", selected = "hide_score")
        updateTabsetPanel(inputId = "tabset_dates", selected = "by_range")
      }
    }) %>%
      bindEvent(selected_tab())

    observe({
      if (length(unique(df_data$Depth)) > 1) {
        updateTabsetPanel(inputId = "tabset_depth", selected = "depth_null")
      } else if (selected_tab() %in% c("map", "graphs")) {
        updateTabsetPanel(inputId = "tabset_depth", selected = "depth_n")
      } else {
        updateTabsetPanel(inputId = "tabset_depth", selected = "depth_all")
      }
    }) %>%
      bindEvent(selected_tab())

    # Generate output ----
    # * Map ----

    # * Report card ----

    # * Graph ----

    # * Download -----

    # Load bearing outdated code ----
    df_score_filter <- reactive({
      req(input$select_year)

      df <- dplyr::filter(df_score, .data$Year == input$select_year)

      return(df)
    })

    # Return data ----
    return(
      list(
        sites_all = reactive({
          loc_server$sites_all()
        }),
        sites_n = reactive({
          loc_server$sites_n()
        }),
        site_list = reactive({
          loc_server$site_list()
        }),
        param_all = reactive({
          input$select_param_download
        }),
        param_n = reactive({
          input$select_param_n
        }),
        param_short = reactive({
          input$select_param_score
        }), # used for report card
        score = reactive({
          input$chk_nascore
        }),
        depth_n = reactive({
          input$select_depth_n
        }),
        depth_all = reactive({
          input$select_depth_all
        }),
        year = reactive({
          input$select_year
        }),
        year_range = reactive({
          input$select_year_range
        }),
        month_range = reactive({
          input$select_month
        }),
        df_score_f = reactive({
          df_score_filter()
        })
      )
    )
  })
}
