#' Sidebar location UI
#'
#' @description `mod_sidebar_location_ui()` is a helper module for
#' `mod_sidebar_ui()` that produces the UI used to filter and select sites.
#'
#' @param id Namespace ID for module. Must match ID used by
#' `mod_sidebar_location_server()`.
#'
#' @inheritParams mod_sidebar_ui
#'
#' @seealso mod_sidebar_location_server
mod_sidebar_location_ui <- function(id, varlist) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      id = ns("tabset_toggle"),
      type = "hidden",
      selected = varlist$loc_tab,
      tabPanelBody(
        "toggle",
        radioButtons(
          ns("loc_type"),
          label = h3("Select Location"),
          choices = varlist$loc_choices
        )
      ),
      tabPanelBody(
        "notoggle",
        h3("Select Location")
      ),
      tabPanelBody("blank")
    ),
    tabsetPanel(
      id = ns("tabset_loc"),
      type = "hidden",
      selected = varlist$loc_choices[1],
      tabPanelBody(
        "town",
        conditionalPanel(
          condition = paste(length(varlist$state), "> 0"),
          dropdown(
            ns("select_state"),
            label = h4("Select State"),
            choices = varlist$state
          )
        ),
        conditionalPanel(
          condition = paste(length(varlist$town), "> 0"),
          dropdown(
            ns("select_town"),
            label = h4("Select Town"),
            choices = varlist$town
          )
        )
      ),
      tabPanelBody(
        "watershed",
        dropdown(
          ns("select_watershed"),
          label = h4("Select Watershed"),
          choices = varlist$watershed
        )
      ),
      tabPanelBody("blank")
    ),
    tabsetPanel(
      id = ns("tabset_sites"),
      type = "hidden",
      tabPanelBody(
        "sites_all",
        dropdown(
          ns("select_sites_all"),
          label = h3("Select Sites"),
          choices = varlist$Site_ID,
          choice_names = varlist$Site_Name
        )
      ),
      tabPanelBody(
        "sites_n",
        dropdown(
          ns("select_sites_n"),
          label = h3("Select Site"),
          choices = varlist$Site_ID,
          choice_names = varlist$Site_Name,
          multiple = FALSE
        )
      )
    )
  )
}

#' Sidebar location Server
#'
#' @description `mod_sidebar_location_server()` is a helper module for
#' `mod_sidebar_server()` that allows users to select and filter sites.
#'
#' @param id Namespace ID for module. Must match ID used by
#' `mod_sidebar_location_ui()`.
#'
#' @inheritParams mod_sidebar_server
#'
#' @return Named list containing three variables: sites_all, sites_n, and
#' site_list.
#' * sites_all is a Site ID list for all sites selected in the dropdown
#' `select_sites_all`.
#' * sites_n is a Site ID string for the site selected in the dropdown
#' `select_sites_n`.
#' * site_list is a Site ID list of all site choices listed for dropdowns
#' `select_sites_all` and `select_sites_n`. It is used by `mod_graphs_server()`.
#'
#' @seealso mod_sidebar_location_ui
mod_sidebar_location_server <- function(
  id, df_sites, selected_tab, selected_site
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Toggle tabs ----
    observe({
      updateTabsetPanel(
        inputId = "tabset_loc",
        selected = input$loc_type
      )
    }) |>
      bindEvent(input$loc_type)

    observe({
      if (selected_tab() == "graphs") {
        updateTabsetPanel(inputId = "tabset_sites", selected = "sites_n")
      } else {
        updateTabsetPanel(inputId = "tabset_sites", selected = "sites_all")
      }
    }) |>
      bindEvent(selected_tab())

    # Set sites ----
    default_sites <- create_site_list(df_sites)
    locval <- reactiveValues(
      town_sites = default_sites,
      watershed_sites = default_sites
    )

    # * Update by state ----
    observe({
      if (is.null(df_sites$Town)) {
        locval$town_sites <- filter_site_list(
          df_sites,
          "State",
          input$select_state
        )
      } else {
        choices <- filter_towns(df_sites, input$select_state)

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "select_town",
          choices = choices,
          selected = choices
        )
      }
    }) |>
      bindEvent(input$select_state)

    # * Update by town ----
    observe({
      locval$town_sites <- filter_site_list(
        df_sites,
        "Town",
        input$select_town
      )
    }) |>
      bindEvent(input$select_town)

    # * Update by watershed ----
    observe({
      locval$watershed_sites <- filter_site_list(
        df_sites,
        "Watershed",
        input$select_watershed
      )
    }) |>
      bindEvent(input$select_watershed)

    # * Update dropdowns ----
    site_list <- reactive({
      if (input$loc_type == "town") {
        return(locval$town_sites)
      } else {
        return(locval$watershed_sites)
      }
    }) |>
      bindEvent(c(locval$town_sites, locval$watershed_sites, input$loc_type))

    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_sites_all",
        choices = site_list(),
        selected = site_list()
      )
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_sites_n",
        choices = site_list(),
        selected = site_list()[1]
      )
    }) |>
      bindEvent(site_list())

    # Update sites_n on map click ------
    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_sites_n",
        selected = selected_site()
      )
    }) |>
      bindEvent(selected_site())

    # Return data ----
    return(
      list(
        # Return selected sites
        sites_all = reactive({
          input$select_sites_all
        }),
        sites_n = reactive({
          input$select_sites_n
        }),
        # Return site list - used for mod_graphs_server()
        site_list = reactive({
          site_list()
        })
      )
    )
  })
}
