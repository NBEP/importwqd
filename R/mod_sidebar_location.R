#' Sidebar location UI
#'
#' @description A shiny Module used to filter data by location. Submodule for
#' `mod_sidebar_ui()`.
#'
#' @param id Internal parameter for [shiny].
#' @param sites Dataframe with site data. Must include columns Site_ID,
#' Site_Name. Optionally may include columns State, Town, Watershed.
#' Default `NULL`.
#'
#' @seealso mod_sidebar_location_server
#'
#' @export
mod_sidebar_location_ui <- function(id, sites) {
  ns <- NS(id)

  # define vars
  state <- unique(sites$State)
  town <- unique(sites$Town)
  watershed <- unique(sites$Watershed)
  loc_choices <- set_loc_choices(sites)

  tagList(
    tabsetPanel(
      id = ns("tabset_toggle"),
      type = "hidden",
      selected = loc_tab(loc_choices),
      tabPanelBody(
        "toggle",
        radioButtons(
          ns("loc_type"),
          label = h3("Select Location"),
          choices = loc_choices
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
      selected = loc_choices[1],
      tabPanelBody(
        "town",
        conditionalPanel(
          condition = paste(length(state), "> 0"),
          dropdown(
            ns("select_state"),
            label = h4("Select State"),
            choices = state
          )
        ),
        conditionalPanel(
          condition = paste(length(town), "> 0"),
          dropdown(
            ns("select_town"),
            label = h4("Select Town"),
            choices = town
          )
        )
      ),
      tabPanelBody(
        "watershed",
        dropdown(
          ns("select_watershed"),
          label = h4("Select Watershed"),
          choices = watershed
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
          choices = sites$Site_ID,
          choice_names = sites$Site_Name
        )
      ),
      tabPanelBody(
        "sites_n",
        dropdown(
          ns("select_sites_n"),
          label = h3("Select Site"),
          choices = sites$Site_ID,
          choice_names = sites$Site_Name,
          multiple = FALSE
        )
      )
    )
  )
}

#' Sidebar location Server
#'
#' @description A shiny Module used to filter data by location. Submodule for
#' `mod_sidebar_ui()`.
#'
#' @param tab Reactive variable. Name of selected tab.
#' @param selected_site Reactive variable. Site_ID for site selected in
#' `mod_map_ui()`.
#'
#' @inheritParams mod_sidebar_location_ui
#'
#' @seealso mod_sidebar_location_ui
#'
#' @export
mod_sidebar_location_server <- function(id, sites, tab, selected_site) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set variables ----
    state <- unique(sites$State)
    town <- unique(sites$Town) %>% sort()
    watershed <- unique(sites$Watershed)

    # Toggle tabs ----
    observe({
      updateTabsetPanel(
        inputId = "tabset_loc",
        selected = input$loc_type
      )
    }) %>%
      bindEvent(input$loc_type)

    observe({
      if (tab() == "graphs") {
        updateTabsetPanel(inputId = "tabset_sites", selected = "sites_n")
      } else {
        updateTabsetPanel(inputId = "tabset_sites", selected = "sites_all")
      }
    }) %>%
      bindEvent(tab())

    # Set sites ----
    default_sites <- create_site_list(sites)
    locval <- reactiveValues(
      town_sites = default_sites,
      watershed_sites = default_sites
    )

    # * Update by state ----
    observe({
      if (is.null(town)) {
        locval$town_sites <- filter_site_list("State", input$select_state)
      } else {
        choices <- filter_towns(town, input$select_state)

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "select_town",
          choices = choices,
          selected = choices
        )
      }
    }) %>%
      bindEvent(input$select_state)

    # * Update by town ----
    observe({
      locval$town_sites <- filter_site_list("Town", input$select_town)
    }) %>%
      bindEvent(input$select_town)

    # * Update by watershed ----
    observe({
      locval$watershed_sites <- filter_site_list(
        "Watershed", input$select_watershed
      )
    }) %>%
      bindEvent(input$select_watershed)

    # * Update dropdowns ----
    site_list <- reactive({
      if (input$loc_type == "town") {
        return(locval$town_sites)
      } else {
        return(locval$watershed_sites)
      }
    }) %>%
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
    }) %>%
      bindEvent(site_list())

    # Update sites_n on map click ------
    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_sites_n",
        selected = selected_site()
      )
    }) %>%
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
