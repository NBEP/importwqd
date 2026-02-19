#' Graph UI
#'
#' @description `mod_graph_ui()` produces the UI code for the `wqdashboard`
#' graph tab.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_server()`.
#'
#' @inheritParams mod_sidebar_ui
#'
#' @export
mod_graph_ui <- function(id, varlist) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      id = ns("tabset"),
      full_screen = FALSE,
      # Site trends -----
      bslib::nav_panel(
        "Site Trends",
        value = "trend",
        mod_graph_trend_ui(ns("graph_trends"))
      ),
      # Watershed trends -----
      if (isTruthy(varlist$watershed)) {
        bslib::nav_panel(
          "Watershed Trends",
          value = "watershed",
          dropdown(
            ns("select_watershed"),
            label = h2("Select Watershed"),
            choices = varlist$watershed,
            multiple = FALSE
          ),
          mod_graph_watershed_ui(ns("graph_watershed"))
        )
      },
      # Depth ----
      if (isTruthy(varlist$depth)) {
        bslib::nav_panel(
          "Compare Depths",
          value = "depth",
          dropdown(
            ns("extra_depth"),
            label = h2("Select Depths"),
            choices = varlist$depth
          ),
          mod_graph_compare_ui(ns("graph_depth"))
        )
      },
      # Sites ----
      bslib::nav_panel(
        "Compare Sites",
        value = "site",
        dropdown(
          ns("extra_sites"),
          label = HTML(
            paste(
              h2("Select Sites"),
              "Select up to five sites"
            )
          ),
          choices = varlist$site_id,
          choice_names = varlist$site_name,
          max_options = 5
        ),
        mod_graph_compare_ui(ns("graph_sites"))
      ),
      # Parameters ----
      bslib::nav_panel(
        "Compare Parameters",
        value = "param",
        dropdown(
          ns("extra_param"),
          label = HTML(
            paste(
              h2("Select Indicators"),
              "Select two indicators"
            )
          ),
          choices = varlist$param,
          max_options = 2
        ),
        mod_graph_compare_ui(ns("graph_param"))
      ),
      bslib::nav_spacer(),
      # Options ----
      bslib::nav_menu(
        title = "Options",
        bslib::nav_item(
          conditionalPanel(
            condition = paste0('input["', ns("tabset"), '"] != "watershed"'),
            shinyWidgets::prettySwitch(
              ns("chk_line"),
              "Show lines between points",
              slim = TRUE
            )
          ),
          conditionalPanel(
            condition = paste0('input["', ns("tabset"), '"] == "trend"'),
            shinyWidgets::prettySwitch(
              ns("chk_thresh"),
              "Show thresholds",
              value = TRUE,
              slim = TRUE
            )
          ),
          conditionalPanel(
            condition = paste0(
              '["trend", "watershed"].includes(input["', ns("tabset"), '"])'
            ),
            shinyWidgets::prettySwitch(
              ns("chk_trend"),
              "Show trendline",
              value = TRUE,
              slim = TRUE
            )
          )
        )
      )
    )
  )
}

#' Graph server
#'
#' `mod_graph_server()` produces the server code for the `wqdashboard` graph
#' tab.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_graph_ui()`.
#' @param in_var Reactive output from `mod_sidebar_server`.
#'
#' @export
mod_graph_server <- function(id, in_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update dropdowns ----
    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "extra_sites",
        choices = in_var$site_list(),
        selected = in_var$sites_n()
      )
    }) |>
      bindEvent(in_var$site_list(), in_var$sites_n())

    observe({
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "extra_param",
        selected = in_var$param_n()
      )
    }) |>
      bindEvent(in_var$param_n())

    # Graph: Site Trends ----
    df_trends <- reactive({
      req(input$tabset == "trend")
      req(in_var$sites_n())
      req(in_var$param_n())

      sites <- in_var$sites_n()
      param <- in_var$param_n()

      dat <- in_var$df_graph() |>
        dplyr::filter(
          .data$Site_ID == !!sites,
          .data$Parameter == !!param
        )

      chk <- grepl("depth|height", param)
      if ("Depth" %in% colnames(dat) & !chk) {
        depth <- in_var$depth_n()
        dat <- dplyr::filter(dat, .data$Depth == !!depth)
      }

      dat
    })

    trend_style <- reactive({
      req(input$tabset == "trend")

      list(
        lines = input$chk_line,
        trend = input$chk_trend,
        thresh = input$chk_thresh
      )
    }) |>
      bindEvent(input$chk_line, input$chk_trend, input$chk_thresh, input$tabset)

    mod_graph_trend_server(
      "graph_trends",
      df = reactive({
        df_trends()
      }),
      in_var = reactive({
        trend_style()
      })
    )

    # Graph: Watershed Trends ----
    df_watershed <- reactive({
      req(input$tabset == "watershed")
      req(input$select_watershed)
      req(in_var$param_n())

      watershed <- input$select_watershed
      param <- in_var$param_n()

      dat <- in_var$df_graph() |>
        dplyr::filter(
          .data$Watershed == !!watershed,
          .data$Parameter == !!param
        )

      chk <- grepl("depth|height", param)
      if ("Depth" %in% colnames(dat) & !chk) {
        depth <- in_var$depth_n()
        dat <- dplyr::filter(dat, .data$Depth == !!depth)
      }

      dat
    })

    watershed_style <- reactive({
      req(input$tabset == "watershed")

      input$chk_trend
    }) |>
      bindEvent(input$chk_trend, input$tabset)

    mod_graph_watershed_server(
      "graph_watershed",
      df = reactive({
        df_watershed()
      }),
      trendline = reactive({
        watershed_style()
      })
    )

    # Graph: Compare Sites ----
    df_comp_sites <- reactive({
      req(input$tabset == "site")
      req(input$extra_sites)
      req(in_var$param_n())

      sites <- input$extra_sites
      param <- in_var$param_n()

      dat <- in_var$df_graph() |>
        dplyr::filter(
          .data$Site_ID %in% !!sites,
          .data$Parameter == !!param
        )

      chk <- grepl("depth|height", param)
      if ("Depth" %in% colnames(dat) & !chk) {
        depth <- in_var$depth_n()
        dat <- dplyr::filter(dat, .data$Depth == !!depth)
      }

      dat
    })

    site_style <- reactive({
      req(input$tabset == "site")

      input$chk_line
    }) |>
      bindEvent(input$chk_line, input$tabset)

    mod_graph_compare_server(
      "graph_sites",
      df = reactive({
        df_comp_sites()
      }),
      add_lines = reactive({
        site_style()
      })
    )

    # Graph: Compare Depths ----
    df_comp_depth <- reactive({
      req(input$tabset == "depth")
      req(in_var$sites_n())
      req(in_var$param_n())
      req(input$extra_depth)

      sites <- in_var$sites_n()
      param <- in_var$param_n()
      depth <- input$extra_depth

      dat <- in_var$df_graph() |>
        dplyr::filter(
          .data$Site_ID == !!sites,
          .data$Parameter == !!param,
        )

      if (!grepl("depth|height", param)) {
        dat <- dplyr::filter(dat, .data$Depth %in% !!depth)
      }

      dat
    })

    depth_style <- reactive({
      req(input$tabset == "depth")

      input$chk_line
    }) |>
      bindEvent(input$chk_line, input$tabset)

    mod_graph_compare_server("graph_depth",
      df = reactive({
        df_comp_depth()
      }),
      group = "Depth",
      add_lines = reactive({
        depth_style()
      })
    )

    # Graph: Compare Parameters ----
    df_comp_par <- reactive({
      req(input$tabset == "param")
      req(in_var$sites_n())
      req(input$extra_param)

      sites <- in_var$sites_n()
      param <- input$extra_param

      dat <- in_var$df_graph() |>
        dplyr::filter(
          .data$Site_ID == !!sites,
          .data$Parameter %in% !!param
        )

      if ("Depth" %in% colnames(dat)) {
        depth <- in_var$depth_n()
        dat <- dat |>
          dplyr::filter(
            grepl("depth|height", .data$Parameter) | .data$Depth == !!depth
          )
      }

      dat
    })

    param_style <- reactive({
      req(input$tabset == "param")

      input$chk_line
    }) |>
      bindEvent(input$chk_line, input$tabset)

    mod_graph_compare_server(
      "graph_param",
      df = reactive({
        df_comp_par()
      }),
      group = "Parameter",
      add_lines = reactive({
        param_style()
      })
    )
  })
}
