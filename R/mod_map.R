#' Map UI
#'
#' @description `mod_map_ui()` produces the map UI for the companion app
#' `wqdashboard`.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_map_server()`.
#'
#' @export
mod_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    add_js(),
    bslib::navset_card_tab(
      id = ns("tabset"),
      title = h2(textOutput(ns("title"))),
      full_screen = TRUE,
      bslib::nav_panel(
        "Map",
        leaflet::leafletOutput(ns("map"))
      ),
      bslib::nav_panel(
        "Table",
        reactable::reactableOutput(ns("table"))
      )
    )
  )
}

#' Map server
#'
#' @description `mod_map_server()` produces the map server for the
#' companion app`wqdashboard`.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_map_ui()`.
#' @param in_var Reactive output from `mod_sidebar_server`.
#' @param map_bounds Numeric list. Map boundaries. Should be ordered minimum
#' longitude, minimum latitude, maximum longitude, maximum latitude.
#' @param df_raw Dataframe. Default map data.
#' @param selected_tab Reactive string. Name of selected tab.
#' @param shp_watershed Shapefile. Watershed polygons. Set to `NULL` if
#' no shapefile available. Default `NULL`.
#' @param shp_river Shapefile. River polylines. Set to `NULL` if no
#' shapefile available. Default `NULL`.
#'
#' @returns Reactive list containing two items.
#' * graph_link: Button press for "View Trends"
#' * site: Site_ID for selected site when "View Trends" is pressed
#'
#' @export
mod_map_server <- function(
  id, in_var, map_bounds, df_raw, selected_tab, shp_watershed = NULL,
  shp_river = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set title ----
    output$title <- renderText({
      paste0(in_var$param_n(), " (", in_var$year(), ")")
    })

    # Static variables ----
    drop_col <- c(
      "Year", "Site_ID", "Parameter", "Unit", "score_typ",
      "Latitude", "Longitude", "popup_loc", "popup_score", "alt"
    )

    df_table <- dplyr::select(df_raw, !dplyr::any_of(drop_col))

    # Reactive variables ----
    val <- reactiveValues(
      df_filter = df_raw,
      df_map = df_raw,
      score_range = c(0, 1),
      score_str = "No Data Available",
      legend = "",
      dynamic_table = df_table,
      static_table = df_table,
      show_score = TRUE,
      count = 0,
      dynamic_col = "Average",
      static_tile = "Average"
    )

    # * Active tab ----
    map_tab <- reactive({
      if (selected_tab() == "map") {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    # * Most variables ----
    observe({
      req(in_var$param_n())
      req(in_var$df_map())

      dat <- in_var$df_map() |>
        dplyr::filter(!is.na(.data$score_num))

      score_min <- 0
      score_max <- 1
      score_str <- "No Data Available"
      par_unit <- ""
      col_title <- "Average"

      if (nrow(dat) > 0) {
        score_min <- min(dat$score_num, na.rm = TRUE)
        score_max <- max(dat$score_num, na.rm = TRUE)
        score_str <- unique(dat$score_str)
        par_unit <- dat$Unit[1]
        col_title <- unique_na(dat$score_typ)
      }

      if (length(col_title) > 1 || is.na(col_title)) {
        col_title <- in_var$param_n()
      }

      if (par_unit %in% c(NA, "None", "")) {
        par_unit <- ""
      } else {
        par_unit <- paste0("(", par_unit, ")")
      }

      val$score_range <- c(score_min, score_max)
      val$score_str <- score_str
      val$legend <- trimws(paste(in_var$param_n(), par_unit))
      val$dynamic_col <- trimws(paste(col_title, par_unit))
    }) |>
      bindEvent(in_var$df_map(), in_var$param_n())

    # * Dataframes ----
    observe({
      req(map_tab())
      req(in_var$sites_all())
      req(in_var$df_map())

      sites <- in_var$sites_all()

      val$df_filter <- in_var$df_map() |>
        dplyr::filter(.data$Site_ID %in% !!sites)
    }) |>
      bindEvent(
        map_tab(),
        in_var$df_map(),
        in_var$sites_all()
      )

    observe({
      if (input$tabset == "Map") {
        val$df_map <- val$df_filter
      } else {
        val$dynamic_table <- val$df_filter |>
          dplyr::select(!dplyr::any_of(drop_col))
      }
    }) |>
      bindEvent(input$tabset, val$df_filter)

    # * Map type ----
    map_type <- reactive({
      if ("No Threshold Established" %in% val$score_str) {
        return("score_num")
      } else {
        return("score_str")
      }
    }) |>
      bindEvent(val$score_str)

    # Map ----
    output$map <- leaflet::renderLeaflet({
      layer_list <- NA

      map <- leaflet::leaflet() |>
        leaflet::fitBounds(
          map_bounds$lng1,
          map_bounds$lat1,
          map_bounds$lng2,
          map_bounds$lat2
        ) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) |>
        leaflet::addScaleBar(position = "bottomleft")

      # * Add watershed ----
      if (!is.null(shp_watershed)) {
        map <- map |>
          leaflet::addPolygons(
            data = shp_watershed,
            layerId = shp_watershed,
            # Label
            label = ~Name,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Stroke
            color = "#6B8091",
            weight = 0.5,
            smoothFactor = 0.5,
            opacity = 0.9,
            # Fill
            fillOpacity = 0.4,
            fillColor = "#C6D9EC",
            # Highlight
            highlightOptions = leaflet::highlightOptions(
              fillColor = "#EEF4F9",
              weight = 1.2,
              bringToFront = FALSE
            ),
            # misc
            group = "Watersheds"
          )
        layer_list <- "Watersheds"
      }

      # * Add rivers ----
      if (!is.null(shp_river)) {
        map <- map |>
          leaflet::addMapPane("river_pane", zIndex = 420) |>
          leaflet::addPolylines(
            data = shp_river,
            layerId = shp_river,
            # Label
            label = ~Label,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            popup = ~Popup,
            # Stroke
            color = "#6B8091",
            weight = 2,
            smoothFactor = 1,
            opacity = 1,
            # Highlight
            highlightOptions = leaflet::highlightOptions(
              color = "#3e576c",
              weight = 3,
              bringToFront = TRUE
            ),
            # misc
            options = leaflet::pathOptions(pane = "river_pane"),
            group = "Rivers"
          )
        layer_list <- c(layer_list, "Rivers")
        layer_list <- layer_list[!is.na(layer_list)]
      }

      # * Add layer toggle ----
      if (!all(is.na(layer_list))) {
        map <- map |>
          leaflet::addLayersControl(
            overlayGroups = layer_list,
            position = "topleft"
          )
      }

      map
    })

    # * Add sites ----
    observe({
      if (nrow(val$df_map) == 0) {
        leaflet::leafletProxy("map") |>
          leaflet::clearMarkers()
      } else if (map_type() == "score_num") {
        leaflet::leafletProxy("map") |>
          leaflet::clearMarkers() |>
          leaflet::addMarkers(
            data = val$df_map,
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Site_ID,
            # Icons
            icon = ~ leaflet::icons(
              iconUrl = num_symbols(val$df_map, val$score_range),
              iconWidth = 20,
              iconHeight = 20
            ),
            # Label
            label = ~alt,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~ paste0(
              popup_loc,
              "<br><br><b>", in_var$param_n(), "</b>",
              popup_score, "<br>",
              actionLink(
                ns("graph_link"),
                label = "View Trends",
                onclick = paste0(
                  'Shiny.setInputValue("', ns("graph_link"),
                  '", (Math.random() * 1000) + 1);'
                )
              )
            ),
            # Accessibility
            options = leaflet::markerOptions(
              alt = ~alt,
              riseOnHover = TRUE
            )
          )
      } else {
        leaflet::leafletProxy("map") |>
          leaflet::clearMarkers() |>
          leaflet::addMarkers(
            data = val$df_map,
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Site_ID,
            # Icons
            icon = ~ leaflet::icons(
              iconUrl = cat_pal(val$score_str)[score_str],
              iconWidth = 20,
              iconHeight = 20
            ),
            # Label
            label = ~alt,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~ paste0(
              popup_loc,
              "<br><br><b>", in_var$param_n(), "</b>",
              popup_score, "<br>",
              actionLink(
                ns("graph_link2"),
                label = "View Trends",
                onclick = paste0(
                  'Shiny.setInputValue("', ns("graph_link"),
                  '", (Math.random() * 1000) + 1);'
                )
              )
            ),
            # Accessibility
            options = leaflet::markerOptions(
              alt = ~alt,
              riseOnHover = TRUE
            )
          )
      }
    }) |>
      bindEvent(
        val$df_map,
        map_type()
      )

    # * Add legend ----
    observe({
      if (map_type() == "score_num") {
        leaflet::leafletProxy("map") |>
          leaflet::clearControls() |>
          leaflegend::addLegendNumeric(
            pal = num_pal(val$score_range),
            values = val$score_range,
            title = htmltools::tags$div(
              val$legend,
              style = "font-size: 18px"
            ),
            shape = "rect",
            orientation = "vertical",
            bins = 5,
            naLabel = "No Data Available",
            labelStyle = "font-size: 14px;",
            position = "topright",
            group = "Legend"
          )
      } else {
        leaflet::leafletProxy("map") |>
          leaflet::clearControls() |>
          leaflegend::addLegendImage(
            images = cat_pal(val$score_str, TRUE),
            labels = cat_labels(val$score_str),
            width = 20,
            height = 20,
            orientation = "vertical",
            title = htmltools::tags$div(
              in_var$param_n(),
              style = "font-size: 18px"
            ),
            labelStyle = "font-size: 14px;",
            position = "topright",
            group = "Legend"
          )
      }
    }) |>
      bindEvent(
        map_type(),
        val$score_range,
        val$score_str,
        val$legend
      )

    # Table -----
    observe({
      print("map val$show_score val$static_table val$static_col")

      if (map_type() == "score_str") {
        val$show_score <- TRUE
      } else {
        val$show_score <- FALSE
      }

      val$static_table <- val$dynamic_table
      val$static_col <- val$dynamic_col
    }) |>
      bindEvent(val$dynamic_table, ignoreInit = TRUE, once = TRUE)

    output$table <- reactable::renderReactable({
      report_table(
        val$static_table,
        show_score = val$show_score,
        col_title = val$static_col
      )
    })

    # * Update table ----
    observe({
      req(input$tabset == "Table")

      print("map updateReactable")

      reactable::updateReactable(
        "table",
        data = val$dynamic_table,
        meta = list(col_title = val$dynamic_col)
      )
    }) |>
      bindEvent(
        input$tabset, val$dynamic_table, val$dynamic_col,
        ignoreInit = TRUE
      )

    observe({
      if (map_type() == "score_str" & input$tabset == "Table") {
        print("show columns")
        hideCols(ns("table"), as.list(""))
      } else if (input$tabset == "Table") {
        print("hide columns")
        hideCols(ns("table"), as.list("score_str"))
      }
    }) |>
      bindEvent(input$tabset, map_type())

    # Return data ----
    selected_site <- reactive({
      input$map_marker_click$id
    }) |>
      bindEvent(input$graph_link)

    return(
      list(
        graph_link = reactive({
          input$graph_link
        }),
        site = reactive({
          selected_site()
        })
      )
    )
  })
}
