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
#' @param sbVar Reactive output from `mod_sidebar_server`.
#' @param siteVar Static list. Coordinate boundaries for site data.
#' @param df_raw Static dataframe. Default data.
#' @param map_tab Reactive boolean. If map tab is selected and data should be
#' updated, set to `TRUE`. Else, set to `FALSE`.
#' @param shp_watershed Static shapefile. Watershed polygons. Set to `NULL` if
#' no shapefile available. Default `NULL`.
#' @param shp_river Static shapefile. River polylines. Set to `NULL` if no
#' shapefile available. Default `NULL`
#'
#' @returns Reactive list containing two items.
#' * graph_link: Button press for "View Trends"
#' * site: Site_ID for selected site when "View Trends" is pressed
#'
#' @export
mod_map_server <- function(
    id, sbVar, siteVar, df_raw, map_tab, shp_watershed = NULL,
    shp_river = NULL
  ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns <-
      # Set title ----
      output$title <- renderText({
        paste0(sbVar$param_n(), " (", sbVar$year(), ")")
      })

    # Static variables ----
    drop_col <- c(
      "Year", "Site_ID", "Parameter", "Unit", "score_typ",
      "Latitude", "Longitude", "popup_loc", "popup_score", "alt"
    )

    df_table_raw <- df_raw %>%
      dplyr::filter(.data$Year == max(.data$Year)) %>%
      dplyr::select(!dplyr::any_of(drop_col))

    # Reactive variables ----
    mapVar <- reactiveValues(
      df_map = df_raw,
      score_range = c(0, 1),
      score_str = "No Data Available",
      legend = "",
      df_table = df_table_raw,
      show_score = TRUE,
      static_col = "Average",
      dynamic_col = "Average"
    )

    # * Most variables ----
    observe({
      dat <- sbVar$df_map() %>%
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
        col_title <- dat$score_typ[1]
      }

      if (par_unit %in% c(NA, "None", "")) {
        par_unit <- ""
      } else {
        par_unit <- paste0("(", par_unit, ")")
      }

      mapVar$score_range <- c(score_min, score_max)
      mapVar$score_str <- score_str
      mapVar$legend <- trimws(paste(sbVar$param_n, par_unit))
      tableVar$dynamic_col <- trimws(paste(col_title, par_unit))
    }) %>%
      bindEvent(sbVar$param_n(), sbVar$df_map())

    # * df_map ----
    observe({
      req(sbVar$sites_all())

      if (map_tab()) {
        sites <- sbVar$sites_all()

        mapVar$df_map <- sbVar$df_map() %>%
          dplyr::filter(.data$Site_ID %in% !!sites)
      }
    }) %>%
      bindEvent(
        map_tab(),
        sbVar$df_map(),
        sbVar$sites_all()
      )

    # * Map type ----
    map_type <- reactive({
      chk <- "No Threshold Established" %in% mapVar$score_str
      if (chk) {
        return("score_num")
      } else {
        return("score_str")
      }
    }) %>%
      bindEvent(mapVar$score_str)

    # * Default table ----
    observe({
      mapVar$df_table <- dplyr::select(mapVar$df_map, !dplyr::any_of(drop_col))
      mapVar$static_col <- mapVar$dynamic_col
      if (map_type() == "score_str") {
        mapVar$show_score <- TRUE
      } else {
        mapVar$show_score <- FALSE
      }
    }) %>%
      bindEvent(input$tabset, ignoreInit=TRUE, once=TRUE)

    # Map ----
    output$map <- leaflet::renderLeaflet({
      layer_list <- NA

      map <- leaflet::leaflet() %>%
        leaflet::fitBounds(
          siteVar$lon_min,
          siteVar$lat_min,
          siteVar$lon_max,
          siteVar$lat_max
        ) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet::addScaleBar(position = "bottomleft")

      # * Add watershed ----
      if (!is.null(shp_watershed)) {
        map <- map %>%
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
        map <- map %>%
          leaflet::addMapPane("river_pane", zIndex = 420) %>%
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
        map <- map %>%
          leaflet::addLayersControl(
            overlayGroups = layer_list,
            position = "topleft"
          )
      }

      map
    })

    # * Add sites ----
    observe({
      if (nrow(mapVar$df_map) == 0) {
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers()
      } else if (map_type() == "score_num") {
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = mapVar$df_map,
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Site_ID,
            # Icons
            icon = ~ leaflet::icons(
              iconUrl = num_symbols(mapVar$df_map, mapVar$score_range),
              iconWidth = 20,
              iconHeight = 20
            ),
            # Label
            label = ~alt,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~ paste0(
              popup_loc,
              "<br><br><b>", sbVar$param_n(), "</b>",
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
        leaflet::leafletProxy("map") %>%
          leaflet::clearMarkers() %>%
          leaflet::addMarkers(
            data = mapVar$df_map,
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~Site_ID,
            # Icons
            icon = ~ leaflet::icons(
              iconUrl = cat_pal(mapVar$score_str())[score_str],
              iconWidth = 20,
              iconHeight = 20
            ),
            # Label
            label = ~alt,
            labelOptions = leaflet::labelOptions(textsize = "15px"),
            # Popup
            popup = ~ paste0(
              popup_loc,
              "<br><br><b>", sbVar$param_n(), "</b>",
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
    }) %>%
      bindEvent(
        mapVar$df_map,
        map_type()
      )

    # * Add legend ----
    observe({
      if (map_type() == "score_num") {
        leaflet::leafletProxy("map") %>%
          leaflet::clearControls() %>%
          leaflegend::addLegendNumeric(
            pal = num_pal(mapVar$score_range),
            values = mapVar$score_range,
            title = htmltools::tags$div(
              mapVar$legend,
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
        leaflet::leafletProxy("map") %>%
          leaflet::clearControls() %>%
          leaflegend::addLegendImage(
            images = cat_pal(mapVar$score_str, TRUE),
            labels = cat_labels(mapVar$score_str),
            width = 20,
            height = 20,
            orientation = "vertical",
            title = htmltools::tags$div(
              sbVar$param_n(),
              style = "font-size: 18px"
            ),
            labelStyle = "font-size: 14px;",
            position = "topright",
            group = "Legend"
          )
      }
    }) %>%
      bindEvent(
        map_type(),
        mapVar$score_range,
        mapVar$score_str,
        mapVar$legend
      )

    # Table -----
    output$table <- reactable::renderReactable({
      report_table(
        mapVar$df_table,
        show_score = mapVar$show_score,
        col_title = mapVar$col_title
      )
    })

    # * Update table ----
    observe({
      reactable::updateReactable(
        "table",
        data = dplyr::select(mapVar$df_map, !dplyr::any_of(drop_col)),
        meta = list(col_title = mapVar$dynamic_col)
      )
    }) %>%
      bindEvent(mapVar$df_map, mapVar$dynamic_col)

    observe({
      if (map_type() == "score_str") {
        hideCols(ns("table"), as.list(""))
      } else {
        hideCols(ns("table"), as.list("score_str"))
      }
    }) %>%
      bindEvent(map_type())

    # Return data ----
    selected_site <- reactive({
      input$map_marker_click$id
    }) %>%
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
