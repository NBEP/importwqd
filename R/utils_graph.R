#' Format graph data for table
#'
#' @description `prep_graph_table()` formats graph data for use in a table.
#' Helper function for `graph_table()`.
#'
#' @param .data Dataframe
#' @param group String. Name of column used to divide the data in to groups.
#'
#' @return Updated data frame that has been pivoted wide with "Date" and unique
#' values from `group` as column headers and "Result" used as values.
#'
#' @noRd
prep_graph_table <- function(.data, group) {
  dat <- .data

  if (group == "Parameter") {
    dat <- dplyr::mutate(
      dat,
      "Parameter" = dplyr::if_else(
        is.na(.data$Unit),
        .data$Parameter,
        paste(.data$Parameter, .data$Unit)
      )
    )
  }

  col_select <- c("Date", "Result", group)
  dat <- dplyr::select(dat, dplyr::all_of(col_select))

  var_list <- unique(dat[[group]])

  if (length(var_list) == 1) {
    var_name <- dat[[group]][1]

    df_wide <- dat |>
      dplyr::select("Date", "Result") |>
      dplyr::rename({{ var_name }} := "Result")
  } else {
    df_wide <- dat |>
      tidyr::pivot_wider(
        names_from = {{ group }},
        values_from = "Result",
        values_fn = list
      )

    for (var in var_list) {
      df_wide <- df_wide |>
        dplyr::rowwise() |>
        dplyr::mutate({{ var }} := paste(.data[[var]], collapse = ", ")) |>
        dplyr::ungroup() |>
        wqformat::col_to_numeric(var)
    }

    df_wide <- data.frame(df_wide, check.names = FALSE)
  }

  df_wide
}

#' Format scatterplot lines
#'
#' @description `prep_scatter_lines()` formats data as scatterplot lines by
#' adding `NA` values on January 1 of each year.
#'
#' @param .data Dataframe
#'
#' @return Updated data frame
#'
#' @noRd
prep_scatter_lines <- function(.data) {
  if ("Depth" %in% colnames(.data)) {
    df_null <- expand.grid(
      Site_Name = unique(.data$Site_Name),
      Parameter = unique(.data$Parameter),
      Year = unique(.data$Year),
      Depth = unique(.data$Depth)
    )
  } else {
    df_null <- expand.grid(
      Site_Name = unique(.data$Site_Name),
      Parameter = unique(.data$Parameter),
      Year = unique(.data$Year)
    )
  }

  df_null <- df_null |>
    dplyr::mutate("Date" = as.Date(paste0(.data$Year, "-1-1")))

  dplyr::bind_rows(.data, df_null) |>
    data.frame() |>
    dplyr::arrange(.data$Date) |>
    dplyr::select(!"Year")
}

#' Set graph style
#'
#' @description `graph_style()` sets standard style options for all graphs.
#'
#' @param .data `Plotly` object.
#' @param fig_title String. Figure title.
#' @param y_title String. Y-axis title.
#' @param y_range Numeric list with two variables. Range for y-axis.
#'
#' @return Updated `plotly` object
#'
#' @noRd
graph_style <- function(.data, fig_title, y_title, y_range) {
  .data |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "sendDataToCloud",
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        "autoScale2d",
        "resetScale2d",
        "zoomIn2d",
        "zoomOut2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      toImageButtonOptions = list(height = 400, width = 800)
    ) |>
    plotly::layout(
      title = fig_title,
      yaxis = list(
        title = y_title,
        rangemode = "tozero",
        fixedrange = TRUE,
        range = y_range,
        titlefont = list(size = 16),
        tickfont = list(size = 16),
        linecolor = "black",
        showgrid = FALSE,
        tickcolor = "black"
      ),
      xaxis = list(
        title = "Date",
        rangemode = "tozero",
        fixedrange = TRUE,
        titlefont = list(size = 16),
        tickfont = list(size = 16),
        linecolor = "black",
        showgrid = FALSE,
        tickcolor = "black"
      ),
      showlegend = TRUE,
      hoverlabel = list(bgcolor = "white"),
      margin = list(
        l = 50, r = 20,
        b = 20, t = 55,
        pad = 0
      )
    )
}

#' Plot thresholds
#'
#' @description `plot_thresholds` generates a `plotly` object with
#' colored bars indicating the thresholds for "Does Not Meet Criteria" and
#' "Excellent".
#'
#' @param .data Dataframe
#' @param thresh List of thresholds.
#' @param date_range List. Minimum, maximum dates for x-axis.
#' @param y_range List. Minimum, maximum values for y-axis.
#' @param visible Boolean. If `TRUE`, show thresholds. If `FALSE`, hide
#' thresholds. Default `TRUE`.
#'
#' @return Updated graph.
#'
#' @noRd
plot_thresholds <- function(
  .data, thresh, date_range, y_range, visible = TRUE
) {
  fig <- plotly::plot_ly()

  if (is.null(thresh) || !visible) {
    return(fig)
  }

  thresh_min <- thresh$thresh_min
  thresh_max <- thresh$thresh_max
  thresh_excellent <- thresh$thresh_exc
  thresh_best <- thresh$thresh_best

  min_date <- date_range[1]
  max_date <- date_range[2]
  min_val <- y_range[1]
  max_val <- y_range[2]

  if (!is.na(thresh_min) & min_val < thresh_min) {
    fig <- fig |>
      plotly::add_polygons(
        x = c(min_date, max_date, max_date, min_date),
        y = c(thresh_min, thresh_min, min_val, min_val),
        line = list(width = 0),
        fillcolor = "#f6c0c0",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Does Not Meet Criteria",
        inherit = FALSE,
        name = "Does Not Meet\nCriteria",
        legendrank = 1003
      )
  }

  if (!is.na(thresh_max) & max_val > thresh_max) {
    fig <- fig |>
      plotly::add_polygons(
        x = c(min_date, max_date, max_date, min_date),
        y = c(thresh_max, thresh_max, max_val, max_val),
        line = list(width = 0),
        fillcolor = "#f6c0c0",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Does Not Meet Criteria",
        inherit = FALSE,
        name = "Does Not\nMeet Criteria",
        legendrank = 1002
      )
  }

  if (is.na(thresh_best)) {
    return(fig)
  }

  if (thresh_best == "low" & thresh_excellent > min_val) {
    fig <- fig |>
      plotly::add_polygons(
        x = c(min_date, max_date, max_date, min_date),
        y = c(thresh_excellent, thresh_excellent, min_val, min_val),
        line = list(width = 0),
        fillcolor = "#dde8fe",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Excellent",
        inherit = FALSE,
        name = "Excellent",
        legendrank = 1001
      )
  } else if (thresh_best == "high" & thresh_excellent < max_val) {
    fig <- fig |>
      plotly::add_polygons(
        x = c(min_date, max_date, max_date, min_date),
        y = c(thresh_excellent, thresh_excellent, max_val, max_val),
        line = list(width = 0),
        fillcolor = "#dde8fe",
        visible = visible,
        hoverinfo = "text",
        hovertext = "Excellent",
        inherit = FALSE,
        name = "Excellent",
        legendrank = 1001
      )
  }

  fig
}

#' Add trend line (GAM)
#'
#' @description `add_gam()` calculates GAM and adds a smooth trend line with
#' 95% confidence interval to a `plotly` graph.
#'
#' @param fig Plotly graph.
#' @param df Dataframe.
#'
#' @return Updated plotly graph with a trendline
#'
#' @noRd
add_gam <- function(fig, df) {
  df <- dplyr::mutate(df, "Dec_Date" = lubridate::decimal_date(.data$Date))

  # Code from Carmen Chan
  # https://www.displayr.com/how-to-add-trend-lines-in-r-using-plotly/

  df_gam <- mgcv::gam(df$Result ~ s(df$Dec_Date))
  df_pred <- stats::predict(df_gam, type = "response", se.fit = TRUE)
  df_new <- data.frame(
    x = df_gam$model[, 2],
    y = df_pred$fit,
    lb = as.numeric(df_pred$fit - (1.96 * df_pred$se.fit)),
    ub = as.numeric(df_pred$fit + (1.96 * df_pred$se.fit))
  ) |>
    dplyr::mutate("x" = lubridate::date_decimal(.data$x)) |>
    dplyr::arrange(.data$x)

  fig |>
    plotly::add_trace(
      data = df_new,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#2c2c2c",
        width = 2,
        dash = "dash"
      ),
      visible = TRUE,
      inherit = FALSE,
      name = "Trend Line (GAM)"
    ) |>
    plotly::add_ribbons(
      data = df_new,
      x = ~x,
      ymin = ~lb,
      ymax = ~ub,
      line = list(
        color = "#818181",
        opacity = 0.4,
        width = 0
      ),
      fillcolor = list(
        color = "#818181",
        opacity = 0.4
      ),
      visible = TRUE,
      inherit = FALSE,
      name = "95% Confidence\nInterval"
    )
}

#' Threshold text
#'
#' @description `thresh_text()` is a helper function for
#' `mod_graph_trend_server()` that lists the thresholds used for the selected
#' parameter.
#'
#' @param thresh List of threshold values
#'
#' @return List of threshold values.
#'
#' @noRd
thresh_text <- function(thresh) {
  thresh_min <- thresh$thresh_min
  thresh_max <- thresh$thresh_max
  thresh_best <- thresh$thresh_best
  thresh_excellent <- thresh$thresh_exc
  unit <- thresh$unit

  thresh_text <- ""

  if (!is.na(thresh_min) & !is.na(thresh_max)) {
    thresh_text <- paste0(
      thresh_text, "<b>Acceptable:</b> ", pretty_number(thresh_min), " - ",
      pretty_number(thresh_max), " ", unit
    )
  } else if (!is.na(thresh_min)) {
    thresh_text <- paste0(
      thresh_text, "<b>Acceptable:</b> &gt; ", pretty_number(thresh_min),
      " ", unit
    )
  } else if (!is.na(thresh_max)) {
    thresh_text <- paste0(
      thresh_text, "<b>Acceptable:</b> &lt; ", pretty_number(thresh_max),
      " ", unit
    )
  }

  thresh_text <- trimws(thresh_text)

  if (is.na(thresh_best)) {
    return(thresh_text)
  }

  if (thresh_text != "") {
    thresh_text <- paste0(trimws(thresh_text), "<br>")
  }

  if (thresh_best == "low") {
    thresh_text <- paste0(
      thresh_text, "<b>Excellent:</b> &lt; ", pretty_number(thresh_excellent),
      " ", unit
    )
  } else if (thresh_best == "high") {
    thresh_text <- paste0(
      thresh_text, "<b>Excellent:</b> &gt; ", pretty_number(thresh_excellent),
      " ", unit
    )
  }

  trimws(thresh_text)
}
