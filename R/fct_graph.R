#' Graph trends
#'
#' @description `graph_trends()` creates a scatterplot that includes thresholds
#' and trendlines.
#'
#' @param .data Dataframe with raw data.
#' @param thresholds Dataframe with threshold values.
#' @param trendline Boolean. If `TRUE`, adds trendline. If `FALSE`,
#' overrides `display$trend` and does not add trendline. Default `TRUE`.
#' @param display List with three items: lines, trend, thresh
#' * `lines`: Boolean. If `TRUE`, displays scatter plot as lines +
#' markers. If `FALSE`, displays scatter plot as markers only.
#' * `trend`: Boolean. If `TRUE`, adds trendline.
#' * `thresh`: Boolean. If `TRUE`, adds red bar to indicate values outside
#' acceptable range and a blue bar to indicate excellent values. Default
#' `FALSE`.
#'
#' @return Scatterplot
#'
#' @noRd
graph_trends <- function(
  .data, thresholds, trendline = TRUE,
  display = list(lines = FALSE, trend = TRUE, thresh = TRUE)
) {
  if (nrow(.data) == 0) {
    return(NULL)
  }

  if (!display$trend) {
    trendline <- FALSE
  }

  # Set variables
  param <- .data$Parameter[1]
  unit <- .data$Unit[1]
  y_range <- val_range(.data)

  min_date <- min(.data$Date)
  max_date <- max(.data$Date)

  if (min_date == max_date) {
    date_diff <- lubridate::dmonths(1)
  } else {
    date_diff <- lubridate::interval(min_date, max_date) |>
      lubridate::as.duration()
    date_diff <- date_diff * 0.06
  }

  min_date <- min_date - date_diff
  max_date <- max_date + date_diff

  # Create plot
  fig <- plot_thresholds(
    .data,
    thresh = thresholds,
    date_range = c(min_date, max_date),
    y_range = y_range,
    visible = display$thresh
  )

  if (display$lines) {
    df_new <- prep_scatter_lines(.data)

    fig <- fig |>
      plotly::add_trace(
        data = df_new,
        x = ~Date,
        y = ~Result,
        type = "scatter",
        mode = "lines+markers",
        inherit = FALSE,
        name = ~ stringr::str_wrap(Site_Name, 20),
        marker = list(size = 7, color = "#2daebe"),
        line = list(color = "#2daebe"),
        hoverinfo = "text",
        hovertext = ~Description
      )
  } else {
    fig <- fig |>
      plotly::add_trace(
        data = .data,
        x = ~Date,
        y = ~Result,
        type = "scatter",
        mode = "markers",
        inherit = FALSE,
        name = ~ stringr::str_wrap(Site_Name, 20),
        marker = list(size = 7, color = "#2daebe"),
        hoverinfo = "text",
        hovertext = ~Description
      )
  }


  # Add trendlines
  if (trendline && display$trend) {
    fig <- add_gam(fig, .data)
  }

  # Style plot
  fig |>
    graph_style(
      fig_title = param,
      y_title = pretty_unit(param, unit),
      y_range = list(y_range)
    ) |>
    plotly::layout(
      xaxis = list(range = c(min_date, max_date))
    )
}

#' Graph with multiple traces
#'
#' @description `graph_compare()` creates a scatterplot with a multiple traces
#' and a single Y-axis.
#'
#' @param .data Dataframe
#' @param fig_title String. Graph title.
#' @param group String. Column used to group data.
#' @param add_lines Boolean. If `TRUE`, displays scatter plot as lines +
#' markers. If `FALSE`, displays scatter plot as markers only.
#'
#' @return Scatterplot
#'
#' @noRd
graph_compare <- function(
  .data, fig_title, group = "Site_Name", add_lines = FALSE
) {
  if (nrow(.data) == 0) {
    return(NULL)
  }

  # Set variables
  param <- .data$Parameter[1]
  unit <- .data$Unit[1]
  graph_mode <- "markers"

  y_range <- val_range(.data)

  # Set palette
  group_len <- length(unique(.data[[group]]))
  pal <- c("#2daebe", "#6f3d61", "#f5b400", "#cf4e13", "#2c2c2c")
  pal <- pal[1:group_len]
  shapes <- c("circle", "square", "diamond", "triangle-up", "x")
  shapes <- shapes[1:group_len]

  # Create plot
  if (add_lines) {
    .data <- prep_scatter_lines(.data)
    graph_mode <- "lines+markers"
  }
  dat <- dplyr::mutate(.data, "Group" = .data[[group]])

  plotly::plot_ly(
    data = dat,
    name = ~ stringr::str_wrap(Group, 20),
    x = ~Date,
    y = ~Result,
    type = "scatter",
    mode = graph_mode,
    color = ~Group,
    colors = pal,
    symbol = ~Group,
    symbols = shapes,
    marker = list(size = 7),
    hoverinfo = "text",
    hovertext = ~Description
  ) |>
    graph_style(
      fig_title = fig_title,
      y_title = pretty_unit(param, unit),
      y_range = list(y_range)
    )
}

#' Graph parameters
#'
#' @description `graph_two_axis()` creates a scatterplot that compares up to
#' two parameters. Each paramater is assigned its own y-axis.
#'
#' @param .data Dataframe
#' @param fig_title String. Graph title.
#' @param add_lines Boolean. If `TRUE`, displays scatter plot as lines +
#' markers. If `FALSE`, displays scatter plot as markers only.
#'
#' @return Scatterplot
#'
#' @noRd
graph_param <- function(.data, fig_title, add_lines = FALSE) {
  if (nrow(.data) == 0) {
    return(NULL)
  }

  # Set variables
  par1 <- .data$Parameter[1]
  unit1 <- .data$Unit[1]
  graph_mode <- "markers"

  if (add_lines) {
    graph_mode <- "lines+markers"
    .data <- prep_scatter_lines(.data)
  }

  df1 <- dplyr::filter(.data, .data$Parameter == !!par1)
  df2 <- dplyr::filter(.data, .data$Parameter != !!par1)

  pal <- "#2daebe"
  shapes <- c("circle", "diamond")
  if (nrow(df2) > 0) {
    pal <- c("#2daebe", "#2c2c2c")
  }

  # Create graph with first parameter
  fig <- plotly::plot_ly(
    data = df1,
    name = stringr::str_wrap(par1, 20),
    type = "scatter",
    mode = graph_mode,
    x = ~Date,
    y = ~Result,
    color = ~Parameter,
    colors = pal,
    symbol = ~Parameter,
    symbols = shapes,
    marker = list(size = 7),
    hoverinfo = "text",
    hovertext = ~Description
  )

  # Add second parameter
  if (nrow(df2) > 0) {
    par2 <- df2$Parameter[1]
    unit2 <- unique_na(df2$Unit)

    fig <- fig |>
      plotly::add_trace(
        data = df2,
        name = stringr::str_wrap(par2, 20),
        type = "scatter",
        mode = graph_mode,
        yaxis = "y2",
        x = ~Date,
        y = ~Result,
        color = ~Parameter,
        colors = pal,
        symbol = ~Parameter,
        symbols = shapes,
        marker = list(size = 7),
        hoverinfo = "text",
        hovertext = ~Description
      ) |>
      plotly::layout(
        yaxis2 = list(
          title = pretty_unit(par2, unit2),
          overlaying = "y",
          side = "right",
          rangemode = "tozero",
          fixedrange = TRUE,
          titlefont = list(size = 16),
          tickfont = list(size = 16),
          linecolor = "black",
          showgrid = FALSE,
          tickcolor = "black"
        ),
        legend = list(
          xanchor = "left",
          x = 1.15
        )
      )
  }

  # Calculate axes, style plot
  graph_style(
    fig,
    fig_title = fig_title,
    y_title = pretty_unit(par1, unit1),
    y_range = NA
  )
}
