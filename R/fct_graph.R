#' Graph Trends
#'
#' @description `graph_trends()` creates a scatterplot that includes thresholds
#' and trendlines.
#'
#' @param .data Dataframe.
#' @param fig_title Title of graph.
#' @param thresholds Boolean. If TRUE, adds red bar to indicate values outside
#'  acceptable range. Default FALSE.
#'
#' @return Scatterplot.
#'
#' @noRd
graph_trends <- function(
  .data, thresholds, show_thresh = TRUE, create_trend = TRUE,
  show_trend = TRUE
) {
  if (nrow(.data) == 0) {
    return(NULL)
  }

  # Set variables
  param <- .data$Parameter[1]
  unit <- .data$Unit[1]

  min_val <- min(.data$Result) * .8
  if (min_val > 0 & param != "pH") {
    min_val <- 0
  }

  max_val <- max(.data$Result) * 1.2
  if (max_val == min_val) {
    max_val <- min_val + 1
  }

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

  df_new <- prep_scatter_lines(.data)

  # Create plot
  fig <- plot_thresholds(
    .data,
    thresh = thresholds,
    date_range = c(min_date, max_date),
    y_range = c(min_val, max_val),
    visible = show_thresh
  ) |>
    plotly::add_trace(
      data = df_new,
      x = ~Date,
      y = ~Result,
      type = "scatter",
      mode = "lines",
      inherit = FALSE,
      name = ~ stringr::str_wrap(Site_Name, 20),
      line = list(color = "#2daebe")
    ) |>
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

  # Add trendlines
  if (create_trend) {
    fig <- add_gam(fig, .data, show_trend)
  }

  # Style plot
  graph_style(
    fig,
    fig_title = param,
    y_title = pretty_unit(param, unit),
    y_range = list(min_val, max_val)
  ) |>
    plotly::layout(
      xaxis = list(range = c(min_date, max_date))
    )
}

#' #' graph_one_var
#' #'
#' #' @description Creates a scatterplot using `plotly`.
#' #'
#' #' @param df Dataframe.
#' #' @param thresholds Boolean. If TRUE, adds red bar to indicate values outside
#' #'  acceptable range. Default FALSE.
#' #'
#' #' @return Scatterplot.
#' #'
#' #' @noRd
#'
#' graph_one_var <- function(df, fig_title, group = "Site_Name") {
#'   if (nrow(df) == 0) {
#'     return(NULL)
#'   }
#'
#'   # Set variables ----
#'   min_val <- min(df$Result) * .8
#'   if (min_val > 0) {
#'     min_val <- 0
#'   }
#'   max_val <- max(df$Result) * 1.2
#'   if (max_val == min_val) {
#'     max_val <- min_val + 1
#'   }
#'
#'   df_new <- add_line_breaks(df)
#'   df_new["Group"] <- df_new[[group]]
#'
#'   # Set line/marker palette, shape, and dash ----
#'   group_len <- length(unique(df_new$Group))
#'   pal <- c("#2daebe", "#6f3d61", "#f5b400", "#cf4e13", "#2c2c2c")
#'   pal <- pal[1:group_len]
#'   shapes <- c("circle", "square", "diamond", "triangle-up", "x")
#'   shapes <- shapes[1:group_len]
#'
#'   # Create plot ----
#'   fig <- plotly::plot_ly(
#'     data = df_new,
#'     name = ~ stringr::str_wrap(Group, 20),
#'     x = ~Date,
#'     y = ~Result,
#'     type = "scatter",
#'     mode = "lines+markers",
#'     color = ~Group,
#'     colors = pal,
#'     symbol = ~Group,
#'     symbols = shapes,
#'     marker = list(size = 7),
#'     hoverinfo = "text",
#'     hovertext = ~Description
#'   )
#'
#'   # Style plot ----
#'   fig <- graph_style(fig,
#'                      fig_title = fig_title,
#'                      y_title = pretty_unit(df$Parameter[1], df$Unit[1]),
#'                      y_range = list(min_val, max_val)
#'   )
#'
#'   return(fig)
#' }
#'
#' #' graph_two_var
#' #'
#' #' @description Creates a scatterplot with two y-axises using `plotly`.
#' #'
#' #' @param df Dataframe.
#' #' @param fig_title String. Title of graph.
#' #'
#' #' @return Scatterplot.
#' #'
#' #' @noRd
#'
#' graph_two_var <- function(df, fig_title) {
#'   if (nrow(df) == 0) {
#'     return(NULL)
#'   }
#'   par1 <- df$Parameter[1]
#'   unit1 <- df$Unit[1]
#'
#'   df_new <- add_line_breaks(df)
#'   df1 <- dplyr::filter(df_new, Parameter == par1)
#'   df2 <- dplyr::filter(df_new, Parameter != par1)
#'
#'   pal <- "#2daebe"
#'   shapes <- c("circle", "diamond")
#'   if (nrow(df2) > 0) {
#'     pal <- c("#2daebe", "#2c2c2c")
#'   }
#'
#'   # Create graph with first parameter
#'   fig <- plotly::plot_ly(
#'     data = df1,
#'     name = stringr::str_wrap(par1, 20),
#'     type = "scatter",
#'     mode = "lines+markers",
#'     x = ~Date,
#'     y = ~Result,
#'     color = ~Parameter,
#'     colors = pal,
#'     symbol = ~Parameter,
#'     symbols = shapes,
#'     marker = list(size = 7),
#'     hoverinfo = "text",
#'     hovertext = ~Description
#'   )
#'
#'   # Add second parameter
#'   if (nrow(df2) > 0) {
#'     par2 <- df2$Parameter[1]
#'     unit2 <- unique(df2$Unit)
#'     unit2 <- unit2[!is.na(unit2)]
#'
#'     fig <- fig |>
#'       plotly::add_trace(
#'         data = df2,
#'         name = stringr::str_wrap(par2, 20),
#'         type = "scatter",
#'         mode = "lines+markers",
#'         yaxis = "y2",
#'         x = ~Date,
#'         y = ~Result,
#'         color = ~Parameter,
#'         colors = pal,
#'         symbol = ~Parameter,
#'         symbols = shapes,
#'         marker = list(size = 7),
#'         hoverinfo = "text",
#'         hovertext = ~Description
#'       ) |>
#'       plotly::layout(
#'         yaxis2 = list(
#'           title = pretty_unit(par2, unit2),
#'           overlaying = "y",
#'           side = "right",
#'           rangemode = "tozero",
#'           fixedrange = TRUE,
#'           titlefont = list(size = 16),
#'           tickfont = list(size = 16),
#'           linecolor = "black",
#'           showgrid = FALSE,
#'           tickcolor = "black"
#'         ),
#'         legend = list(
#'           xanchor = "left",
#'           x = 1.15
#'         )
#'       )
#'   }
#'
#'   # Calculate axes, style plot
#'   fig <- graph_style(
#'     fig,
#'     fig_title = fig_title,
#'     y_title = pretty_unit(par1, unit1),
#'     y_range = NA
#'   )
#'
#'   return(fig)
#' }
