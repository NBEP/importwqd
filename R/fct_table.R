#' Add table
#'
#' @description Formats data as a `reactable` table.
#'
#' @param df Input dataframe.
#' @param show_score Boolean. If `TRUE`, shows the "score_str" column. If
#' `FALSE`, hides the "score_str" column. Default `TRUE`.
#' @param col_title Title for the "score_num" column. Default value "Average."
#'
#' @return Reactable table.
#'
#' @noRd
report_table <- function(df, show_score = TRUE, col_title = "Average") {
  reactable::reactable(
    df,
    highlight = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8"),
      na = "-"
    ),
    columns = column_styles(df, show_score),
    meta = list(col_title = col_title)
  )
}

#' Graph Table
#'
#' @description Formats graph data as a `reactable` table.
#'
#' @param df Input dataframe.
#' @param group How to group the data.
#'
#' @return Reactable table.
#'
#' @noRd
graph_table <- function(df, group) {
  df_wide <- prep_graph_table(df, group)

  reactable::reactable(
    df_wide,
    highlight = TRUE,
    defaultColDef = reactable::colDef(
      header = function(value) gsub("_", " ", value, fixed = TRUE),
      headerStyle = list(background = "#f7f7f8"),
      na = "-"
    ),
    columns = list(
      Date = reactable::colDef(
        rowHeader = TRUE,
        sticky = "left",
        style = list(borderRight = "1px solid #eee"),
        minWidth = 120
      )
    )
  )
}
