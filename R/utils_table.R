#' Style table columns
#'
#' @description `column_styles()` is a helper function for `add_table()` that
#' sets the column style.
#'
#' @param df Input dataframe.
#' @param show_score Boolean. If `TRUE`, show "score_str" column. If `FALSE`,
#' hide "score_str" column. Default `TRUE`.
#'
#' @return Formatted list of column styles.
#'
#' @noRd
column_styles <- function(df, show_score = TRUE) {
  col_style <- list(
    Site_Name = reactable::colDef(
      rowHeader = TRUE,
      sticky = "left",
      style = list(borderRight = "1px solid #eee")
    ),
    score_str = reactable::colDef(
      name = "Score",
      show = show_score,
      style = htmlwidgets::JS(
        "function(rowInfo) {
          if (rowInfo.values['score_str'] == 'Excellent') {
            return { backgroundColor: '#afccec' }
          } else if (rowInfo.values['score_str'] == 'Good' |
              rowInfo.values['score_str'] == 'Meets Criteria') {
            return { backgroundColor: '#cbe4e7' }
          } else if (rowInfo.values['score_str'] == 'Fair') {
            return { backgroundColor: '#ffffe0' }
          } else if (rowInfo.values['score_str'] == 'Poor' |
              rowInfo.values['score_str'] == 'Does Not Meet Criteria') {
            return { backgroundColor: '#f9cfb4' }
          } else if (rowInfo.values['score_str'] == 'No Data Available' |
              rowInfo.values['score_str'] == 'No Threshold Established') {
            return { fontStyle: 'italic' }
          }
        }"
      )
    )
  )

  col_loc <- c("Town", "State", "Watershed", "Group")
  col_loc <- intersect(colnames(df), col_loc)
  if (length(col_loc) > 0) {
    col_right <- col_loc[length(col_loc)]
    col_style[[col_right]] <- reactable::colDef(
      style = list(borderRight = "1px solid #eee")
    )
  }

  if ("score_num" %in% colnames(df)) {
    col_style[["score_num"]] <- reactable::colDef(
      name = "Value",
      na = "-",
      header = htmlwidgets::JS(
        "function(column, state) {
          const { col_title } = state.meta
          return col_title
        }"
      )
    )
  }

  col_style
}
