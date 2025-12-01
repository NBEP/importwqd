#' Add column with popup text
#'
#' @description
#' `popup_text()` generates and formats descriptive popup text for each
#' row in a dataframe. It updates `target_col` by concatenating the existing
#' text with the column name and value for each column in `col_list`.
#' * Unless `style` is updated, each column/value pair is formatted as
#' "<b>column name:</b> column value"
#' * A delimiter (`<br>`) is added between each line of text.
#'
#' @param .data Dataframe.
#' @param col_list List or string. Columns used to populate values in popup
#' text. Will be added in order listed.
#' @param col_title List or string. The title to place at the start of each new
#' line. Default `NULL`.
#' * If list, must be same length as `col_list`.
#' * If `NULL`, `col_title` will be replaced with `col_list`. Underscores will
#' be replaced with spaces.
#' @param target_col String. Name of column to modify and add text to. If
#' column does not exist, it will be added to the dataframe. Default "Popup".
#' @param na_value String. Replacement for `NA` values. Default "-".
#' @param hide_na Boolean. If `TRUE`, will not add a new line of text if
#' `in_data` is `NA`. Default `TRUE`.
#' @param style String. How to format each line of data. String must include
#' "in_title" and "in_data" to represent the title and data value respectively.
#' Default "<b>in_title:</b> in_data".
#'
#' @return Updated dataframe with column named "Popup" unless `target_col`
#' set to different value. "Popup" column contains formatted popup text.
#'
#' @export
popup_text <- function(
    .data, col_list, col_title = NULL, target_col = "Popup", na_value = "-",
    hide_na = FALSE, style = "<b>in_title:</b> in_data") {
  dat <- .data

  # Check for errors
  col_list <- intersect(col_list, colnames(dat))
  if (length(col_list) == 0) {
    return(.data)
  }

  if (is.null(col_title)) {
    col_title <- gsub("_", " ", col_list)
  } else if (length(col_title) != length(col_list)) {
    stop("col_list and col_title must be same length")
  }

  chk <- grepl("in_title", style) & grepl("in_data", style)
  if (any(!chk)) {
    stop("style must include in_title and in_data")
  }

  # Set variables
  names(col_list) <- col_title

  # Update dataframe
  if (!target_col %in% colnames(dat)) {
    dat[[target_col]] <- NA
  }

  for (i in names(col_list)) {
    j <- col_list[[i]] # i = col_title, j = col_list

    dat <- dat %>%
      dplyr::mutate(
        {{ target_col }} := mapply(
          function(w, x, y) {
            format_popup(w, x, y, na_value, hide_na, style)
          },
          .data[[target_col]], !!i, .data[[j]],
          USE.NAMES = FALSE
        )
      )
  }

 dat
}
