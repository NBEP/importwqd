#' Format graph data for table
#'
#' @description `prep_graph_table()` formats graph data for use in a table.
#' Helper function for _____.
#'
#' @param .data Input dataframe.
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
        .data$Unit %in% c(NA, "", "None"),
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

    df_wide <- dat %>%
      dplyr::select("Date", "Result") %>%
      dplyr::rename({{ var_name }} := "Result")
  } else {
    df_wide <- dat %>%
      tidyr::pivot_wider(
        names_from = {{ group }},
        values_from = "Result",
        values_fn = list
      )

    for (var in var_list) {
      df_wide <- df_wide %>%
        dplyr::rowwise() %>%
        dplyr::mutate({{ var }} := paste(.data[[var]], collapse = ", ")) %>%
        dplyr::ungroup() %>%
        wqformat::col_to_numeric(var)
    }

    df_wide <- data.frame(df_wide)
  }

  df_wide
}
