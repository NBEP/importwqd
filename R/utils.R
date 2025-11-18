#' Check for missing columns
#'
#' @description `check_col_missing()` produces an error message if any columns
#' are missing.
#'
#' @param df Input dataframe.
#' @param col_list List of column names.
#'
#' @return If any columns are missing, returns an error message, else does not
#' return anything.
#'
#' @noRd
check_col_missing <- function(df, col_list) {
  chk <- col_list %in% colnames(df)

  if (any(!chk)) {
    missing_col <- col_list[!chk]
    stop(
      "\tThe following columns are missing: ",
      paste(missing_col, collapse = ", "),
      call. = FALSE
    )
  }
}

#' Check columns for duplicate values
#'
#' @description `check_val_duplicate()` produces an error message if there are
#' any duplicate rows across the input column(s).
#'
#' @param df Input dataframe.
#' @param col_list List. Column names.
#' @param is_stop Boolean. If `TRUE`, returns an error message. If `FALSE`,
#' returns a warning.
#'
#' @returns If encounters duplicate rows, returns warning or error message, else
#' doesn't return anything.
#'
#' @noRd
check_val_duplicate <- function(df, col_list, is_stop = TRUE) {
  dup1 <- df %>%
    dplyr::select(dplyr::all_of(col_list)) %>%
    duplicated()

  if (any(dup1)) {
    dup2 <- df %>%
      dplyr::select(dplyr::all_of(col_list)) %>%
      duplicated(fromLast = TRUE)

    dup_rws <- c(which(dup1), which(dup2)) %>%
      unique() %>%
      sort()

    msg <- paste0(
      "Duplicate values found for ", paste(col_list, collapse = ", "),
      ": check rows ", paste(dup_rws, collapse = ", ")
    )

    if (is_stop == TRUE) {
      stop(msg, call. = FALSE)
    } else {
      warning("\t", msg, call. = FALSE)
    }
  }
}

#' Check column for blank cells
#'
#' @description `check_val_missing()` produces an error message if there are
#' any blank cells in the selected column.
#'
#' @param df Input dataframe.
#' @param col_name String. Column name.
#' @param ignore_rows Numeric list.
#' @param is_stop Boolean. If `TRUE`, returns error message. If `FALSE`, returns
#' warning. Default `TRUE`.
#'
#' @return If any blank cells are detected, returns an error or warning message,
#' else returns nothing.
#'
#' @noRd
check_val_missing <- function(.data, col_name, ignore_rows = NA,
                              is_stop = TRUE) {
  chk <- is.na(.data[col_name])

  if (all(!chk)) {
    return(.data)
  }

  rws <- which(chk)
  rws <- setdiff(rws, ignore_rows)

  chk <- length(rws)
  if (chk == 0) {
    return(.data)
  }

  msg <- paste(toString(chk), "empty rows detected in", col_name)
  if (chk < 20) {
    msg <- paste0(msg, ". Check rows: ", paste(rws, collapse = ", "))
  }

  if (is_stop == TRUE) {
    stop(msg, call. = FALSE)
  } else {
    warning("\t", msg, call. = FALSE)
  }
}

#' Drop column if empty
#'
#' @description `check_empty_col()` removes the selected column if it is empty.
#'
#' @param .data Input dataframe.
#' @param col_name String. Column name.
#'
#' @return Updated dataframe.
#'
#' @noRd
drop_empty_col <- function(.data, col_name) {
  chk <- is.na(.data[[col_name]])

  if (all(chk)) {
    message("\tRemoved blank column: ", col_name)
    .data[col_name] <- NULL
  }

  .data
}

#' Drop column if all values are identical
#'
#' @description `drop_uniform_col()` checks if the selected column only contains
#' one unique value; if `TRUE`, drops the column from the dataframe.
#'
#' @param .data Input dataframe.
#' @param col_name String. Column name.
#' @param include_na Boolean. If `TRUE`, `NA` values count towards the total
#' number of unique values in the column. Default `TRUE`.
#'
#' @return Updated dataframe.
#'
#' @noRd
drop_uniform_col <- function(.data, col_name, include_na = TRUE) {
  chk <- unique(.data[[col_name]])

  if (!include_na && !all(is.na(chk))) {
    chk <- chk[!is.na(chk)]
  }

  if (length(chk) < 2) {
    message("\tRemoved homogenous column: ", col_name)
    .data[col_name] <- NULL
  }

  return(.data)
}

#' Convert unit
#'
#' @description `convert_unit()` converts data to new unit. Helper function for
#' `standardize_result_units()` and `standardize_detection_units()`.
#'
#' @param x Numeric. Value to convert.
#' @param old_unit String. Current unit.
#' @param new_unit String. New unit.
#'
#' @return If `old_unit` and `new_unit` are compatible, returns updated value.
#' If they are incompatible, returns `-999999`.
#'
#' @noRd
convert_unit <- function(x, old_unit, new_unit) {
  if (is.na(x) || is.na(old_unit) || is.na(new_unit)) {
    return(NA)
  } else if (old_unit == new_unit) {
    return(x)
  }

  # Update names to work with measurements::conv_unit()
  if (old_unit %in% names(wqd_units)) {
    old_unit <- wqd_units[names(wqd_units) == old_unit]
  }

  if (new_unit %in% names(wqd_units)) {
    new_unit <- wqd_units[names(wqd_units) == new_unit]
  }

  # Run conversion
  chk <- grepl("/", c(old_unit, new_unit))

  if (any(chk)) {
    y <- try(
      measurements::conv_multiunit(x, old_unit, new_unit),
      silent = TRUE
    )
  } else {
    y <- try(
      measurements::conv_unit(x, old_unit, new_unit),
      silent = TRUE
    )
  }

  if (inherits(y, "try-error")) {
    return(-999999)
  }

  y
}
