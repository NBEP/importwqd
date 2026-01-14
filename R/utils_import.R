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

#' Standardize threshold units
#'
#' @description `update_threshold_units()` updates threshold data so that
#' each parameter uses the same units as those used in `result_data`. Helper
#' function for `format_results()`.
#'
#' @param .data Dataframe
#' @param result_data Dataframe containing result data.
#'
#' @returns Updated dataframe. Unit and threshold values will be updated to
#' match the units used in `result_data`.
#'
#' @noRd
update_threshold_units <- function(.data, result_data) {
  result_units <- result_data %>%
    dplyr::group_by(.data$Parameter) %>%
    dplyr::summarize("temp_unit" = dplyr::last(.data$Result_Unit))

  dat <- dplyr::inner_join(.data, result_units, by = "Parameter")

  if (nrow(dat) == 0) {
    dat <- dplyr::select(dat, !"temp_unit")
    return(dat)
  }

  dat <- suppressWarnings(
    wqformat::standardize_units_across(
      dat,
      "temp_unit",
      "Unit",
      c("Min", "Max", "Excellent", "Good", "Fair"),
      unit_format = "wqdashboard"
    )
  )

  chk <- dat$Unit == dat$temp_unit | is.na(dat$Unit) | is.na(dat$temp_unit)
  if (any(!chk)) {
    bad_row <- dat[which(!chk), ]
    bad_param <- unique(bad_row$Parameter)

    warning(
      "Removed thresholds for ", paste(bad_param, collapse = ", "),
      " due to incompatible units",
      call. = FALSE
    )
    dat <- dat[which(chk), ]
  }

  dplyr::select(dat, !"temp_unit")
}

#' Add threshold values
#'
#' @description
#' `add_thresholds()` checks stored threshold values, and locates the relevant
#' threshold values for a given site, parameter, and depth. If no thresholds
#' found, returns NULL.
#'
#' @param site_id String. Site ID.
#' @param group String. Site group.
#' @param state String. State where the site is located.
#' @param depth String. Depth category.
#' @param parameter String. Parameter.
#'
#' @return One row dataframe with list of thresholds for provided site,
#' parameter, and depth. If no thresholds found, returns NULL.
#'
#' @noRd
add_thresholds <- function(
  thresholds, site_id, group, state, depth, parameter
) {
  dat <- thresholds %>%
    dplyr::filter(
      .data$Parameter == parameter,
      is.na(.data$State) | .data$State == state,
      is.na(.data$Group) | .data$Group == group,
      is.na(.data$Site) | .data$Site == site_id,
      is.na(.data$Depth) | .data$Depth == depth
    )

  if (nrow(dat) == 0) {
    return(
      list(
        Calculation = "mean",
        Min = NA,
        Max = NA,
        Excellent = NA,
        Good = NA,
        Fair = NA,
        Best = NA
      )
    )
  }

  list(
    Calculation = dat$Calculation[1],
    Min = dat$Min[1],
    Max = dat$Max[1],
    Excellent = dat$Excellent[1],
    Good = dat$Good[1],
    Fair = dat$Fair[1],
    Best = dat$Best[1]
  )
}

#' Rename variables in column
#'
#' @description `try_rename()` is a helper function for `prep_thresholds()` and
#' `prep_results()` that renames the variables in a column according to a paired
#' list of variable names in `df_var`.
#'
#' @param .data Input dataframe
#' @param col_name Column name.
#' @param df_var Dataframe. Must include "wqdashboard" and "Custom"
#' columns with a paired list of variable names.
#'
#' @return Updated dataframe. If `df_var` doesn't contain paired variable names
#' or `col_name` does not exist, the original dataframe is returned.
#'
#' @noRd
try_rename <- function(.data, col_name, df_var) {
  df_var <- df_var %>%
    dplyr::filter(!is.na(.data$wqdashboard) & !is.na(.data$Custom))

  if (!col_name %in% colnames(.data)) {
    message("\tDid not find ", col_name)
    return(.data)
  } else if (nrow(df_var) == 0) {
    message("\tDid not update ", col_name)
    return(.data)
  }

  .data %>%
    wqformat::update_var(col_name, df_var$Custom, df_var$wqdashboard)
}
