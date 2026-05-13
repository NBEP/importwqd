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
  dup1 <- df |>
    dplyr::select(dplyr::all_of(col_list)) |>
    duplicated()

  if (any(dup1)) {
    dup2 <- df |>
      dplyr::select(dplyr::all_of(col_list)) |>
      duplicated(fromLast = TRUE)

    dup_rws <- c(which(dup1), which(dup2)) |>
      unique() |>
      sort()

    msg <- paste0(
      "Duplicate values found for ", paste(col_list, collapse = ", "),
      ": check rows ", paste(dup_rws, collapse = ", ")
    )

    if (is_stop == TRUE) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
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

  msg <- paste(toString(chk), "empty rows in", col_name)
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

  if (!include_na && length(chk) > 1) {
    chk <- chk[!is.na(chk)]
  }

  if (length(chk) < 2) {
    message("\tRemoved homogenous column: ", col_name)
    .data[col_name] <- NULL
  }

  .data
}

#' Standardize threshold units
#'
#' @description `update_threshold_units()` updates threshold data so that
#' each parameter uses the same units as those used in `result_data`. Helper
#' function for `format_results()`.
#'
#' @param .data Dataframe containing thresholds
#' @param result_data Dataframe containing result data
#'
#' @returns Updated dataframe. Unit and threshold values will be updated to
#' match the units used in `result_data`.
#'
#' @noRd
update_threshold_units <- function(.data, result_data) {
  result_units <- result_data |>
    dplyr::group_by(.data$Parameter) |>
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

#' Combine thresholds
#'
#' @description `combine_thresholds()` groups thresholds by location, depth,
#' and parameter. Grouped thresholds are combined on a single row.
#'
#' @param .data Dataframe containing thresholds
#'
#' @returns Updated dataframe
#'
#' @noRd
combine_thresholds <- function(.data) {
  chk <- .data |>
    dplyr::count(
      .data$State, .data$Group, .data$Site, .data$Parameter, .data$Depth
    )

  if (max(chk$n) < 2) {
    dat <- .data |>
      dplyr::mutate(
        dplyr::across(
          "Min":"Fair",
          ~ as.character(.)
        )
      )

    return(dat)
  }

  .data |>
    dplyr::group_by(
      .data$State, .data$Group, .data$Site, .data$Depth, .data$Parameter,
      .data$Unit
    ) |>
    dplyr::summarise(
      "Calculation" = paste(.data$Calculation, collapse = ", "),
      "Min" = paste(.data$Min, collapse = ", "),
      "Max" = paste(.data$Max, collapse = ", "),
      "Excellent" = paste(.data$Excellent, collapse = ", "),
      "Good" = paste(.data$Good, collapse = ", "),
      "Fair" = paste(.data$Fair, collapse = ", "),
      "Best" = paste(.data$Best, collapse = ", "),
    ) |>
    dplyr::mutate(
      dplyr::across(
        "Calculation":"Best",
        ~ dplyr::na_if(., "NA")
      )
    ) |>
    data.frame()
}

#' Add threshold values
#'
#' @description
#' `add_thresholds()` checks stored threshold values, and locates the relevant
#' threshold values for a given site, parameter, and depth. If no thresholds
#' found, returns `NULL`.
#'
#' @param site_id String. Site ID.
#' @param group String. Site group.
#' @param state String. State where the site is located.
#' @param depth String. Depth category.
#' @param parameter String. Parameter.
#'
#' @return One row dataframe with list of thresholds for provided site,
#' parameter, and depth. If no thresholds found, returns `NULL`.
#'
#' @noRd
add_thresholds <- function(
  thresholds, site_id, group, state, depth, parameter
) {
  dat <- thresholds |>
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

#' Add depth category
#'
#' @description
#' `add_depth_category()` is a helper function for `qaqc_results()` and
#' `qaqc_cat_results()` that updates the "Depth", "Depth Unit", and "Depth
#' Category" columns.
#'
#' @param .data Dataframe. Must include columsn "Depth", "Depth Unit", and
#' "Depth Category"
#' @param df_sites Dataframe containing site data.
#'
#' @return updated dataframe where "Depth" and "Depth Unit" have been converted
#' to meters and empty values in "Depth Category" have been filled out if
#' possible.
#'
#' @noRd
add_depth_category <- function(.data, df_sites) {
  chk <- is.na(.data$Depth)
  if (all(chk)) {
    .data$Depth <- NA_integer_
    .data$Depth_Unit <- "m"
    return(.data)
  }

  dat <- .data |>
    wqformat::col_to_numeric("Depth", silent = FALSE) |>
    wqformat::set_units("Depth", "Depth_Unit", "m", unit_format = "wqdashboard")

  depth_cat <- c("Surface", "Midwater", "Near Bottom", "Bottom")

  wqformat::warn_invalid_var(dat, "Depth_Unit", "m")
  wqformat::warn_invalid_var(dat, "Depth_Category", depth_cat)

  depth_col <- c("Max_Surface_Depth_m", "Max_Midwater_Depth_m", "Max_Depth_m")

  df_sites <- dplyr::select(df_sites, dplyr::any_of(c("Site_ID", depth_col)))

  dplyr::left_join(dat, df_sites, by = "Site_ID", keep = FALSE) |>
    dplyr::mutate(
      "Depth_Category" = dplyr::case_when(
        grepl("depth|height", .data$Parameter, ignore.case = TRUE) ~ NA,
        !is.na(.data$Depth_Category) ~ .data$Depth_Category,
        is.na(.data$Depth) | is.na(.data$Depth_Unit) |
          .data$Depth_Unit != "m" ~ NA,
        is.na(.data$Max_Depth_m) & is.na(.data$Max_Midwater_Depth_m) &
          is.na(.data$Max_Surface_Depth_m) ~ NA,
        !is.na(.data$Max_Depth_m) & .data$Depth >= .data$Max_Depth_m ~ "Bottom",
        !is.na(.data$Max_Midwater_Depth_m) &
          .data$Depth > .data$Max_Midwater_Depth_m ~ "Near Bottom",
        !is.na(.data$Max_Surface_Depth_m) &
          .data$Depth > .data$Max_Surface_Depth_m ~ "Midwater",
        TRUE ~ "Surface"
      )
    ) |>
    dplyr::select(!dplyr::any_of(depth_col))
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
  df_var <- df_var |>
    dplyr::filter(!is.na(.data$wqdashboard) & !is.na(.data$Custom))

  if (!col_name %in% colnames(.data)) {
    message("\tDid not find ", col_name)
    return(.data)
  } else if (nrow(df_var) == 0) {
    message("\tDid not update ", col_name)
    return(.data)
  }

  .data |>
    wqformat::update_var(col_name, df_var$Custom, df_var$wqdashboard)
}

#' Calculate geometric mean
#'
#' @description `geo_mean()` calculates the geometric mean.
#'
#' @param x Numeric list
#' @param zero_sub Integer. Number to substitute for zero when calculating
#' geometric mean. Default value 1.
#'
#' @return Geometric mean
#'
#' @noRd
geo_mean <- function(x) {
  chk <- x <= 0
  if (all(chk)) {
    return(0)
  }

  x[x <= 0] <- NA

  zero_sub <- min(x, na.rm = TRUE) / 10
  if (zero_sub > 1) {
    zero_sub <- 1
  }

  x[is.na(x)] <- zero_sub

  exp(mean(log(x)))
}

#' Calculate score
#'
#' @description `calculate_score()` is a helper function for `score_results()`.
#'
#' @param score_max,score_min,score_mean,score_median,score_geomean,score_90p
#' Integer. Annual maximum, minimum, mean, median, geometric mean, and
#' 90th percentile values.
#' @param calculation String. Which score(s) to use when assessing thresholds.
#' @param thresh_min,thresh_max,thresh_excellent,thresh_good,thresh_fair,thresh_best
#' String or integer. Threshold values used to assess scores. Must be the same
#' order and length as `calculation`.
#' @param param_unit String. Unit. Default `NA`.
#'
#' @return List containing four items: score_typ, score_num, score_str, and
#' score_desc.
#'
#' @noRd
calculate_score <- function(
  score_max, score_min, score_mean, score_median, score_geomean, score_90p,
  calculation, thresh_min, thresh_max, thresh_excellent,
  thresh_good, thresh_fair, thresh_best, param_unit = NA
) {
  chk <- grepl(",", calculation)
  if (chk) {
    calculation <- split_string(calculation)
    thresh_min <- split_string(thresh_min, as_integer = TRUE)
    thresh_max <- split_string(thresh_max, as_integer = TRUE)
    thresh_excellent <- split_string(thresh_excellent, as_integer = TRUE)
    thresh_good <- split_string(thresh_good, as_integer = TRUE)
    thresh_fair <- split_string(thresh_fair, as_integer = TRUE)
    thresh_best <- split_string(thresh_best)
  }

  calc_num <- NULL
  calc_str <- NULL
  calc_typ <- NULL
  calc_desc <- NULL

  for (i in seq_along(calculation)) {
    temp_typ <- calculation[i]
    temp_min <- thresh_min[i]
    temp_max <- thresh_max[i]
    temp_excellent <- thresh_excellent[i]
    temp_good <- thresh_good[i]
    temp_fair <- thresh_fair[i]
    temp_best <- thresh_best[i]

    temp_num <- dplyr::case_when(
      is.na(temp_typ) ~ score_mean,
      temp_typ == "max" ~ score_max,
      temp_typ == "min" ~ score_min,
      temp_typ == "median" ~ score_median,
      temp_typ == "geomean" ~ score_geomean,
      temp_typ == "90p" ~ score_90p,
      TRUE ~ score_mean
    )

    temp_typ <- dplyr::case_when(
      temp_typ == "min" ~ "Minimum",
      temp_typ == "max" ~ "Maximum",
      temp_typ == "median" ~ "Median",
      temp_typ == "mean" ~ "Average",
      temp_typ == "geomean" ~ "Geometric Mean",
      temp_typ == "90p" ~ "90th Percentile",
      TRUE ~ temp_typ
    )

    temp_desc <- dplyr::case_when(
      is.na(temp_num) ~ NA,
      is.na(param_unit) ~ paste0(temp_typ, ": ", temp_num),
      TRUE ~ paste0(temp_typ, ": ", temp_num, " ", param_unit)
    )

    temp_str <- dplyr::case_when(
      is.na(temp_best) | is.na(temp_num) ~ NA,
      temp_best == "high" & temp_num >= temp_excellent ~ "5_Excellent",
      temp_best == "high" & temp_num >= temp_good ~ "4_Good",
      temp_best == "high" & temp_num >= temp_fair ~ "3_Fair",
      temp_best == "high" ~ "1_Poor",
      temp_best == "low" & temp_num <= temp_excellent ~ "5_Excellent",
      temp_best == "low" & temp_num <= temp_good ~ "4_Good",
      temp_best == "low" & temp_num <= temp_fair ~ "3_Fair",
      temp_best == "low" ~ "1_Poor",
      TRUE ~ NA
    )

    temp_str <- dplyr::case_when(
      !is.na(temp_str) | is.na(temp_num) ~ temp_str,
      is.na(temp_min) & is.na(temp_max) ~ NA,
      !is.na(temp_min) & temp_num < temp_min ~
        "0_Does Not Meet Criteria",
      !is.na(temp_max) & temp_num > temp_max ~
        "0_Does Not Meet Criteria",
      TRUE ~ "2_Meets Criteria"
    )

    calc_num <- c(calc_num, pretty_number(temp_num))
    calc_str <- c(calc_str, temp_str)
    calc_typ <- c(calc_typ, temp_typ)
    calc_desc <- c(calc_desc, temp_desc)
  }

  chk <- is.na(calc_str)
  if (any(!chk)) {
    calc_str <- min(calc_str, na.rm = TRUE)
    calc_str <- substr(calc_str, 3, nchar(calc_str))
  }

  chk <- is.na(calc_desc)
  if (all(chk)) {
    calc_desc <- NA
  } else {
    calc_desc <- calc_desc[!is.na(calc_desc)]
    calc_desc <- paste(unique(calc_desc), collapse = "<br>")
  }

  list(
    score_typ = calc_typ[1],
    score_num = calc_num[1],
    score_str = calc_str,
    score_desc = calc_desc
  )
}
