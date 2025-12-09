#' Prepare threshold metadata
#'
#' @description `prep_trhesholds()` prepares site data for use in `wqdashboard`
#' by updating parameter and unit names.
#'
#' @inheritParams prep_results
#'
#' @return Updated dataframe
#'
#' @export
prep_thresholds <- function(.data, df_param, df_unit) {
  message("Preparing thresholds...")

  .data %>%
    try_rename("Parameter", df_param) %>%
    try_rename("Unit", df_unit)
}

#' Check thresholds metadata for formatting errors
#'
#' @description `qaqc_thresholds()` checks imported threshold metadata for major
#' formatting errors. Specifically, it runs the following checks:
#' * Checks for missing columns
#' * Checks for missing values
#' * Checks if rows contain logical threshold values
#'
#' @param .data Input dataframe
#' @param in_format String. Input format used for parameters, units. Default
#' "wqdashboard"
#'
#' @return Updated dataframe
#'
#' @export
qaqc_thresholds <- function(.data, in_format = "wqdashboard") {
  message("Checking thresholds...")

  # Define variables
  field_all <- c(
    "State", "Group", "Site_ID", "Depth_Category", "Parameter", "Unit",
    "Calculation", "Threshold_Min", "Threshold_Max", "Excellent", "Good", "Fair"
  )

  # Check - missing columns
  check_col_missing(.data, c("Parameter", "Unit"))

  chk <- c("Threshold_Min", "Threshold_Max") %in% colnames(.data)
  chk <- any(chk) | all(c("Excellent", "Good", "Fair") %in% colnames(.data))
  if (!chk) {
    stop(
      "Data must include at least one threshold column",
      call. = FALSE
    )
  }

  # Check - missing data (error)
  check_val_missing(.data, "Parameter")
  check_val_missing(.data, "Unit")

  # Update columns
  missing_col <- setdiff(field_all, colnames(.data))
  .data[missing_col] <- NA

  dat <- .data %>%
    wqformat::update_param("Parameter", in_format, "wqdashboard") %>%
    wqformat::update_unit("Unit", in_format, "wqdashboard") %>%
    wqformat::state_to_abb("State") %>%
    wqformat::col_to_numeric("Threshold_Min", silent = FALSE) %>%
    wqformat::col_to_numeric("Threshold_Max", silent = FALSE) %>%
    wqformat::col_to_numeric("Excellent", silent = FALSE) %>%
    wqformat::col_to_numeric("Good", silent = FALSE) %>%
    wqformat::col_to_numeric("Fair", silent = FALSE) %>%
    dplyr::mutate("Calculation" = tolower(.data$Calculation)) %>%
    dplyr::select(dplyr::all_of(field_all))

  # Check - bad data (error)
  chk <- is.na(dat$Site_ID) | is.na(dat$Group)
  if (any(!chk)) {
    stop(
      "Site and group thresholds must be on seperate rows. Check rows: ",
      paste(which(!chk), collapse = ", "),
      call. = FALSE
    )
  }

  chk <- is.na(dat$Site_ID) | is.na(dat$State)
  if (any(!chk)) {
    stop(
      "Site and state thresholds must be on seperate rows. Check rows: ",
      paste(which(!chk), collapse = ", "),
      call. = FALSE
    )
  }

  check_val_duplicate(dat, c("State", "Group", "Site_ID", "Parameter"))

  chk <- is.na(dat$Excellent) | is.na(dat$Good) | is.na(dat$Fair)
  chk <- any(chk) & is.na(dat$Threshold_Min) & is.na(dat$Threshold_Max)
  if (any(chk)) {
    stop(
      "Each row must include at least one threshold value. Check rows: ",
      paste(which(chk), collapse = ", "),
      call. = FALSE
    )
  }

  chk <- is.na(dat$Excellent) | is.na(dat$Good) | is.na(dat$Fair) |
    (dat$Excellent > dat$Fair & dat$Excellent >= dat$Good &
      dat$Good >= dat$Fair) |
    (dat$Excellent < dat$Fair & dat$Excellent <= dat$Good &
      dat$Good <= dat$Fair)

  if (any(!chk)) {
    stop(
      "Illogical values for Excellent, Good, and Fair. Check rows: ",
      paste(which(!chk), collapse = ", ")
    )
  }

  calc_list <- c(
    "min", "minimum", "max", "maximum", "mean", "average", "median"
  )
  wqformat::warn_invalid_var(dat, "Calculation", calc_list)

  # Check - missing data (warning)
  chk <- is.na(.data$State)
  if (!all(chk)) {
    check_val_missing(dat, "State", is_stop = FALSE)
  }

  chk <- is.na(.data$Calculation)
  if (!all(chk)) {
    check_val_missing(dat, "Calculation", is_stop = FALSE)
  }

  dat
}

#' Format threshold metadata for use in wqdashboard
#'
#' @description `format_thresholds()` formats threshold metadata for use in
#' [wqdashboard].
#'
#' @param .data Input dataframe
#'
#' @return Updated dataframe
#'
#' @export
format_thresholds <- function(.data) {
  message("Formatting thresholds...")

  .data %>%
    dplyr::rename(
      "Site" = "Site_ID",
      "Depth" = "Depth_Category",
      "Min" = "Threshold_Min",
      "Max" = "Threshold_Max"
    ) %>%
    dplyr::arrange_at(c("Site", "State", "Group", "Depth")) %>%
    dplyr::mutate(
      "Calculation" = dplyr::case_when(
        .data$Calculation %in% c(NA, "average") ~ "mean",
        .data$Calculation == "minimum" ~ "min",
        .data$Calculation == "maximum" ~ "max",
        TRUE ~ .data$Calculation
      )
    ) %>%
    dplyr::mutate(
      "Best" = dplyr::case_when(
        is.na(.data$Excellent) | is.na(.data$Good) | is.na(.data$Fair) ~ NA,
        .data$Excellent > .data$Fair ~ "high",
        .data$Excellent < .data$Fair ~ "low",
        TRUE ~ NA
      )
    )
}

#' Standardize threshold units
#'
#' @description `update_threshold_units()` updates threshold data so that
#' each parameter uses the same units as those used in `result_data`.
#'
#' @param .data Dataframe
#' @param result_data Dataframe containing result data.
#'
#' @returns Updated dataframe. Unit and threshold values will be updated to
#' match the units used in `result_data`.
#'
#' @export
update_threshold_units <- function(.data, result_data) {
  result_units <- result_data %>%
    dplyr::group_by(.data$Parameter) %>%
    dplyr::summarize("temp_unit" = dplyr::last(.data$Unit))

  dat <- dplyr::left_join(.data, result_units, by = "Parameter")

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
    warning(
      "Removed ", length(which(!chk)), " thresholds due to incompatible units",
      call. = FALSE
    )
    dat <- dat[which(chk), ]
  }

  dplyr::select(dat, !"temp_unit")
}
