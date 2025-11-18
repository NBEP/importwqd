#' Standardize result units
#'
#' @description `standardize_result_units()` updates result data so that each
#' parameter uses a single, standard unit for all results.
#'
#' @param .data Dataframe
#'
#' @returns Updated dataframe. Unit and result values will be updated to ensure
#' each parameter uses a single, consitent unit.
standardize_result_units <- function(.data) {
  # Set var
  dat_temp <- dplyr::filter(.data, !is.na(.data$Result))

  # List parameters with multiple units, set target unit
  param_unit <- dat_temp %>%
    dplyr::group_by(.data$Parameter) %>%
    dplyr::summarize(
      "n" = length(unique(.data$Result_Unit)),
      "temp_unit" = dplyr::last(.data$Result_Unit)
    ) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::select(!"n")

  if (nrow(param_unit) == 0) {
    return(.data)
  }

  # Simplify data, convert units
  dat_temp <- dat_temp %>%
    dplyr::select(c("Parameter", "Result", "Result_Unit")) %>%
    unique()

  dat_temp <- dplyr::inner_join(dat_temp, param_unit, by = "Parameter") %>%
    dplyr::filter(.data$Result_Unit != .data$temp_unit) %>%
    dplyr::mutate(
      "temp_result" = mapply(
        function(x, y, z) convert_unit(x, y, z),
        .data$Result, .data$Result_Unit, .data$temp_unit
      )
    )

  # Check - any data fail to convert?
  chk <- dat_temp$temp_result == -999999
  if (any(chk)) {
    bad_par <- dat_temp[which(chk), ]
    bad_par <- unique(bad_par$Parameter)
    stop(
      "Unable to standardize units for ", paste(bad_par, collapse = ","),
      call. = FALSE
    )
  }

  # Join updated, old data
  join_col <- c("Parameter", "Result", "Result_Unit")
  dat <- dplyr::left_join(.data, dat_temp, by = join_col)

  # Update units, results
  dat <- dat %>%
    dplyr::mutate(
      "Result" = dplyr::if_else(
        is.na(.data$temp_result),
        .data$Result,
        .data$temp_result
      )
    ) %>%
    dplyr::mutate(
      "Result_Unit" = dplyr::if_else(
        is.na(.data$temp_unit),
        .data$Result_Unit,
        .data$temp_unit
      )
    ) %>%
    dplyr::select(!dplyr::any_of(c("temp_unit", "temp_result")))

  # Check - any rows missing unit?
  chk <- is.na(dat$Result) | is.na(dat$Result_Unit)

  if (all(!chk)) {
    return(dat)
  }

  # Patch in missing units
  dplyr::left_join(dat, param_unit, by = "Parameter") %>%
    dplyr::mutate(
      "Result_Unit" = dplyr::if_else(
        is.na(.data$temp_unit),
        .data$Result_Unit,
        .data$temp_unit
      )
    ) %>%
    dplyr::select(!"temp_unit")
}

#' Standardize detection limit units
#'
#' @description `standardize_detection_units()` updates result data so that
#' each row uses the same units for results and detection limits.
#'
#' @param .data Dataframe
#'
#' @returns Updated dataframe. Detection limit and detection limit unit will
#' be updated to match that row's result unit.
standardize_detection_units <- function(.data) {
  # Check - all rows okay as is?
  chk <- is.na(.data$Detection_Limit_Unit) |
    .data$Detection_Limit_Unit == .data$Result_Unit
  if (all(chk)) {
    return(.data)
  }

  # Split data according to whether it needs to be updated
  dat <- dplyr::mutate(.data, "temp_row" = dplyr::row_number())

  dat1 <- dat[which(chk), ]
  dat2 <- dat[which(!chk), ] %>%
    dplyr::mutate(
      "temp_lower" = mapply(
        function(x, y, z) convert_unit(x, y, z),
        .data$Lower_Detection_Limit, .data$Detection_Limit_Unit,
        .data$Result_Unit
      )
    ) %>%
    dplyr::mutate(
      "temp_upper" = mapply(
        function(x, y, z) convert_unit(x, y, z),
        .data$Upper_Detection_Limit, .data$Detection_Limit_Unit,
        .data$Result_Unit
      )
    )

  # Check - any rows fail to update data?
  chk <- (!is.na(dat2$temp_lower) & dat2$temp_lower == -999999) |
    (!is.na(dat2$temp_upper) & dat2$temp_upper == -999999)
  if (any(chk)) {
    bad_par <- dat2[which(chk), ]
    bad_par <- unique(bad_par$temp_row)
    stop(
      "Result and detection units are incompatible. Check rows: ",
      paste(bad_par, collapse = ", "),
      call. = FALSE
    )
  }

  dat2 <- dat2 %>%
    dplyr::mutate(
      "Lower_Detection_Limit" = dplyr::if_else(
        is.na(.data$temp_lower),
        .data$Lower_Detection_Limit,
        .data$temp_lower
      )
    ) %>%
    dplyr::mutate(
      "Upper_Detection_Limit" = dplyr::if_else(
        is.na(.data$temp_upper),
        .data$Upper_Detection_Limit,
        .data$temp_upper
      )
    ) %>%
    dplyr::mutate(
      "Detection_Limit_Unit" = dplyr::if_else(
        is.na(.data$Result_Unit),
        .data$Detection_Limit_Unit,
        .data$Result_Unit
      )
    ) %>%
    dplyr::select(!c("temp_upper", "temp_lower"))

  # Combine data
  rbind(dat1, dat2) %>%
    dplyr::arrange(.data$temp_row) %>%
    dplyr::select(!"temp_row")
}
