#' Convert unit
#'
#' @description `convert_unit()` uses values in `unit_conversion` to convert
#' units.
#'
#' @param x Numeric. Value to convert.
#' @param old_unit String. Current unit.
#' @param new_unit String. New unit.
#' @param is_stop If TRUE, provides error message for invalid results. Default
#' TRUE.
#'
#' @return Updated value.
#'
#' @noRd
convert_unit <- function(x, old_unit, new_unit, is_stop = TRUE) {
  if (old_unit == new_unit) {
    return(x)
  }

  # Standardize names
  var_names <- find_var_names(varnames_units, "WQX", "measurements")

  old_unit <- rename_var(old_unit, var_names$old_names, var_names$new_names)
  new_unit <- rename_var(new_unit, var_names$old_names, var_names$new_names)

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

  if (class(y) != "try-error") {
    return(y)
  } else if (is_stop) {
    stop("Unable to convert ", old_unit, " to ", new_unit, call. = FALSE)
  } else {
    return(NA)
  }
}

#' Standardize Units
#'
#' @description Checks if more than one `Result_Unit` per `Parameter`. If more
#'   than one unit listed, standardizes parameter units according to most recent
#'   result.
#'
#' @param df Dataframe
#'
#' @noRd
standardize_units <- function(df) {
  chk <- skip_rows(df)
  df2 <- df[which(!chk), ]

  df_temp <- df2 %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarise("temp_unit" = dplyr::last(Result_Unit))

  df2 <- dplyr::left_join(df2, df_temp, by = dplyr::join_by(Parameter)) %>%
    dplyr::filter(Result_Unit != temp_unit)

  if (nrow(df2) == 0) {
    return(df)
  }

  df2 <- df2 %>%
    dplyr::select(Parameter, Result, Result_Unit, temp_unit) %>%
    unique() %>%
    dplyr::mutate(
      temp_result = mapply(
        function(x, y, z) convert_unit(x, y, z),
        Result, Result_Unit, temp_unit
      )
    )

  chk <- is.na(df2$temp_result)
  if (any(chk)) {
    rws <- which(chk)
    df_error <- df2[rws, ]
    stop("Only one unit allowed per parameter. Unable to standardize units for:\n\t-",
         paste(unique(df_error$Parameter), collapse = "\n\t-"),
         call. = FALSE
    )
  }

  df <- dplyr::left_join(
    df,
    df2,
    by = dplyr::join_by(Parameter, Result, Result_Unit)
  ) %>%
    dplyr::mutate(
      Result = dplyr::if_else(
        is.na(temp_result),
        Result,
        temp_result
      )
    ) %>%
    dplyr::mutate(
      Result_Unit = dplyr::if_else(
        is.na(temp_unit),
        Result_Unit,
        temp_unit
      )
    ) %>%
    dplyr::select(!c(temp_unit, temp_result))

  return(df)
}
