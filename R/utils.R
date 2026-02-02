#' Round number to two significant digits
#'
#' @description `pretty_number()` rounds numbers to two decimal points. If a
#' number is between -1 and 1, it is instead rounded to two significant digits.
#'
#' @param x Number to round
#'
#' @return Rounded number
#'
#' @noRd
pretty_number <- function(x) {
  dplyr::if_else(
    abs(x) < 1,
    signif(x, 2),
    round(x, 2)
  )
}

#' Concatentate paramter, unit
#'
#' @description `pretty_unit()` concatenates parameter and unit as
#' "parameter (unit)". If `unit` is `NA`, "", " ", or "None", formats string as
#' "parameter".
#'
#' @param parameter String. Parameter.
#' @param unit String. Unit.
#'
#' @return String containing parameter and unit.
#'
#' @noRd
pretty_unit <- function(parameter, unit) {
  if (unit %in% c(NA, "", " ", "None")) {
    return(parameter)
  }

  paste0(parameter, " (", unit, ")")
}

#' Find unique values and drop NA
#'
#' @description `unique_na()` find unique values in a list and removes `NA`
#' values. If all values are `NA`, returns `NA`.
#'
#' @param x List
#'
#' @return Unique values in a list, with `NA` values removed. If all values are
#' `NA`, returns `NA`
#'
#' @noRd
unique_na <- function(x) {
  x <- unique(x)

  if (length(x) > 1) {
    x <- x[!is.na(x)]
  }

  x
}
