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
