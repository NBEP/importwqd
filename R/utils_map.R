#' Continuous palette
#'
#' `num_pal()` generates a colorblind safe palette for continuous data.
#'
#' @param pal_range Numeric list. Minimum, maximum values for continuous data.
#'
#' @returns Icon code.
#'
#' @noRd
num_pal <- function(pal_range) {
  x <- pal_range[1]
  y <- pal_range[2]

  if (x == y) {
    if (x >= 1) {
      x <- x - 1
    }
    y <- y + 1
  }

  leaflet::colorNumeric(
    palette = c(
      "#b2fd99", "#97e39d", "#7dcaa1", "#63b1a5", "#4997a9",
      "#417ea0", "#47638c", "#4e4876", "#55285d"
    ),
    domain = c(x, y),
    # bins = 5,
    na.color = "#f4f4f4"
  )
}

#' Icon shape
#'
#' `num_shape()` is a helper function for `num_symbols` that sets `NA` values
#' as crosses and all other values as circles.
#'
#' @param x Integer
#'
#' @returns Shape name. If `x` is `NA`, returns "cross", else returns "circle".
#'
#' @noRd
num_shape <- function(x) {
  if (is.na(x)) {
    "cross"
  } else {
    "circle"
  }
}

#' Continuous data icons
#'
#' `num_symbols()` generates icons for continuous data.
#'
#' @param .data Dataframe
#' @param pal_range Integer list. Minimum, maximum values for continuous scale.
#'
#' @returns Icon code
#'
#' @noRd
num_symbols <- function(.data, pal_range) {
  pal <- num_pal(pal_range)

  Map(
    f = leaflegend::makeSymbol,
    shape = lapply(.data$score_num, num_shape),
    fillColor = pal(.data$score_num),
    color = "#444444",
    opacity = 1,
    width = 24,
    "stroke-width" = 1.5
  )
}

#' Categorical data icons
#'
#' `cat_pal()` generates icons for categorical data.
#'
#' @param score_str List. Variable names.
#' @param is_legend Boolean. If `TRUE`, formats symbols for a legend. If `FALSE`,
#' formats symbols for map. Default `FALSE`.
#'
#' @returns Icon code
#'
#' @noRd
cat_pal <- function(score_str, is_legend = FALSE) {
  x <- c("Excellent", "Good", "Fair", "Poor")
  y <- c("Meets Criteria", "Does Not Meet Criteria")

  icon_color <- "#f4f4f4"
  icon_shape <- "cross"
  icon_names <- "No Data Available"

  chk_x <- any(score_str %in% x)
  chk_y <- any(score_str %in% y)
  chk_legend <- chk_x & chk_y & is_legend

  if (chk_x) {
    icon_color <- c("#347bc0", "#62c2dd", "#f3d56f", "#db7363", icon_color)
    icon_shape <- c("circle", "rect", "triangle", "diamond", icon_shape)
    icon_names <- c(x, icon_names)
  }

  if (chk_y & !chk_legend) {
    icon_color <- c("#62c2dd", "#db7363", icon_color)
    icon_shape <- c("rect", "diamond", icon_shape)
    icon_names <- c(y, icon_names)
  }

  icon_symbols <- Map(
    f = leaflegend::makeSymbol,
    shape = icon_shape,
    fillColor = icon_color,
    color = "#444444",
    opacity = 1,
    width = 24,
    "stroke-width" = 1.5
  )

  if (!is_legend) {
    icon_symbols <- setNames(icon_symbols, nm = icon_names)
  }

  icon_symbols
}

#' Categorical data labels
#'
#' `cat_labels` generates legend labels to accompany `pal_cat`.
#'
#' @inheritParams cat_pal
#'
#' @returns List of labels
#'
#' @noRd
cat_labels <- function(score_str) {
  x <- c("Excellent", "Good", "Fair", "Poor")
  y <- c("Meets Criteria", "Does Not Meet Criteria")

  label_list <- "No Data Available"

  chk_x <- any(score_str %in% x)
  chk_y <- any(score_str %in% y)
  if (chk_x && chk_y) {
    label_list <- c(
      "Excellent", "Good / Meets Criteria", "Fair",
      "Poor / Does Not Meet Criteria", label_list
    )
  } else if (chk_x) {
    label_list <- c(x, label_list)
  } else if (chk_y) {
    label_list <- c(y, label_list)
  }

  label_list
}
