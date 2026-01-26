#' Create dropdown menu
#'
#' @description `dropdown()` creates a dropdown widget.
#'
#' @param id String. Widget id.
#' @param label String. Widget heading/label.
#' @param choices List. Dropdown choices.
#' @param choice_names List. Display names for choices. Default `NULL`.
#' @param sorted Boolean. Whether to sort the choices. Default `TRUE`.
#' @param decreasing Boolean. Whether to sort choices in descending order.
#' Default `FALSE`.
#' @param multiple Boolean. Whether to allow multiple selections. Default
#' `TRUE`.
#' @param max_options Integer. Maximum number of selections. Default `NULL`.
#'
#' @return A dropdown widget.
#'
#' @export
dropdown <- function(
  id, label, choices, choice_names = NULL, sorted = TRUE,
  decreasing = FALSE, multiple = TRUE, max_options = NULL
) {
  if (!is.null(choice_names)) {
    names(choices) <- choice_names
  }

  choices <- choices[!duplicated(choices)]

  if (sorted && is.null(choice_names)) {
    choices <- sort(choices, decreasing = decreasing)
  } else if (sorted) {
    choices <- choices[order(names(choices), decreasing = decreasing)]
  }

  selected <- choices[1]
  allow_actions <- FALSE
  if (multiple && is.null(max_options)) {
    selected <- choices
    allow_actions <- TRUE
  }

  shinyWidgets::pickerInput(
    id,
    label = label,
    choices = choices,
    selected = selected,
    options = list(
      `actions-box` = allow_actions,
      `live-search` = TRUE,
      `selected-text-format` = "count > 1",
      `max-options` = max_options,
      container = "body"
    ),
    multiple = multiple
  )
}
