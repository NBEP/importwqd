#' Add javascript
#'
#' @description `add_js()` adds `js/handlers.js` to the shiny app. Must be
#' added to UI section of module for `hideCols()` to work.
#'
#' @noRd
add_js <- function() {
  includeScript(system.file("js/handlers.js", package = "importwqd"))
}

#' Hide table columns
#'
#' @description `hideCols` uses javascript to dynamically hide columns in
#' `reactable` tables.
#' Code by dleopold https://github.com/glin/reactable/issues/192
#'
#' @param id Reactable table ID.
#' @param cols List of columns to hide
#' @param session Active shiny session
#'
#' @noRd
hideCols <- function(id, cols, session = getDefaultReactiveDomain()) {
  session$sendCustomMessage("hideCols", list(id = id, cols = cols))
}
