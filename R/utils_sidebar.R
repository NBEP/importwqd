#' List choices for location toggle
#'
#' @description `loc_choices()` generates a list of inputs for radioButton
#' `loc_type`. Helper function for `mod_sidebar_location_ui()`.
#'
#' @param sites Dataframe containing site data.
#'
#' @return Named list.
#'
#' @noRd
set_loc_choices <- function(sites) {
  loc_choices <- NULL

  if (!is.null(sites$Town)) {
    loc_choices <- c("By Town" = "town")
  } else if (!is.null(sites$State)) {
    loc_choices <- c("By State" = "town")
  }

  if (!is.null(sites$Watershed)) {
    loc_choices <- c(loc_choices, "By Watershed" = "watershed")
  } else if (is.null(loc_choices)) {
    loc_choices <- "blank"
  }

  loc_choices
}

#' Set tab for `loc_tab`
#'
#' @description `loc_tab` is used to select a tab for `ns(tabset_toggle)` in
#' `mod_sidebar_location_ui()`.
#'
#' @param loc_choices Named list.
#'
#' @return Name of tab.
#'
#' @noRd
loc_tab <- function(loc_choices) {
  if (length(loc_choices) > 1) {
    return("toggle")
  } else if (loc_choices == "blank") {
    return("blank")
  }

  "notoggle"
}

#' Update town list
#'
#' @description `filter_towns` filters list of towns by state.
#'
#' @param sites Dataframe containing site data.
#' @param states List of all selected states. Must be two letter abbreviation.
#'
#' @return List of towns located in selected states.
#'
#' @noRd
filter_towns <- function(sites, states) {
  if (is.null(states)) {
    towns <- sort(unique(sites$Town))
    return(towns)
  }

  sites <- sites %>%
    dplyr::filter(.data$State %in% states)

  if (nrow(sites) == 0) {
    return(NULL)
  }

  sort(unique(sites$Town))
}

#' Generate list of sites
#'
#' @description `create_site_list()` generates a list of sites, sorted by name.
#'
#' @param sites Dataframe containing site data. Must include columns
#' Site_ID, Site_Name.
#'
#' @return Sorted site list
#'
#' @noRd
create_site_list <- function(sites) {
  if (nrow(sites) == 0) {
    return(NULL)
  }

  site_list <- sites$Site_ID
  names(site_list) <- sites$Site_Name

  site_list[order(names(site_list))]
}

#' Filter list of sites
#'
#' @description `filter_site_list()` generates a list of sites that match the
#' filter criteria.
#'
#' @param sites Dataframe containing site data.
#' @param filter_col String. Name of column used to filter site data. Should be
#' Town, State, or Watershed.
#' @param filter_list List. Sites will be filtered for matching items.
#'
#' @return Sorted site list
#'
#' @noRd
filter_site_list <- function(sites, filter_col, filter_list) {
  df_sites <- dplyr::filter(sites, .data[[filter_col]] %in% filter_list)
  create_site_list(df_sites)
}

#' Sort depth
#'
#' @description `sort_depth()` sorts depths from shallow to deep.
#'
#' @param depth_list Unsorted depth list
#'
#' @return Sorted depth list
#'
#' @noRd
sort_depth <- function(depth_list) {
  depth_list <- unique(depth_list)

  default_depths <- c("Surface", "Midwater", "Near Bottom", "Bottom")
  default_depths <- default_depths[default_depths %in% depth_list]

  extra_depths <- setdiff(depth_list, default_depths)
  extra_depths <- sort(extra_depths, na.last = TRUE)

  c(default_depths, extra_depths)
}

#' Sort months
#'
#' @description `sort_months()` sorts months chronologically and removes
#' duplicate values.
#'
#' @param month_list Unsorted list of months
#'
#' @return Chronological list of months
#'
#' @noRd
sort_months <- function(month_list) {
  month_list <- unique(month_list)

  if (length(month_list) == 1) {
    return(month_list)
  }

  month_list <- month_list[month_list %in% month.name]
  month_list <- as.integer(factor(month_list, levels = month.name))

  month_range <- seq(min(month_list), max(month_list))
  month_range <- month.name[month_range]
}
