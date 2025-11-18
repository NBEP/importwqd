#' Find Threshold
#'
#' @description
#' `find_threshold()` checks stored threshold values, and locates the relevant
#' threshod values for a given site, parameter, and depth. If no thresholds
#' found, returns NULL.
#'
#' @param site_id String. Site ID.
#' @param parameter String. Parameter.
#' @param depth String. Depth category. Acceptable values: Surface, Midwater,
#' Near Bottom, Bottom. Default NA.
#'
#' @return One row dataframe with list of thresholds for provided site,
#' parameter, and depth. If no thresholds found, returns NULL.
#'
#' @noRd
find_threshold <- function(site_id, parameter, depth = NA) {
  # Define vars
  var_group <- NA
  var_state <- NA

  if ("Group" %in% colnames(df_sites)) {
    df <- df_sites %>%
      dplyr::filter(.data$Site_ID == site_id)

    var_group <- df$Group[1]
  }

  if ("State" %in% colnames(df_sites_all)) {
    # use df_sites_all in case state col dropped in df_sites
    df <- df_sites_all %>%
      dplyr::filter(.data$Site_ID == site_id)

    var_state <- df$State[1]
  }

  if (exists("custom_thresholds")) {
    df_thresh <- dplyr::bind_rows(custom_thresholds, official_thresholds)
  } else {
    df_thresh <- official_thresholds
  }

  df_thresh <- df_thresh %>%
    dplyr::filter(
      .data$Parameter == parameter,
      is.na(.data$State) | .data$State == var_state,
      is.na(.data$Group) | .data$Group == var_group
    )

  if ("Site_ID" %in% colnames(df_thresh)) {
    df_thresh <- df_thresh %>%
      dplyr::filter(
        is.na(.data$Site_ID) | .data$Site_ID == site_id
      )
  }

  if ("Depth_Category" %in% colnames(df_thresh)) {
    df_thresh <- df_thresh %>%
      dplyr::filter(
        is.na(.data$Depth_Category) | .data$Depth_Category == depth
      )
  }

  if (nrow(df_thresh) == 0) {
    return(NULL)
  } else {
    return(df_thresh[1, ])
  }
}

#' Convert Threshold Unit
#'
#' @description Converts threshold value to new unit. `NA` values are returned
#' as -999999
#'
#' @param x Numeric. Value to convert.
#' @param old_unit String. Current unit.
#' @param new_unit String. New unit.
#'
#' @return Updated value
#'
#' @noRd
convert_threshold_unit <- function(x, old_unit, new_unit) {
  if (is.na(x)) {
    return(-999999)
  }

  convert_unit(x, old_unit, new_unit)
}
