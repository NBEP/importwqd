#' Prepare site metadata
#'
#' @description `prep_sites()` prepares site data for use in `wqdashboard` by
#' updating colum names.
#'
#' @inheritParams prep_results
#'
#' @return Updated dataframe
#'
#' @export
prep_sites <- function(.data, df_colnames) {
  message("Preparing site metadata...")

  df_colnames <- df_colnames |>
    dplyr::filter(!is.na(.data$wqdashboard) & !is.na(.data$Custom))

  if (nrow(df_colnames) == 0) {
    message("No changes made")
    return(.data)
  }

  .data |>
    wqformat::rename_col(df_colnames$Custom, df_colnames$wqdashboard)
}

#' Check site metadata for formatting errors
#'
#' @description `qaqc_sites()` checks imported site metadata for major
#' formatting errors. Specifically, it runs the following checks:
#' * Checks for missing columns
#' * Checks for missing values
#' * Checks for duplicate site ID, site name, coordinates
#' * Checks if coordinate, depth columns contain non-numeric values
#' * Checks if depth values are illogical (eg surface depth is greater than
#' midwater depth)
#'
#' @param .data Input dataframe
#' @param state String. State name or abbreviation. Will replace `NA` rows in
#' "State" column. Default `NA`.
#'
#' @seealso [format_sites()]
#'
#' @return Updated dataframe
#'
#' @export
qaqc_sites <- function(.data, state = NA) {
  message("Checking site metadata...")

  # Define vars
  field_need <- c("Site_ID", "Site_Name", "Latitude", "Longitude")
  field_optional <- c(
    "Town", "State", "Watershed", "Group", "Max_Surface_Depth_m",
    "Max_Midwater_Depth_m", "Max_Depth_m"
  )

  # Check - missing columns?
  check_col_missing(.data, field_need)

  # Check - missing  or duplicate values?
  for (field in field_need) {
    check_val_missing(.data, field)
  }

  check_val_duplicate(.data, "Site_ID")
  check_val_duplicate(.data, "Site_Name", is_stop = FALSE)
  check_val_duplicate(.data, c("Latitude", "Longitude"), is_stop = FALSE)

  # Drop empty columns
  chk <- apply(.data, 2, function(x) all(is.na(x)))
  empty_col <- colnames(.data)[which(chk)]
  drop_col <- setdiff(empty_col, field_optional)
  .data[drop_col] <- NULL

  # Check - missing values in optional columns?
  field_check <- intersect(field_optional, colnames(.data))
  for (field in field_check) {
    check_val_missing(.data, field, is_stop = FALSE)
  }

  # Update columns
  missing_col <- setdiff(field_optional, colnames(.data))
  .data[missing_col] <- NA

  dat <- .data |>
    wqformat::col_to_numeric("Latitude", silent = FALSE) |>
    wqformat::col_to_numeric("Longitude", silent = FALSE) |>
    wqformat::col_to_numeric("Max_Surface_Depth_m", silent = FALSE) |>
    wqformat::col_to_numeric("Max_Midwater_Depth_m", silent = FALSE) |>
    wqformat::col_to_numeric("Max_Depth_m", silent = FALSE) |>
    dplyr::mutate(
      "State" = dplyr::if_else(
        is.na(.data$State),
        state,
        .data$State
      )
    ) |>
    wqformat::state_to_abb("State") |>
    dplyr::mutate("Site_Name" = make.unique(.data$Site_Name, sep = " "))

  # Check - illogical values
  chk <- is.na(dat$Max_Surface_Depth_m) | is.na(dat$Max_Midwater_Depth_m) |
    dat$Max_Surface_Depth_m < dat$Max_Midwater_Depth_m
  chk2 <- is.na(dat$Max_Midwater_Depth_m) | is.na(dat$Max_Depth_m) |
    dat$Max_Midwater_Depth_m < dat$Max_Depth_m
  chk3 <- is.na(dat$Max_Surface_Depth_m) | is.na(dat$Max_Depth_m) |
    dat$Max_Surface_Depth_m < dat$Max_Depth_m
  chk <- chk & chk2 & chk3
  if (any(!chk)) {
    stop(
      "Illogical depth values. Check rows: ",
      paste(which(!chk), collapse = ", ")
    )
  }

  message("\tAdding depth thresholds")
  dat |>
    dplyr::mutate(
      "Max_Surface_Depth_m" = dplyr::case_when(
        !is.na(.data$Max_Surface_Depth_m) ~ .data$Max_Surface_Depth_m,
        !is.na(.data$Max_Midwater_Depth_m) & .data$Max_Midwater_Depth_m <= 1 ~
          NA,
        !is.na(.data$Max_Depth_m) & .data$Max_Depth_m <= 2 ~ NA,
        TRUE ~ 1
      )
    ) |>
    dplyr::mutate(
      "Max_Midwater_Depth_m" = dplyr::case_when(
        !is.na(.data$Max_Midwater_Depth_m) ~ .data$Max_Midwater_Depth_m,
        is.na(.data$Max_Depth_m) ~ NA,
        .data$Max_Depth_m <= 2 ~ NA,
        TRUE ~ .data$Max_Depth_m - 1
      )
    )
}

#' Format site metadata for use in wqdashboard
#'
#' @description `format_sites()` formats site metadata for use in [wqdashboard].
#'
#' @param .data Input dataframe
#' @param site_list List of site ID values.  Dataframe will be filtered for
#' matching values in column "Site_ID". If `NULL`, dataframe will not be
#' filtered. Default `NULL`.
#'
#' @seealso [qaqc_sites()]
#'
#' @return Updated dataframe.
#'
#' @export
format_sites <- function(.data, site_list = NULL) {
  message("Formatting site data...")
  dat <- .data

  # Drop surplus sites
  if (!is.null(site_list)) {
    dat <- dat |>
      dplyr::filter(.data$Site_ID %in% site_list)

    chk <- setdiff(.data$Site_ID, dat$Site_ID)
    if (length(chk) > 0) {
      message("Dropped ", length(chk), " sites: ", paste(chk, collapse = ", "))
    }
  }

  # Drop extra columns
  field_all <- c(
    "Site_ID", "Site_Name", "Latitude", "Longitude", "Town", "State",
    "Watershed", "Group"
  )

  message("\tDropping extra columns")
  dat <- dat |>
    dplyr::select(dplyr::all_of(field_all)) |>
    drop_uniform_col("Watershed") |>
    drop_uniform_col("State") |>
    drop_empty_col("Town")

  if (!"State" %in% colnames(dat)) {
    dat$State <- NA
    dat <- dat |>
      drop_uniform_col("Town")
  }

  if ("Town" %in% colnames(dat)) {
    message("\tUpdating town names")
    dat <- dat |>
      dplyr::mutate(
        "Town" = dplyr::if_else(
          is.na(.data$State),
          .data$Town,
          paste0(.data$Town, ", ", .data$State)
        )
      )
  }

  dat <- wqformat::abb_to_state(dat, "State")

  suppressMessages(drop_empty_col(dat, "State"))
}
