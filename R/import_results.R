#' Check water quality data for formatting errors
#'
#' @description `qaqc_results()` checks imported water quality data for major
#' formatting errors. Specifically, it runs the following checks:
#' * Checks for missing values
#' * Checks for unknown sites
#' * Checks Results, Depth columns are numeric
#' * Checks Depth is in meters
#' * Assigns depth category
#' * Standardizes units
#'
#' @param .data Input dataframe
#' @param sites Dataframe containing site metadata
#'
#' @seealso [format_results()], [score_results()]
#'
#' @return Updated dataframe
#'
#' @export
qaqc_results <- function(.data, sites) {
  message("Checking data...")

  # Check - missing data?
  check_val_missing(.data, "Site_ID")
  check_val_missing(.data, "Date")
  check_val_missing(.data, "Parameter")

  chk <- is.na(.data$Qualifier) & is.na(.data$Result)
  if (any(chk)) {
    stop(
      "Result missing. Check rows: ",
      paste(which(chk), collapse = ", ")
    )
  }

  chk <- !is.na(.data$Result) & is.na(.data$Result_Unit)
  if (any(chk)) {
    stop(
      "Result_Unit missing. Check rows: ",
      paste(which(chk), collapse = ", ")
    )
  }

  chk <- is.na(.data$Detection_Limit_Unit) &
    (!is.na(.data$Lower_Detection_Limit) | !is.na(.data$Upper_Detection_Limit))
  if (any(chk)) {
    stop(
      "Detection_Limit_Unit missing. Check rows: ",
      paste(which(chk), collapse = ", ")
    )
  }

  # Check - all sites valid?
  site_list <- sites$Site_ID
  data_sites <- unique(.data$Site_ID)

  chk <- setdiff(data_sites, site_list)
  if (length(chk) > 0) {
    stop("Invalid Site_ID: ", paste(chk, collapse = ", "), call. = FALSE)
  }

  # Check - columns correct format? Depth in m?
  dat <- .data %>%
    wqformat::col_to_numeric("Result", silent = FALSE) %>%
    wqformat::col_to_numeric("Lower_Detection_Limit", silent = FALSE) %>%
    wqformat::col_to_numeric("Upper_Detection_Limit", silent = FALSE) %>%
    wqformat::col_to_numeric("Depth", silent = FALSE)

  depth_cat <- c("Surface", "Midwater", "Near Bottom", "Bottom")

  wqformat::warn_invalid_var(dat, "Depth_Unit", "m")
  wqformat::warn_invalid_var(dat, "Depth_Category", depth_cat)

  # Update depth category
  depth_col <- c("Max_Surface", "Max_Midwater", "Max_Depth")
  site_depth <- dplyr::select(sites, dplyr::any_of(c("Site_ID", depth_col)))

  dat <- dplyr::left_join(dat, site_depth, by = "Site_ID", keep = FALSE) %>%
    dplyr::mutate(
      "Depth_Category" = dplyr::case_when(
        !is.na(.data$Depth_Category) ~ .data$Depth_Category,
        is.na(.data$Depth) | is.na(.data$Depth_Unit) |
          .data$Depth_Unit != "m" ~ NA,
        is.na(.data$Max_Depth) & is.na(.data$Max_Midwater) &
          is.na(.data$Max_Surface) ~ NA,
        !is.na(.data$Max_Depth) & .data$Depth >= .data$Max_Depth ~ "Bottom",
        !is.na(.data$Max_Midwater) & .data$Depth >= .data$Max_Midwater ~
          "Near Bottom",
        !is.na(.data$Max_Surface) & .data$Depth >= .data$Max_Surface ~
          "Midwater",
        TRUE ~ "Surface"
      )
    ) %>%
    dplyr::select(!dplyr::any_of(depth_col))

  # Final adjustments
  dat %>%
    dplyr::mutate("Year" = as.numeric(strftime(.data$Date, "%Y"))) %>%
    dplyr::mutate(
      "Parameter" = dplyr::if_else(
        .data$Parameter == "Escherichia coli",
        "E. coli",
        .data$Parameter
      )
    ) %>%
    standardize_result_units() %>%
    standardize_detection_units()
}

#' Format water quality data for wqdasbhoard
#'
#' @description `format_results()` formats water quality data for use in
#' wqdashboard. Must run `qaqc_results()` first.
#'
#' @param thresholds Dataframe containing threshold values
#'
#' @inheritParams qaqc_results
#'
#' @return Updated dataframe
#'
#' @export
format_results <- function(.data, sites, thresholds) {
  message("Formatting data...")

  # Drop extra rows
  message("\tDropping extra rows")
  q_under <- c(
    "<2B", "2-5B", "BQL", "BRL", "D>T", "DL", "IDL", "K", "LTGTE", "U"
  )
  q_over <- c("GT", "E", "EE")
  keep_qual <- c(NA, q_under, q_over)

  dat <- .data %>%
    dplyr::filter(.data$Qualifier %in% keep_qual) %>%
    dplyr::filter(
      !grepl("quality control", .data$Activity_Type, ignore.case = TRUE)
    )

  # Update nondetect, overdetect values
  chk <- is.na(dat$Qualifier)
  if (any(!chk)) {
    message("\tSetting nondetect, overdetect values")

    dat <- dat %>%
      dplyr::mutate(
        "Result" = dplyr::case_when(
          !is.na(.data$Result) | is.na(.data$Qualifier) ~ .data$Result,
          .data$Qualifier %in% q_over ~ .data$Upper_Detection_Limit,
          .data$Qualifier %in% q_under & .data$Parameter == "pH" ~
            .data$Lower_Detection_Limit,
          .data$Qualifier %in% q_under & is.na(.data$Lower_Detection_Limit) ~ 0,
          .data$Qualifier %in% q_under ~ .data$Lower_Detection_Limit / 2,
          TRUE ~ .data$Result
        )
      )
  }

  check_val_missing(dat, "Result")

  # Adding threshold data
  message("\tAdding threshold values")
  df_temp <- dat %>%
    dplyr::select("Site_ID", "Depth_Category", "Parameter") %>%
    unique()

  df_sites <- sites %>%
    dplyr::select("Site_ID", "State", "Group")

  df_temp <- dplyr::left_join(df_temp, df_sites) %>%
    dplyr::mutate(
      "thresh_temp" = mapply(
        function(id, group, state, depth, par) {
          add_thresholds(thresholds, id, group, state, depth, par)
        },
        .data$Site_ID, .data$Group, .data$State,
        .data$Depth_Category, .data$Parameter,
        SIMPLIFY = FALSE
      )
    ) %>%
    tidyr::unnest_wider("thresh_temp") %>%
    dplyr::select(!c("State", "Group"))

  dat <- dplyr::left_join(
    dat, df_temp,
    by = c("Site_ID", "Depth_Category", "Parameter")
  )

  # Adjust columns
  message("\tDropping extra columns")

  field_keep <- c(
    "Site_ID", "Date", "Year", "Parameter", "Result", "Result_Unit",
    "Depth_Category", "Calculation", "Min", "Max", "Excellent", "Good", "Fair",
    "Best"
  )

  dat %>%
    dplyr::select(dplyr::any_of(field_keep)) %>%
    dplyr::rename("Depth" = "Depth_Category", "Unit" = "Result_Unit") %>%
    dplyr::mutate("Month" = strftime(.data$Date, "%B"))
}

#' Calculate annual scores and format results for wqdashboard
#'
#' @description `score_results()` calculates annual scores and formats data
#' for use in [wqdashboard]. Must run `format_results()` first.
#'
#' @inheritParams qaqc_results
#'
#' @return Updated dataframe
#'
#' @export
score_results <- function(.data, sites) {
  # Calculate scores, etc
  message("Formatting data scores...")

  message("\tGrouping data")
  dat <- .data %>%
    dplyr::group_by_at(c("Site_ID", "Parameter", "Depth", "Year")) %>%
    dplyr::summarise(
      "score_max" = max(.data$Result),
      "score_min" = min(.data$Result),
      "score_mean" = mean(.data$Result),
      "score_median" = median(.data$Result),
      "Unit" = dplyr::last(.data$Unit),
      "score_typ" = dplyr::last(.data$Calculation),
      "Min" = dplyr::last(.data$Min),
      "Max" = dplyr::last(.data$Max),
      "Excellent" = dplyr::last(.data$Excellent),
      "Good" = dplyr::last(.data$Good),
      "Fair" = dplyr::last(.data$Fair),
      "Best" = dplyr::last(.data$Best),
      .groups = "drop"
    ) %>%
    data.frame() # fix test error

  message("\tCalculating score")
  dat <- dat %>%
    dplyr::mutate(
      "score_num" = dplyr::case_when(
        is.na(.data$score_typ) | .data$score_typ == "mean" ~
          .data$score_mean,
        .data$score_typ == "max" ~ .data$score_max,
        .data$score_typ == "min" ~ .data$score_min,
        .data$score_typ == "median" ~ .data$score_median,
        TRUE ~ .data$score_mean
      )
    ) %>%
    dplyr::mutate(
      "score_str" = dplyr::case_when(
        is.na(.data$Best) ~ NA,
        .data$Best == "high" & .data$score_num >= .data$Excellent ~ "Excellent",
        .data$Best == "high" & .data$score_num >= .data$Good ~ "Good",
        .data$Best == "high" & .data$score_num >= .data$Fair ~ "Fair",
        .data$Best == "high" ~ "Poor",
        .data$Best == "low" & .data$score_num <= .data$Excellent ~ "Excellent",
        .data$Best == "low" & .data$score_num <= .data$Good ~ "Good",
        .data$Best == "low" & .data$score_num <= .data$Fair ~ "Fair",
        .data$Best == "low" ~ "Poor",
        TRUE ~ NA
      )
    ) %>%
    dplyr::mutate(
      "score_str" = dplyr::case_when(
        !is.na(.data$score_str) ~ .data$score_str,
        is.na(.data$Min) & is.na(.data$Max) ~ NA,
        !is.na(.data$Min) & .data$score_num < .data$Min ~
          "Does Not Meet Criteria",
        !is.na(.data$Max) & .data$score_num > .data$Max ~
          "Does Not Meet Criteria",
        TRUE ~ "Meets Criteria"
      )
    ) %>%
    dplyr::select(
      c(
        "Site_ID", "Parameter", "Unit", "Depth", "Year", "score_typ",
        "score_num", "score_str"
      )
    )

  message("\tFormatting data")

  # Generate dataframe of site/year/parameter/depth combinations
  df_temp <- dat %>%
    dplyr::select("Site_ID", "Year", "Parameter", "Depth") %>%
    unique()

  df_all <- expand.grid(
    Site_ID = unique(dat$Site_ID),
    Year = unique(dat$Year),
    Parameter = unique(dat$Parameter),
    Depth = unique(dat$Depth)
  )

  df_missing <- dplyr::setdiff(df_all, df_temp)

  if (nrow(df_missing) > 0) {
    dat <- dplyr::bind_rows(dat, df_missing)
  }

  # Add site data
  site_col <- c(
    "Site_ID", "Site_Name", "Latitude", "Longitude", "Town_Code",
    "County_Code", "State", "Watershed", "Group"
  )

  df_sites <- dplyr::select(sites, dplyr::any_of(site_col))

  if ("Town_Code" %in% colnames(df_sites)) {
    df_sites <- df_sites %>%
      dplyr::select(!dplyr::any_of("State")) %>%
      dplyr::rename("Town" = "Town_Code")
  } else if ("County_Code" %in% colnames(df_sites)) {
    df_sites <- df_sites %>%
      dplyr::select(!dplyr::any_of("State")) %>%
      dplyr::rename("County" = "County_Code")
  }

  dat <- dplyr::left_join(dat, df_sites, by = "Site_ID")

  # Final tweaks
  col_order <- c(
    "Year", "Site_Name", "Site_ID", "Town", "County", "State",
    "Watershed", "Group", "Depth", "Parameter", "Unit",
    "score_typ", "score_num", "score_str", "Latitude", "Longitude"
  )

  dat %>%
    dplyr::select(dplyr::any_of(col_order)) %>%
    dplyr::mutate(
      "score_str" = dplyr::case_when(
        !is.na(.data$score_str) ~ .data$score_str,
        !is.na(.data$score_num) ~ "No Threshold Established",
        TRUE ~ "No Data Available"
      )
    ) %>%
    dplyr::mutate(
      "Parameter" = dplyr::if_else(
        is.na(.data$Parameter), "-", .data$Parameter
      )
    ) %>%
    dplyr::arrange(.data$Site_Name, .data$Parameter)
}
