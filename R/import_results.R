#' Prepare water quality data
#'
#' @description `prep_results()` prepares water quality data for use in
#' `wqdashboard` by updating column and variable names.
#'
#' @param .data Input dataframe
#' @param df_colnames Dataframe. Must include paired list of column names in
#' columns "wqdashboard" and "Custom".
#' @param df_param Dataframe. Must include paired list of parameter names in
#' columns "wqdashboard" and "Custom".
#' @param df_unit Dataframe. Must include paired list of unit names in columns
#' "wqdashboard" and "Custom".
#' @param df_qual Dataframe. Must include paired list of qualifier names in
#' columns "wqdashboard" and "Custom".
#' @param df_activity Dataframe. Must include paired list of activity type names
#' in columns "wqdashboard" and "Custom".
#'
#' @return Updated dataframe
#'
#' @export
prep_results <- function(
  .data, df_colnames, df_param, df_unit, df_qual, df_activity
) {
  message("Preparing data...")

  df_colnames <- df_colnames |>
    dplyr::filter(!is.na(.data$wqdashboard) & !is.na(.data$Custom))

  if (nrow(df_colnames) == 0) {
    message("Did not update column names")
    dat <- .data
  } else {
    dat <- .data |>
      wqformat::rename_col(df_colnames$Custom, df_colnames$wqdashboard)
  }

  dat |>
    try_rename("Parameter", df_param) |>
    try_rename("Result_Unit", df_unit) |>
    try_rename("Detection_Limit_Unit", df_unit) |>
    try_rename("Qualifier", df_qual) |>
    try_rename("Activity_Type", df_activity)
}

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

  # Format data
  .data |>
    wqformat::col_to_numeric("Result", silent = FALSE) |>
    wqformat::col_to_numeric("Lower_Detection_Limit", silent = FALSE) |>
    wqformat::col_to_numeric("Upper_Detection_Limit", silent = FALSE) |>
    add_depth_category(sites) |>
    dplyr::mutate("Year" = as.numeric(strftime(.data$Date, "%Y"))) |>
    wqformat::standardize_units(
      "Parameter",
      "Result",
      "Result_Unit",
      warn_only = FALSE
    ) |>
    wqformat::standardize_units_across(
      "Result_Unit",
      "Detection_Limit_Unit",
      c("Lower_Detection_Limit", "Upper_Detection_Limit"),
      warn_only = FALSE
    )
}

#' Check categorical water quality data for formatting errors
#'
#' @description `qaqc_cat_results()` checks categorical water quality data for
#' major formatting errors. Specifically, it runs the following checks:
#' * Checks for missing values
#' * Checks for unknown sites
#' * Checks Depth column is numeric
#' * Checks Depth is in meters
#' * Assigns depth category
#'
#' @param .data Input dataframe
#' @param sites Dataframe containing site metadata
#'
#' @seealso [qaqc_results()]
#'
#' @return Updated dataframe
#'
#' @export
qaqc_cat_results <- function(.data, sites) {
  message("Checking categorical data...")

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

  # Check - all sites valid?
  site_list <- sites$Site_ID
  data_sites <- unique(.data$Site_ID)

  chk <- setdiff(data_sites, site_list)
  if (length(chk) > 0) {
    stop("Invalid Site_ID: ", paste(chk, collapse = ", "), call. = FALSE)
  }

  # Final adjustments
  .data |>
    add_depth_category(sites) |>
    dplyr::mutate("Year" = as.numeric(strftime(.data$Date, "%Y")))
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

  dat <- .data |>
    dplyr::filter(.data$Qualifier %in% keep_qual) |>
    dplyr::filter(
      !grepl("quality control", .data$Activity_Type, ignore.case = TRUE)
    )

  # Update nondetect, overdetect values
  chk <- is.na(dat$Qualifier)
  if (any(!chk)) {
    message("\tSetting nondetect, overdetect values")

    dat <- dat |>
      dplyr::mutate(
        "Result" = dplyr::case_when(
          !is.na(.data$Result) | is.na(.data$Qualifier) ~ .data$Result,
          .data$Qualifier %in% q_over ~ .data$Upper_Detection_Limit,
          !.data$Qualifier %in% q_under ~ .data$Result,
          .data$Parameter == "pH" ~ .data$Lower_Detection_Limit,
          is.na(.data$Lower_Detection_Limit) ~ 0,
          .data$Lower_Detection_Limit < 0 ~ .data$Lower_Detection_Limit,
          TRUE ~ .data$Lower_Detection_Limit / 2
        )
      )
  }

  check_val_missing(dat, "Result")

  # Adding threshold data
  message("\tAdding threshold values")
  df_sites <- sites |>
    dplyr::select("Site_ID", "Site_Name", "State", "Group")

  df_temp <- dat |>
    dplyr::select("Site_ID", "Depth_Category", "Parameter") |>
    unique()
  df_temp <- dplyr::left_join(df_temp, df_sites)

  df_thresh <- update_threshold_units(thresholds, dat)

  if (nrow(df_thresh) > 0) {
    df_temp <- df_temp |>
      dplyr::mutate(
        "thresh_temp" = mapply(
          function(id, group, state, depth, par) {
            add_thresholds(df_thresh, id, group, state, depth, par)
          },
          .data$Site_ID, .data$Group, .data$State, .data$Depth_Category,
          .data$Parameter,
          SIMPLIFY = FALSE
        )
      ) |>
      tidyr::unnest_wider("thresh_temp") |>
      dplyr::select(!c("State", "Group"))
  } else {
    df_temp$Calculation <- "mean"
    df_temp[c("Min", "Max", "Excellent", "Good", "Fair", "Best")] <- NA
  }

  dat <- dplyr::left_join(
    dat, df_temp,
    by = c("Site_ID", "Depth_Category", "Parameter")
  )

  # Adjust columns, add "Description" column
  message("\tDropping extra columns")

  field_keep <- c(
    "Site_ID", "Site_Name", "Date", "Year", "Parameter", "Result",
    "Result_Unit", "Depth_Category", "Calculation", "Min", "Max", "Excellent",
    "Good", "Fair", "Best"
  )

  dat <- dat |>
    dplyr::select(dplyr::any_of(field_keep)) |>
    drop_uniform_col("Depth_Category", include_na = FALSE) |>
    drop_uniform_col("Group") |>
    dplyr::rename("Unit" = "Result_Unit") |>
    dplyr::mutate("Month" = strftime(.data$Date, "%B")) |>
    dplyr::mutate(
      "Description" = paste0(
        "<b>", .data$Site_Name, "</b><br>Date: ",
        format(.data$Date, format = "%B %d, %Y")
      )
    )

  if ("Depth_Category" %in% colnames(dat)) {
    dat <- dat |>
      dplyr::rename("Depth" = "Depth_Category") |>
      dplyr::mutate(
        "Description" = dplyr::case_when(
          grepl("depth|height", .data$Parameter) ~ .data$Description,
          is.na(.data$Depth) ~ paste0(.data$Description, "<br>Depth: -"),
          TRUE ~ paste0(.data$Description, "<br>Depth: ", .data$Depth)
        )
      )
  }

  dat |>
    dplyr::mutate(
      "Description" = paste0(
        .data$Description, "<br>", .data$Parameter, ": ",
        pretty_number(.data$Result)
      )
    ) |>
    dplyr::mutate(
      "Description" = dplyr::if_else(
        .data$Unit %in% c(NA, "None"),
        .data$Description,
        paste(.data$Description, .data$Unit)
      )
    )
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
  group_col <- c("Site_ID", "Parameter", "Depth", "Year")
  group_col <- intersect(colnames(.data), group_col)

  dat <- .data |>
    dplyr::group_by_at(group_col) |>
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
    ) |>
    data.frame() # fix test error

  message("\tCalculating score")
  dat <- dat |>
    dplyr::mutate(
      "score_num" = dplyr::case_when(
        is.na(.data$score_typ) | .data$score_typ == "mean" ~
          .data$score_mean,
        .data$score_typ == "max" ~ .data$score_max,
        .data$score_typ == "min" ~ .data$score_min,
        .data$score_typ == "median" ~ .data$score_median,
        TRUE ~ .data$score_mean
      )
    ) |>
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
    ) |>
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
    ) |>
    dplyr::select(
      dplyr::any_of(
        c(
          "Site_ID", "Parameter", "Unit", "Depth", "Year", "score_typ",
          "score_num", "score_str"
        )
      )
    )

  message("\tFormatting data")

  # Generate dataframe of site/year/parameter/depth combinations
  if ("Depth" %in% colnames(dat)) {
    df_temp <- dat |>
      dplyr::select("Site_ID", "Year", "Parameter", "Depth") |>
      unique()

    depth <- unique(dat$Depth)
    depth <- depth[!is.na(depth)]

    df_all <- expand.grid(
      Site_ID = unique(dat$Site_ID),
      Year = unique(dat$Year),
      Parameter = unique(dat$Parameter),
      Depth = depth
    ) |>
      dplyr::mutate(
        "Depth" = dplyr::if_else(
          grepl("depth|height", tolower(.data$Parameter)),
          NA,
          .data$Depth
        )
      ) |>
      unique()
  } else {
    df_temp <- dat |>
      dplyr::select("Site_ID", "Year", "Parameter") |>
      unique()

    df_all <- expand.grid(
      Site_ID = unique(dat$Site_ID),
      Year = unique(dat$Year),
      Parameter = unique(dat$Parameter)
    )
  }

  df_missing <- dplyr::setdiff(df_all, df_temp)

  if (nrow(df_missing) > 0) {
    dat <- dplyr::bind_rows(dat, df_missing)
  }

  # Add site data
  site_col <- c(
    "Site_ID", "Site_Name", "Latitude", "Longitude", "Town", "State",
    "Watershed", "Group"
  )

  df_sites <- dplyr::select(sites, dplyr::any_of(site_col))

  if ("Town" %in% colnames(df_sites)) {
    df_sites <- df_sites |>
      dplyr::select(!dplyr::any_of("State"))
  }

  dat <- dplyr::left_join(dat, df_sites, by = "Site_ID")

  # Adjust data
  col_order <- c(
    "Year", "Site_Name", "Site_ID", "Town", "State", "Watershed", "Group",
    "Depth", "Parameter", "Unit", "score_typ", "score_num", "score_str",
    "Latitude", "Longitude"
  )

  dat <- dat |>
    dplyr::select(dplyr::any_of(col_order)) |>
    dplyr::mutate("score_num" = pretty_number(.data$score_num)) |>
    dplyr::mutate(
      "score_typ" = dplyr::case_when(
        .data$score_typ == "min" ~ "Minimum",
        .data$score_typ == "max" ~ "Maximum",
        .data$score_typ == "median" ~ "Median",
        .data$score_typ == "mean" ~ "Average",
        TRUE ~ .data$score_typ
      )
    ) |>
    dplyr::mutate(
      "score_str" = dplyr::case_when(
        !is.na(.data$score_str) ~ .data$score_str,
        !is.na(.data$score_num) ~ "No Threshold Established",
        TRUE ~ "No Data Available"
      )
    ) |>
    # dplyr::mutate(
    #   "Parameter" = dplyr::if_else(
    #     is.na(.data$Parameter), "-", .data$Parameter
    #   )
    # ) |>
    dplyr::arrange(.data$Site_Name, .data$Parameter) |>
    dplyr::mutate("popup_loc" = paste0("<b>", .data$Site_Name, "</b>"))

  if ("Town" %in% colnames(dat)) {
    dat <- dat |>
      dplyr::mutate(
        "popup_loc" = dplyr::if_else(
          is.na(.data$Town),
          paste(.data$popup_loc, "<br>Town: -"),
          paste(.data$popup_loc, "<br>Town:", .data$Town)
        )
      )
  } else if ("State" %in% colnames(dat)) {
    dat <- dat |>
      wqformat::abb_to_state("State") |>
      dplyr::mutate(
        "popup_loc" = dplyr::if_else(
          is.na(.data$State),
          paste(.data$popup_loc, "<br>State: -"),
          paste(.data$popup_loc, "<br>State:", .data$State)
        )
      )
  }

  if ("Watershed" %in% colnames(dat)) {
    dat <- dat |>
      dplyr::mutate(
        "popup_loc" = dplyr::if_else(
          is.na(.data$Watershed),
          paste(.data$popup_loc, "<br>Watershed: -"),
          paste(.data$popup_loc, "<br>Watershed:", .data$Watershed)
        )
      )
  }

  if ("Group" %in% colnames(dat)) {
    dat <- dat |>
      dplyr::mutate(
        "popup_loc" = dplyr::if_else(
          is.na(.data$Group),
          paste(.data$popup_loc, "<br>Group: -"),
          paste(.data$popup_loc, "<br>Group:", .data$Group)
        )
      )
  }

  if ("Depth" %in% colnames(dat)) {
    dat <- dat |>
      dplyr::mutate(
        "popup_loc" = dplyr::case_when(
          grepl("depth|height", tolower(.data$Parameter)) ~ .data$popup_loc,
          is.na(.data$Depth) ~ paste(.data$popup_loc, "<br>Depth: -"),
          TRUE ~ paste(.data$popup_loc, "<br>Depth:", .data$Depth)
        )
      )
  }

  dat |>
    dplyr::mutate(
      "popup_score" = dplyr::case_when(
        is.na(.data$score_num) ~ "<i>No data</i>",
        .data$Unit %in% c(NA, "None") ~
          paste0(.data$score_typ, ": ", .data$score_num),
        TRUE ~ paste0(.data$score_typ, ": ", .data$score_num, " ", .data$Unit)
      )
    ) |>
    dplyr::mutate(
      "popup_score" = dplyr::if_else(
        is.na(.data$score_num) | .data$score_str == "No Threshold Established",
        paste0("<br>", .data$popup_score),
        paste0("<br>", .data$popup_score, "<br>Score: ", .data$score_str)
      )
    ) |>
    dplyr::mutate(
      "alt" = dplyr::case_when(
        is.na(.data$score_num) ~ paste0(.data$Site_Name, ", No data"),
        .data$score_str != "No Threshold Established" ~
          paste0(.data$Site_Name, ", ", .data$score_str),
        .data$Unit %in% c(NA, "None") ~
          paste0(.data$Site_Name, ", ", .data$score_num),
        TRUE ~ paste0(.data$Site_Name, ", ", .data$score_num, " ", .data$Unit)
      )
    )
}

#' Generate dropdown lists for sidebar
#'
#' @description `sidebar_var()` processes site and result data to generate
#' a list of dropdown options for `mod_sidebar_ui()`.
#'
#' @param df_data Dataframe with result data. Must have been processed
#' by `format_results()` and include columns Parameter, Month, Year.
#' @param df_score Dataframe with data scores. Must have been processed
#' by `score_results()` and include column Parameter.
#' @param df_sites Dataframe with site data. Must have been processed
#' by `format_sites()` and  include columns Site_ID, Site_Name.
#' @param df_cat Dataframe with categorical data. Default `NULL`.
#'
#' @return Named list containing the following:
#' * state - List of state names.
#' * town - List of town names.
#' * watershed - List of watershed names.
#' * param - List of parameters
#' * param_short - List of parameters that include at least one categorical
#' score (eg excellent, good, fair)
#' * depth - List of depths
#' * year - Sorted list of years
#' * month - Continuous, sorted list of months
#'
#' @export
sidebar_var <- function(df_sites, df_data, df_score, df_cat = NULL) {
  # Filter df_sites by sites listed in df_data, df_cat
  data_sites <- unique(df_data$Site_ID)
  if (!is.null(df_cat)) {
    data_sites <- c(data_sites, df_cat$Site_ID) |>
      unique()
  }

  # Define location variables
  state <- NULL
  town <- NULL
  watershed <- NULL

  if ("State" %in% colnames(df_sites)) {
    state <- unique(df_sites$State)
  }
  if ("Town" %in% colnames(df_sites)) {
    town <- unique(df_sites$Town)
  }
  if ("Watershed" %in% colnames(df_sites)) {
    watershed <- unique(df_sites$Watershed)
  }

  loc_choices <- NULL
  loc_tab <- "notoggle"

  if (!is.null(town)) {
    loc_choices <- c("By Town" = "town")
  } else if (!is.null(state)) {
    loc_choices <- c("By State" = "town")
  }

  if (!is.null(watershed)) {
    loc_choices <- c(loc_choices, "By Watershed" = "watershed")
  } else if (is.null(loc_choices)) {
    loc_choices <- "blank"
    loc_tab <- "blank"
  }

  if (length(loc_choices) > 1) {
    loc_tab <- "toggle"
  }

  # Define paramater variables
  param_short <- df_score |>
    dplyr::filter(
      !.data$score_str %in% c("No Data Available", "No Threshold Established")
    )
  param_short <- sort(unique(param_short$Parameter))

  param_cat <- NULL
  if (!is.null(df_cat)) {
    param_cat <- unique(df_cat$Parameter)
  }

  depth <- NULL
  if ("Depth" %in% colnames(df_data)) {
    depth <- sort_depth(df_data$Depth)
    depth <- depth[!is.na(depth)]
  }

  list(
    state = state,
    town = town,
    watershed = watershed,
    site_id = unique(df_sites$Site_ID),
    site_name = unique(df_sites$Site_Name),
    loc_choices = loc_choices,
    loc_tab = loc_tab,
    param = unique(df_data$Parameter),
    param_score = param_short,
    param_cat = param_cat,
    depth = depth,
    year = sort(unique(df_data$Year)),
    month = sort_months(df_data$Month)
  )
}
