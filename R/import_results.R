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
#' @param site_data Dataframe containing site metadata.
#'
#' @seealso [format_results()], [score_results()]
#'
#' @return Updated dataframe
qaqc_results <- function(.data, site_data) {
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
  site_list <- site_data$Site_ID
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
  site_depth <- dplyr::select(site_data, dplyr::any_of(c("Site_ID", depth_col)))

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
#' @inheritParams qaqc_results
#'
#' @return Updated dataframe.
format_results <- function(.data) {
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

  # Adjust columns
  message("\tDropping extra columns")

  field_keep <- c(
    "Site_ID", "Date", "Year", "Parameter", "Result", "Result_Unit",
    "Depth_Category"
  )

  dat %>%
    dplyr::select(dplyr::any_of(field_keep)) %>%
    dplyr::rename("Depth" = "Depth_Category") %>%
    dplyr::mutate("Month" = strftime(.data$Date, "%B"))
}

#' Format df_score
#'
#' @description Formats water quality data for use in app. Must run
#'   `format_results` first.
#'
#' @param df Input dataframe.
#'
#' @return Updated dataframe.
score_results <- function(df) {
  # Calculate scores, etc
  message("\nFormatting df_score...\n")
  field_group <- c("Site_ID", "Depth", "Parameter", "Unit", "Year")

  df <- df %>%
    dplyr::group_by_at(field_group) %>%
    dplyr::summarise(
      score_max = max(Result),
      score_min = min(Result),
      score_mean = mean(Result),
      score_median = median(Result),
      .groups = "drop"
    )
  message("\tGrouped data by year\n\tCalculating scores...")

  df <- df %>%
    dplyr::mutate(
      score_temp = mapply(
        function(id, par, unit, depth, a, b, c, d) {
          calculate_score(id, par, unit, depth, a, b, c, d)
        },
        Site_ID, Parameter, Unit, Depth,
        score_max, score_min, score_mean, score_median,
        SIMPLIFY = FALSE
      )
    ) %>%
    tidyr::unnest_wider(score_temp) %>%
    dplyr::select(!score_max:score_median) %>%
    dplyr::mutate(score_num = pretty_number(score_num))
  df <- suppressMessages(check_val_count(df, "Depth"))
  message("\t... ok")

  # Generate dataframe of site/year/parameter/depth combinations
  list_sites <- unique(df_sites$Site_ID)
  list_years <- unique(df_data$Year)
  list_param <- unique(df_data$Parameter)

  df_present <- df %>%
    dplyr::select(Site_ID, Year) %>%
    unique()
  df_all <- expand.grid(list_sites, list_years)
  colnames(df_all) <- c("Site_ID", "Year")
  df_missing <- dplyr::setdiff(df_all, df_present)

  df_join <- merge(df_present, list_param, by = NULL) %>%
    dplyr::rename(Parameter = y)
  if ("Depth" %in% colnames(df)) {
    df_join <- merge(df_join, unique(df$Depth), by = NULL) %>%
      dplyr::rename(Depth = y)
  }

  # Join df with dataframe, add rows for missing site/year combos
  df_join <- merge(df_join, df, all.x = TRUE)
  df <- dplyr::bind_rows(df_join, df_missing)

  # Add site data
  site_col <- c(
    "Site_ID", "Site_Name", "Latitude", "Longitude", "Town_Code",
    "County_Code", "State", "Watershed", "Group"
  )
  site_col <- intersect(colnames(df_sites), site_col)
  sites_temp <- dplyr::select(df_sites, all_of(site_col))
  if ("Town_Code" %in% colnames(sites_temp)) {
    sites_temp <- sites_temp %>%
      dplyr::select(!dplyr::any_of("State")) %>%
      dplyr::rename(Town = Town_Code)
  } else if ("County_Code" %in% colnames(sites_temp)) {
    sites_temp <- sites_temp %>%
      dplyr::select(!dplyr::any_of("State")) %>%
      dplyr::rename(County = County_Code)
  }

  df <- merge(df, sites_temp, all.x = TRUE)

  # Final tweaks
  col_order <- c(
    "Year", "Site_Name", "Site_ID", "Town", "County", "State",
    "Watershed", "Group", "Depth", "Parameter", "Unit",
    "score_typ", "score_num", "score_str", "Latitude", "Longitude"
  )
  col_order <- intersect(col_order, colnames(df))

  df <- df %>%
    dplyr::select(dplyr::all_of(col_order)) %>%
    dplyr::mutate(score_str = dplyr::case_when(
      !is.na(score_str) ~ score_str,
      !is.na(score_num) ~ "No Threshold Established",
      TRUE ~ "No Data Available"
    )) %>%
    dplyr::mutate(Parameter = dplyr::if_else(
      is.na(Parameter), "-", Parameter
    )) %>%
    dplyr::arrange(Site_Name, Parameter)

  df <- add_popup_text(df)
}
