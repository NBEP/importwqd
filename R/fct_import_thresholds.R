#' QAQC Thresholds
#'
#' @description Runs quality control on threshold dataframe.
#'
#' @param df Input dataframe.
#'
#' @return Updated dataframe.
#'
#' @noRd
qaqc_thresholds <- function(df) {
  # Define variables ----------------------------------------------------------
  field_need <- c("Parameter", "Unit")
  field_optional <- c("State", "Group", "Site_ID", "Depth_Category")
  field_tvalues <- c(
    "Threshold_Min", "Threshold_Max", "Excellent", "Good",
    "Fair"
  )
  field_all <- c(field_optional, field_need, "Min_Max_Mean", field_tvalues)

  # QAQC columns --------------------------------------------------------------
  message("Checking thresholds...\n")
  check_column_missing(df, field_need)

  chk <- intersect(field_tvalues, colnames(df))
  if (length(chk) < 1) {
    stop("Must include at least one threshold column", call. = FALSE)
  }

  # Drop extra columns
  field_keep <- intersect(field_all, colnames(df))
  chk <- length(df) - length(field_keep)
  if (chk > 0) {
    df <- dplyr::select(df, all_of(field_keep)) # Drop extra columns
    message("\t", toString(chk), " columns removed")
  }

  chk <- intersect(field_optional, colnames(df))
  for (field in chk) {
    chk2 <- is.na(df[field])
    if (all(chk2)) {
      df <- dplyr::select(df, !{{ field }})
      message("\tDropped empty column: ", field)
    }
  }

  # Add missing columns (need threshold values for calculate_score)
  field_missing <- setdiff(field_tvalues, colnames(df))
  if (length(field_missing) > 0) {
    warning("Adding blank columns: ",
      paste(field_missing, collapse = ", "),
      call. = FALSE
    )
    df[field_missing] <- NA
  }

  # QAQC values --------------------------------------------------------------
  for (field in field_need) {
    check_val_missing(df, field = field)
  }

  if (all(c("Group", "Site_ID") %in% colnames(df))) {
    chk <- (!is.na(df$Site_ID) & !is.na(df$Group))
    if (any(chk)) {
      rws <- which(chk)
      stop("Group and site thresholds must be on seperate rows. Check rows: ",
        paste(rws, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (all(c("State", "Site_ID") %in% colnames(df))) {
    chk <- (!is.na(df$Site_ID) & !is.na(df$State))
    if (any(chk)) {
      rws <- which(chk)
      stop("State and site thresholds must be on seperate rows. Check rows:",
        paste(rws, collapse = ", "),
        call. = FALSE
      )
    }
  }

  col_id <- c("State", "Group", "Site_ID", "Parameter")
  col_id <- intersect(col_id, colnames(df))
  check_val_duplicate(df, field = col_id)

  if ("State" %in% colnames(df)) {
    check_val_missing(df, "State", is_stop = FALSE)
  }
  if ("Min_Max_Mean" %in% colnames(df)) {
    check_val_missing(df, "Min_Max_Mean", is_stop = FALSE)
  }
  if ("Depth_Category" %in% colnames(df)) {
    ok_cat <- c("Surface", "Midwater", "Near Bottom", "Bottom")
    chk <- df$Depth_Category %in% c(ok_cat, NA)
    if (any(!chk)) {
      rws <- which(!chk)
      stop("Invalid Depth_Category. Acceptable values: ",
        paste(ok_cat, collapse = ", "), ". Check rows: ",
        paste(rws, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Check threshold values
  for (field in field_tvalues) {
    check_val_numeric(df, field = field)
  }

  chk <- is.na(df$Threshold_Min) & is.na(df$Threshold_Max) &
    is.na(df$Excellent) & is.na(df$Good) & is.na(df$Fair)
  if (any(chk)) {
    rws <- which(chk)
    stop("Rows must contain at least one threshold value. Check rows ",
      paste(rws, collapse = ", "),
      call. = FALSE
    )
  }

  # Check units
  var_sub <- find_var_names(varnames_units, "Other", "WQX")
  df <- rename_all_var(df, "Unit", var_sub$old_names, var_sub$new_names)

  # Sort data -----
  field_sort <- c("Site_ID", "State", "Group", "Depth_Category")
  field_sort <- intersect(field_sort, colnames(df))
  df <- dplyr::arrange_at(df, field_sort)

  message("\nQAQC complete")
  return(df)
}
