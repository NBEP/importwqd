#' List rows with quality control data or failed qualifier code
#'
#'
#' @description `skip_rows()` lists rows with a suspect qualifier code or
#' quality control activity type.
#'
#' @param df Input dataframe.
#'
#' @return Updated QAQC check.
#'
#' @noRd
skip_rows <- function(df) {
  if ("Qualifier" %in% colnames(df)) {
    chk <- df$Qualifier %in% qaqc_flag$suspect
  } else {
    chk <- rep(FALSE, nrow(df))
  }

  if ("Activity_Type" %in% colnames(df)) {
    chk2 <- !is.na(df$Activity_Type) &
      stringr::str_detect(df$Activity_Type, "Quality Control")
    chk <- chk | chk2
  }

  return(chk)
}

# Set Nondetect Values
#'
#' @description Sets values for non-detect data. If detection limits are
#'  provided, non-detect data is set to half the detection limit. If detection
#'  limits are not provided, non-detect data is set to zero.
#'
#' @param df Dataframe.
#'
#' @return Updated dataframe
#'
#' @noRd
set_nondetect_values <- function(df) {
  chk <- df$Result %in% c("BDL")
  if ("Qualifier" %in% colnames(df)) {
    chk <- chk | df$Qualifier %in% qaqc_flag$nondetect
  }
  chk <- chk & !skip_rows(df)

  if (all(!chk)) {
    return(df)
  }

  df_d <- df[!chk, ]
  df_nd <- df[chk, ] %>%
    dplyr::mutate(
      Qualifier = dplyr::if_else(
        is.na(Qualifier),
        "DL",
        Qualifier
      )
    )

  if (all(c("Detection_Limit", "Detection_Limit_Unit") %in% colnames(df))) {
    check_val_numeric(df, "Detection_Limit")
    check_val_missing(df, "Detection_Limit_Unit")
    df_nd <- df_nd %>%
      dplyr::mutate(
        Result = dplyr::case_when(
          is.na(Detection_Limit) ~ 0,
          Detection_Limit <= 0 ~ Detection_Limit,
          TRUE ~ Detection_Limit / 2
        )
      ) %>%
      dplyr::mutate(Result_Unit = Detection_Limit_Unit)
  } else {
    df_nd <- df_nd %>%
      dplyr::mutate(Result = 0) %>%
      dplyr::select(!Result_Unit)

    df_temp <- df_d %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarise("Result_Unit" = dplyr::last(Result_Unit))

    df_nd <- dplyr::left_join(df_nd, df_temp, by = dplyr::join_by(Parameter))
  }

  df <- rbind(df_d, df_nd)

  return(df)
}

#' Convert depth to meters
#'
#' @description Converts columns "Depth", "Depth_Unit" to meters.
#'
#' @param df Dataframe
#'
#' @noRd
depth_to_m <- function(df) {
  if (!"Depth_Unit" %in% colnames(df)) {
    stop("The following column is missing: Depth_Unit", call. = FALSE)
  }

  # Exempt rows
  exempt <- stringr::str_detect(df$Parameter, "Depth")
  exempt <- exempt | skip_rows(df)

  chk <- (!is.na(df$Depth) | exempt)
  if (any(!chk)) {
    rws <- which(!chk)
    warning("\tDepth is missing in rows ", paste(rws, collapse = ", "),
      call. = FALSE
    )
  }
  chk <- (!is.na(df$Depth_Unit) | exempt)
  if (any(!chk)) {
    rws <- which(!chk)
    warning("\tDepth_Unit is missing in rows ", paste(rws, collapse = ", "),
      call. = FALSE
    )
  }

  exempt <- (df$Depth_Unit %in% c(NA, "m") | exempt)
  if (all(exempt)) {
    return(df)
  }

  exempt <- which(exempt)

  df_temp <- df[-exempt, ] %>%
    dplyr::select(Depth, Depth_Unit) %>%
    unique() %>%
    dplyr::mutate(
      temp_depth = mapply(
        function(x, y) convert_unit(x, y, "m"),
        Depth, Depth_Unit
      )
    )

  df <- dplyr::left_join(df, df_temp, by = dplyr::join_by(Depth, Depth_Unit))
  df <- df %>%
    dplyr::mutate(
      Depth = dplyr::if_else(
        is.na(temp_depth),
        Depth,
        temp_depth
      )
    ) %>%
    dplyr::mutate(
      Depth_Unit = dplyr::if_else(
        is.na(temp_depth),
        Depth_Unit,
        "m"
      )
    ) %>%
    dplyr::select(!temp_depth)

  message("\tConverted depth to meters")
  return(df)
}

#' Assign depth category
#'
#' @description Assigns depth category. Run `depth_to_m` first.
#'
#' @param df Input dataframe.
#' @param sites Site dataframe. Only present for testing purposes.
#'
#' @noRd
assign_depth_category <- function(df, sites = df_sites) {
  if ("Depth_Category" %in% colnames(df)) {
    ok_cat <- c("Surface", "Midwater", "Near Bottom", "Bottom")
    chk <- df$Depth_Category %in% c(NA, ok_cat)
    if (any(!chk)) {
      rws <- which(!chk)
      df[rws, "Depth_Category"] <- NA
      warning("\tRemoved invalid Depth_Category in rows ",
        paste(rws, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    message("\tAdded column Depth_Category")
    df <- dplyr::mutate(df, Depth_Category = NA)
  }

  depth_col <- c(
    "Max_Depth_Surface", "Max_Depth_Midwater",
    "Max_Depth_Near_Bottom"
  )
  sites <- dplyr::select(sites, dplyr::any_of(c("Site_ID", depth_col)))

  missing_col <- setdiff(depth_col, colnames(sites))
  for (field in missing_col) {
    df <- dplyr::mutate(df, {{ field }} := NA)
  }

  df <- dplyr::left_join(
    df, sites,
    by = "Site_ID",
    keep = FALSE
  ) %>%
    dplyr::mutate(
      Max_Depth_Surface = dplyr::if_else(
        is.na(Max_Depth_Surface),
        1,
        Max_Depth_Surface
      )
    ) %>%
    dplyr::mutate(
      Max_Depth_Midwater = dplyr::if_else(
        is.na(Max_Depth_Midwater),
        max(Depth) + 1,
        Max_Depth_Midwater
      )
    ) %>%
    dplyr::mutate(
      Max_Depth_Near_Bottom = dplyr::if_else(
        is.na(Max_Depth_Near_Bottom),
        max(Depth) + 1,
        Max_Depth_Near_Bottom
      )
    ) %>%
    dplyr::mutate(
      Depth_Category = dplyr::case_when(
        !is.na(Depth_Category) ~ Depth_Category,
        is.na(Depth) | Depth_Unit != "m" ~ Depth_Category,
        Depth > Max_Depth_Near_Bottom ~ "Bottom",
        Depth > Max_Depth_Midwater ~ "Near Bottom",
        Depth > Max_Depth_Surface ~ "Midwater",
        TRUE ~ "Surface"
      )
    ) %>%
    dplyr::select(!dplyr::any_of(depth_col))

  return(df)
}
