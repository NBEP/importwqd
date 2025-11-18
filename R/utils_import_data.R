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
