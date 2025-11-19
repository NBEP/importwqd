#' Calculate annual parameter score
#'
#' @description `calculate_score()` calculates annual numeric and categorical
#' scores for each parameter/site/depth combination.
#'
#' The numeric score returns the annual minimum, maximum, median, or mean value.
#' If the threshold table does not specify which value to return, the mean value
#' is used.
#'
#' The categorical score compares the numeric score to the threshold table and
#' returns a value of Excellent, Good, Fair, Poor, Does Not Meet Criteria, or
#' Meets Criteria.
#'
#' @param site_id Site ID.
#' @param parameter Parameter.
#' @param unit Parameter unit.
#' @param depth Depth category. Default value NA.
#' @param score_max Maximum score.
#' @param score_min Minimum score.
#' @param score_mean Average score.
#' @param score_median Median score.
#'
#' @seealso score_results()
#'
#' @return List including a numeric score (score_num), way the numeric score was
#' calculated (score_typ), and a categorical score (score_str).
calculate_score <- function(
  site_id, parameter, unit, depth = NA, score_max, score_min, score_mean,
  score_median
) {
  # Find thresholds
  df <- find_threshold(site_id, parameter, depth)

  # If no thresholds, return data
  if (is.null(df)) {
    return(
      list(
        score_typ = "Average",
        score_num = score_mean,
        score_str = NA
      )
    )
  }

  # How calculate score - min, max, median, or mean?
  score <- score_mean
  typ <- "Average"

  if (df$Min_Max_Mean %in% c("max", "maximum")) {
    score <- score_max
    typ <- "Maximum"
  } else if (df$Min_Max_Mean %in% c("min", "minimum")) {
    score <- score_min
    typ <- "Minimum"
  } else if (df$Min_Max_Mean %in% "median") {
    score <- score_median
    typ <- "Median"
  }

  # Standardize units
  new_score <- convert_unit(score, unit, df$Unit)

  # If unable to standardize units, return data
  if (new_score == -999999) {
    return(
      list(
        score_typ = typ,
        score_num = score,
        score_str = NA
      )
    )
  }

  # Find category score
  thresh_list <- c(df$Excellent, df$Good, df$Fair)

  chk_asc <- identical(
    sort(thresh_list),
    thresh_list
  )
  chk_des <- identical(
    sort(thresh_list, FALSE),
    thresh_list
  )
  chk_fail <- c(
    !is.na(df$Threshold_Min) & new_score < df$Threshold_Min,
    !is.na(df$Threshold_Max) & new_score > df$Threshold_Max
  )
  chk_pass <- !is.na(df$Threshold_Min) | !is.na(df$Threshold_Max)

  if (chk_asc) {
    if (new_score >= df$Excellent) {
      score2 <- "Excellent"
    } else if (new_score >= df$Good) {
      score2 <- "Good"
    } else if (new_score >= df$Fair) {
      score2 <- "Fair"
    } else {
      score2 <- "Poor"
    }
  } else if (chk_des) {
    if (new_score <= df$Excellent) {
      score2 <- "Excellent"
    } else if (new_score <= df$Good) {
      score2 <- "Good"
    } else if (new_score <= df$Fair) {
      score2 <- "Fair"
    } else {
      score2 <- "Poor"
    }
  } else if (chk_fail) {
    score2 <- "Does Not Meet Criteria"
  } else if (chk_pass) {
    score2 <- "Meets Criteria"
  } else {
    score2 <- NA
  }

  return(
    list(
      score_typ = typ,
      score_num = score,
      score_str = score2
    )
  )
}
