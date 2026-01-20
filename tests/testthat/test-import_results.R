# Test prep_results ----
test_that("prep_results works", {
  df_in <- tst$data_raw
  df_in$Parameter <- "DO"
  df_in$Result_Unit <- c(NA, "ug/l", NA, "ug/l", "ug/l", "mg/l", "mg/l", "mg/l")
  df_in$Detection_Limit_Unit <- c(
    "ug/l", "ug/l", "ug/l", "ug/l", "mg/l", "mg/l", "mg/l", "mg/l"
  )
  df_in$Qualifier <- c("BDL", NA, "BDL", NA, NA, NA, NA, NA)
  colnames(df_in) <- c(
    "Site", "Activity Type", "Date", "Depth", "Depth Unit", "Depth Category",
    "Parameter", "Result", "Unit", "Lower Detection Limit",
    "Upper Detection Limit", "Detection Limit Unit", "Qualifier"
  )

  df_colnames <- data.frame(
    wqdashboard = c(
      "Site_ID", "Activity_Type", "Depth_Unit", "Depth_Category", "Result_Unit",
      "Lower_Detection_Limit", "Upper_Detection_Limit", "Detection_Limit_Unit"
    ),
    Custom = c(
      "Site", "Activity Type", "Depth Unit", "Depth Category", "Unit",
      "Lower Detection Limit", "Upper Detection Limit", "Detection Limit Unit"
    )
  )
  df_param <- data.frame(
    wqdashboard = c("Dissolved oxygen (DO)", "Dissolved oxygen saturation"),
    Custom = c("DO", NA)
  )
  df_unit <- data.frame(
    wqdashboard = c("mg/L", "ug/L"),
    Custom = c("mg/l", "ug/l")
  )
  df_qual <- data.frame(
    wqdashboard = "DL",
    Custom = "BDL"
  )
  df_activity <- data.frame(
    wqdashboard = "Field Msr/Obs",
    Custom = NA
  )

  expect_equal(
    suppressMessages(
      prep_results(df_in, df_colnames, df_param, df_unit, df_qual, df_activity)
    ),
    tst$data_raw
  )

  # Test edge cases
  colnames(df_in) <- colnames(tst$data_raw)
  df_colnames$Custom <- NA

  expect_equal(
    suppressMessages(
      prep_results(df_in, df_colnames, df_param, df_unit, df_qual, df_activity)
    ),
    tst$data_raw
  )
})

# Test qaqc_results ----
test_that("qaqc_results works", {
  expect_equal(
    suppressMessages(
      qaqc_results(tst$data_raw, tst$sites_qaqc)
    ),
    tst$data_qaqc
  )
})

test_that("qaqc_results error messages", {
  # Test - missing data
  df_in <- tst$data_raw
  df_in$Result <- NA

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_qaqc)
    ),
    regexp = "Result missing. Check rows: 2, 4, 5, 6, 7, 8"
  )

  df_in <- tst$data_raw
  df_in$Result_Unit <- NA

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_qaqc)
    ),
    regexp = "Result_Unit missing. Check rows: 2, 4, 5, 6, 7, 8"
  )

  df_in <- tst$data_raw
  df_in$Lower_Detection_Limit <- c(NA, 0.1, NA, NA)
  df_in$Upper_Detection_Limit <- c(4, NA, NA, NA)
  df_in$Detection_Limit_Unit <- NA

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_qaqc)
    ),
    regexp = "Detection_Limit_Unit missing. Check rows: 1, 2, 5, 6"
  )


  # Test - invalid data
  df_in <- tst$data_raw
  df_in$Site_ID <- "003"

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_qaqc)
    ),
    regexp = "Invalid Site_ID: 003"
  )
})

# Test qaqc_cat_results ----
test_that("qaqc_cat_results works", {
  expect_equal(
    suppressMessages(
      qaqc_cat_results(tst$cat_raw, tst$sites_qaqc)
    ),
    tst$cat_qaqc
  )
})

test_that("qaqc_cat_results error messages", {
  # Test - missing data
  df_in <- tst$cat_raw
  df_in$Result <- NA

  expect_error(
    suppressMessages(
      qaqc_cat_results(df_in, tst$sites_qaqc)
    ),
    regexp = "Result missing. Check rows: 2, 4, 5, 6, 7, 8"
  )

  # Test - invalid data
  df_in <- tst$cat_raw
  df_in$Site_ID <- "003"

  expect_error(
    suppressMessages(
      qaqc_cat_results(df_in, tst$sites_qaqc)
    ),
    regexp = "Invalid Site_ID: 003"
  )
})

# Test format_results ----
test_that("format_results works", {
  expect_equal(
    suppressMessages(
      format_results(tst$data_qaqc, tst$sites_qaqc, tst$threshold_final)
    ),
    tst$data_final
  )

  # Edge case - no matching thresholds
  df_thresh <- tst$threshold_final[1, ]

  df_out <- tst$data_final
  df_out$Calculation <- "mean"
  df_out[c("Min", "Max", "Excellent", "Good", "Fair", "Best")] <- NA

  expect_equal(
    suppressMessages(
      format_results(tst$data_qaqc, tst$sites_qaqc, df_thresh)
    ),
    df_out
  )
})

# Test score_results ----
test_that("score_results works", {
  expect_equal(
    suppressMessages(
      score_results(tst$data_final, tst$sites_final)
    ),
    tst$data_score
  )

  # Test edge case
  df_in <- tst$data_final
  df_in$Date[1] <- as.Date("2022-06-30")
  df_in$Year[1] <- 2022

  df_sites <- tst$sites_final
  df_sites$Town <- NULL

  df_out <- data.frame(
    Year = c(2021, 2022, 2023, 2021, 2023, 2022),
    Site_Name = c("Site1", "Site1", "Site1", "Site2", "Site2", "Site2"),
    Site_ID = c("001", "001", "001", "002", "002", "002"),
    State = c(
      "Rhode Island", "Rhode Island", "Rhode Island", "Massachusetts",
      "Massachusetts", "Massachusetts"
    ),
    Watershed = c(
      "Narragansett Bay", "Narragansett Bay", "Narragansett Bay",
      "Upper Blackstone River", "Upper Blackstone River",
      "Upper Blackstone River"
    ),
    Group = c(
      "Coldwater", "Coldwater", "Coldwater", "Warmwater", "Warmwater",
      "Warmwater"
    ),
    Depth = "Surface",
    Parameter = "Dissolved oxygen (DO)",
    Unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", NA),
    score_typ = c("Minimum", "Minimum", "Minimum", "Average", "Average", NA),
    score_num = c(0.05, 0.05, 3, 6.5, 8.5, NA),
    score_str = c(
      "Poor", "Poor", "Poor", "No Threshold Established",
      "No Threshold Established", "No Data Available"
    ),
    Latitude = c(41.83, 41.83, 41.83, 42.28, 42.28, 42.28),
    Longitude = c(-71.41, -71.41, -71.41, -71.77, -71.77, -71.77),
    popup_loc = c(
      "<b>Site1</b> <br>State: Rhode Island <br>Watershed: Narragansett Bay <br>Group: Coldwater <br>Depth: Surface",
      "<b>Site1</b> <br>State: Rhode Island <br>Watershed: Narragansett Bay <br>Group: Coldwater <br>Depth: Surface",
      "<b>Site1</b> <br>State: Rhode Island <br>Watershed: Narragansett Bay <br>Group: Coldwater <br>Depth: Surface",
      "<b>Site2</b> <br>State: Massachusetts <br>Watershed: Upper Blackstone River <br>Group: Warmwater <br>Depth: Surface",
      "<b>Site2</b> <br>State: Massachusetts <br>Watershed: Upper Blackstone River <br>Group: Warmwater <br>Depth: Surface",
      "<b>Site2</b> <br>State: Massachusetts <br>Watershed: Upper Blackstone River <br>Group: Warmwater <br>Depth: Surface"
    ),
    popup_score = c(
      "<br>Minimum: 0.05 mg/L<br>Score: Poor",
      "<br>Minimum: 0.05 mg/L<br>Score: Poor",
      "<br>Minimum: 3 mg/L<br>Score: Poor", "<br>Average: 6.5 mg/L",
      "<br>Average: 8.5 mg/L", "<br><i>No data</i>"
    ),
    alt = c(
      "Site1, Poor", "Site1, Poor", "Site1, Poor", "Site2, 6.5 mg/L",
      "Site2, 8.5 mg/L", "Site2, No data"
    )
  )

  expect_equal(
    suppressMessages(
      score_results(df_in, df_sites)
    ),
    df_out
  )
})

# Test sidebar_var ----
test_that("sidebar_var works", {
  expect_equal(
    sidebar_var(
      tst$sites_final,
      tst$data_final,
      tst$data_score,
      df_cat = tst$cat_qaqc
    ),
    tst$s_var
  )

  # Edge case - no town, df_cat
  df_sites <- tst$sites_final
  df_sites$Town <- NULL

  loc_choices <- c("town", "watershed")
  names(loc_choices) <- c("By State", "By Watershed")

  list_out <- tst$s_var
  list_out["town"] <- list(NULL)
  list_out$loc_choices <- loc_choices
  list_out["param_cat"] <- list(NULL)

  expect_equal(
    sidebar_var(df_sites, tst$data_final, tst$data_score),
    list_out
  )

  # Edge case - no state or town
  df_sites$State <- NULL

  loc_choices <- "watershed"
  names(loc_choices) <- "By Watershed"

  list_out["state"] <- list(NULL)
  list_out$loc_choices <- loc_choices
  list_out$loc_tab <- "notoggle"

  expect_equal(
    sidebar_var(df_sites, tst$data_final, tst$data_score),
    list_out
  )

  # Edge case - no state, town, or watershed
  df_sites$Watershed <- NULL

  list_out["watershed"] <- list(NULL)
  list_out$loc_choices <- "blank"
  list_out$loc_tab <- "blank"

  expect_equal(
    sidebar_var(df_sites, tst$data_final, tst$data_score),
    list_out
  )
})

test_that("sidebar_var warning message", {
  sites_in <- tst$sites_final
  sites_in[3,1:2] = c("003", "Site3")
  sites_in[4,1:2] = c("004", "Site4")

  expect_warning(
    sidebar_var(
      sites_in,
      tst$data_final,
      tst$data_score,
      df_cat = tst$cat_qaqc
    ),
    regexp = "df_sites includes 2 sites with no result data: 003, 004"
  )
})
