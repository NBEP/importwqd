# Test qaqc_results ----
test_that("qaqc_results works", {
  expect_equal(
    suppressMessages(
      qaqc_results(tst$data_raw, tst$sites_final)
    ),
    tst$data_qaqc
  )

  # Additional tests - depth handling, E. coli
  df_in <- tst$data_raw
  df_in$Parameter <- "Escherichia coli"
  df_in$Depth <- c(0.5, 1, 11, 12)

  df_out <- tst$data_qaqc
  df_out$Parameter <- "E. coli"
  df_out$Depth <- c(0.5, 1, 11, 12)
  df_out$Depth_Category <- c(
    "Surface", "Midwater", "Bottom", "Bottom", "Surface", "Midwater",
    "Near Bottom", "Bottom"
  )

  expect_equal(
    suppressMessages(
      qaqc_results(df_in, tst$sites_final)
    ),
    df_out
  )
})

test_that("qaqc_results error messages", {
  # Test - missing data
  df_in <- tst$data_raw
  df_in$Result <- NA

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_final)
    ),
    regexp = "Result missing. Check rows: 2, 4, 5, 6, 7, 8"
  )

  df_in <- tst$data_raw
  df_in$Result_Unit <- NA

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_final)
    ),
    regexp = "Result_Unit missing. Check rows: 2, 4, 5, 6, 7, 8"
  )

  df_in <- tst$data_raw
  df_in$Lower_Detection_Limit <- c(NA, 0.1, NA, NA)
  df_in$Upper_Detection_Limit <- c(4, NA, NA, NA)
  df_in$Detection_Limit_Unit <- NA

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_final)
    ),
    regexp = "Detection_Limit_Unit missing. Check rows: 1, 2, 5, 6"
  )


  # Test - invalid data
  df_in <- tst$data_raw
  df_in$Site_ID <- "003"

  expect_error(
    suppressMessages(
      qaqc_results(df_in, tst$sites_final)
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
    State = c("RI", "RI", "RI", "MA", "MA", "MA"),
    Watershed = c(
      "Narragnasett Bay", "Narragnasett Bay", "Narragnasett Bay",
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
    score_typ = c("min", "min", "min", "mean", "mean", NA),
    score_num = c(0.05, 0.05, 3, 6.5, 8.5, NA),
    score_str = c(
      "Poor", "Poor", "Poor", "No Threshold Established",
      "No Threshold Established", "No Data Available"
    ),
    Latitude = c(41.83, 41.83, 41.83, 42.28, 42.28, 42.28),
    Longitude = c(-71.41, -71.41, -71.41, -71.77, -71.77, -71.77)
  )

  expect_equal(
    suppressMessages(
      score_results(df_in, df_sites)
    ),
    df_out
  )
})
