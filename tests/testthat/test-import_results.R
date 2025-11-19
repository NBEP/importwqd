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
      format_results(tst$data_qaqc)
    ),
    tst$data_final
  )
})
