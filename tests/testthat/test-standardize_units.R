# Test standardize_result_units ----
test_that("standardize_result_units works", {
  df_out <- tst$data_raw
  df_out$Result <- c(NA, 3, NA, 4, 6, 8, 7, 9)
  df_out$Result_Unit <- "mg/L"

  expect_equal(
    standardize_result_units(tst$data_raw),
    df_out
  )

  # Edge cases
  df_in <- tst$data_raw
  df_in$Result <- c(NA, 3, NA, 4, 6, 8, 7, 9)
  df_in$Result_Unit <- "mg/L"

  expect_equal(
    standardize_result_units(df_in),
    df_in
  )

  df_in <- tst$data_raw
  df_in$Result <- c(3, 3000, 5000, 4000, 6000, 8, 7, 9)
  df_in$Result_Unit <- c(
    "mg/L", "ug/L", "ug/L", "ug/L", "ug/L", "mg/L", "mg/L", "mg/L"
  )

  df_out <- tst$data_raw
  df_out$Result <- c(3, 3, 5, 4, 6, 8, 7, 9)
  df_out$Result_Unit <- "mg/L"

  expect_equal(
    standardize_result_units(df_in),
    df_out
  )
})

test_that("standardize_result_units error messages", {
  df_in <- tst$data_raw
  df_in$Result_Unit <- c(
    "ug/L", "deg C", "ug/L", "ug/L", "ug/L", "mg/L", "mg/L", "mg/L"
  )

  expect_error(
    standardize_result_units(df_in),
    regexp = "Unable to standardize units"
  )
})

# Test standardize_detection_units ----
test_that("standardize_detection_units works", {
  df_in <- tst$data_raw
  df_in$Result <- c(NA, 3, NA, 4, 6, 8, 7, 9)
  df_in$Result_Unit <- "mg/L"

  df_out <- df_in
  df_out$Lower_Detection_Limit <- 0.1
  df_out$Detection_Limit_Unit <- "mg/L"

  expect_equal(
    standardize_detection_units(df_in),
    df_out
  )

  # No updates needed
  df_in <- tst$data_raw
  df_in$Result <- c(NA, 3, NA, 4, 6, 8, 7, 9)
  df_in$Result_Unit <- "mg/L"
  df_in$Lower_Detection_Limit <- 0.1
  df_in$Detection_Limit_Unit <- "mg/L"

  expect_equal(
    standardize_detection_units(df_in),
    df_in
  )
})

test_that("standardize_detection_units error messages", {
  df_in <- tst$data_raw
  df_in$Result <- c(NA, 3, NA, 4, 6, 8, 7, 9)
  df_in$Result_Unit <- "mg/L"
  df_in$Detection_Limit_Unit <- c(
    "deg C", "ug/L", "ug/L", "ug/L", "deg C", "mg/L", "mg/L", "mg/L"
  )

  expect_error(
    standardize_detection_units(df_in),
    regexp = "Result and detection units are incompatible. Check rows: 1, 5"
  )
})
