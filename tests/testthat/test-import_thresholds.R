test_that("prep_thresholds works", {
  df_in <- tst$threshold_raw
  df_in$Parameter <- c(
    "DO", "DO", "DO", "pH", "Nitrate", "Enterococcus", "Enterococcus"
  )
  df_in$Unit <- c(
    "mg/l", "mg/l", "mg/l", "None", "mg/l", "cfu/100ml", "cfu/100ml"
  )

  df_param <- data.frame(
    wqdashboard = c("Dissolved oxygen", "Dissolved oxygen saturation"),
    Custom = c("DO", NA)
  )
  df_unit <- data.frame(
    wqdashboard = c("mg/L", "ng/L", "cfu/100mL"),
    Custom = c("mg/l", "ng/l", "cfu/100ml")
  )

  expect_equal(
    suppressMessages(prep_thresholds(df_in, df_param, df_unit)),
    tst$threshold_raw
  )
})

test_that("qaqc_thresholds works", {
  # Simple
  df_in <- tst$threshold_raw[1:4, ]
  df_out <- tst$threshold_qaqc[1:4, ]

  expect_equal(
    suppressMessages(qaqc_thresholds(df_in)),
    df_out
  )

  # Duplicate rows
  expect_equal(
    suppressWarnings(suppressMessages(qaqc_thresholds(tst$threshold_raw))),
    tst$threshold_qaqc
  )

  expect_warning(
    suppressMessages(qaqc_thresholds(tst$threshold_raw)),
    regexp = paste(
      "Multiple thresholds detected for same location/parameter/depth.",
      "Check rows: 6, 7"
    )
  )
})

test_that("qaqc_thresholds error messages", {
  # Missing columns
  df_in <- tst$threshold_raw
  df_in$Parameter <- NULL

  expect_error(
    suppressMessages(qaqc_thresholds(df_in))
  )

  df_in <- tst$threshold_raw
  df_in$Threshold_Min <- NULL
  df_in$Threshold_Max <- NULL
  df_in$Excellent <- NULL

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Data must include at least one threshold column"
    )
  )

  # Bad data
  df_in <- tst$threshold_raw
  df_in$Site_ID <- "foo"

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Site and group thresholds must be on seperate rows.",
      "Check rows: 1, 2, 3, 4"
    )
  )

  df_in$Group <- NA

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Site and state thresholds must be on seperate rows.",
      "Check rows: 1, 2, 3, 4"
    )
  )

  df_in <- tst$threshold_raw
  df_in$Threshold_Min <- c(NA, NA, 5, 6.5, NA, NA, NA)
  df_in$Good <- NA

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Each row must include at least one threshold value. Check rows: 1, 2"
    )
  )

  df_in <- tst$threshold_raw
  df_in$Excellent <- c(5, 5, 4, NA, NA, NA, NA)
  df_in$Good <- 4
  df_in$Fair <- c(5, 4, 4, NA, NA, NA, NA)

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Illogical values for Excellent, Good, and Fair. Check rows: 1, 3"
    )
  )
})

test_that("format_thresholds works", {
  # Simple
  df_in <- tst$threshold_qaqc[1:4, ]

  df_out <- tst$threshold_final[1:4, ]
  df_out$Min <- as.numeric(df_out$Min)
  df_out$Max <- as.numeric(df_out$Max)
  df_out$Excellent <- as.numeric(df_out$Excellent)
  df_out$Good <- as.numeric(df_out$Good)
  df_out$Fair <- as.numeric(df_out$Fair)

  expect_equal(
    suppressMessages(
      format_thresholds(df_in)
    ),
    df_out
  )

  # Complex
  expect_equal(
    suppressMessages(
      format_thresholds(tst$threshold_qaqc)
    ),
    tst$threshold_final
  )
})
