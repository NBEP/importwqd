test_that("qaqc_thresholds works", {
  df_in <- tst$threshold_raw[1:4, ]
  df_out <- tst$threshold_qaqc[1:4, ]

  expect_equal(
    suppressMessages(qaqc_thresholds(df_in)),
    df_out
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
  df_in$Threshold_Min <- c(NA, NA, 5, 6.5, NA)
  df_in$Good <- NA

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Each row must include at least one threshold value. Check rows: 1, 2"
    )
  )

  df_in <- tst$threshold_raw
  df_in$Excellent <- c(5, 5, 4, NA, NA)
  df_in$Good <- 4
  df_in$Fair <- c(5, 4, 4, NA, NA)

  expect_error(
    suppressMessages(qaqc_thresholds(df_in)),
    regexp = paste(
      "Illogical values for Excellent, Good, and Fair. Check rows: 1, 3"
    )
  )

  # Warnings
  expect_warning(
    suppressMessages(qaqc_thresholds(tst$threshold_raw))
  )
})

test_that("format_thresholds works", {
  expect_equal(
    suppressMessages(
      format_thresholds(tst$threshold_qaqc)
    ),
    tst$threshold_final
  )
})
