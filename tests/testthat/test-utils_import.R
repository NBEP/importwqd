test_that("check_col_missing works", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(1, 2, 3)
  )

  expect_no_error(
    check_col_missing(df, c("Col1", "Col2"))
  )

  expect_error(
    df %>% check_col_missing(c("Col1", "Col3")),
    regexp = "The following columns are missing: Col3"
  )
})

test_that("check_val_duplicate works", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c("D", "E", "E"),
    Col3 = c("G", "G", "G")
  )

  # Check - one column
  expect_no_error(check_val_duplicate(df, "Col1"))
  expect_error(
    check_val_duplicate(df, "Col2"),
    regexp = "Duplicate values found for Col2: check rows 2, 3"
  )
  expect_warning(
    check_val_duplicate(df, "Col2", is_stop = FALSE),
    regexp = "\tDuplicate values found for Col2: check rows 2, 3"
  )

  # Check - multiple columns
  expect_no_error(check_val_duplicate(df, c("Col1", "Col2")))
  expect_error(
    check_val_duplicate(df, c("Col2", "Col3")),
    regexp = "Duplicate values found for Col2, Col3: check rows 2, 3"
  )
})

test_that("check_val_missing works", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c("D", NA, NA)
  )

  # Basic tests
  expect_equal(check_val_missing(df, "Col1"), df)
  expect_error(
    check_val_missing(df, "Col2"),
    regexp = "2 empty rows detected in Col2. Check rows: 2, 3"
  )
  expect_warning(
    check_val_missing(df, "Col2", is_stop = FALSE),
    regexp = "\t2 empty rows detected in Col2. Check rows: 2, 3"
  )

  # Edge case - ignore rows
  expect_error(
    check_val_missing(df, "Col2", ignore_rows = 3),
    regexp = "1 empty rows detected in Col2. Check rows: 2"
  )
  expect_equal(
    check_val_missing(df, "Col2", ignore_rows = c(2, 3)),
    df
  )
})

test_that("drop_empty_col works", {
  df_in <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(NA, "E", "E"),
    Col3 = NA
  )

  df_out <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(NA, "E", "E")
  )

  expect_equal(
    suppressMessages(
      df_in %>%
        drop_empty_col("Col1") %>%
        drop_empty_col("Col2") %>%
        drop_empty_col("Col3")
    ),
    df_out
  )
})

test_that("drop_uniform_col works", {
  df_in <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(NA, "E", "E"),
    Col3 = c("G", "G", "G")
  )

  expect_equal(
    suppressMessages(
      df_in %>%
        drop_uniform_col("Col1") %>%
        drop_uniform_col("Col2") %>%
        drop_uniform_col("Col3")
    ),
    data.frame(
      Col1 = c("A", "B", "C"),
      Col2 = c(NA, "E", "E")
    )
  )

  expect_equal(
    suppressMessages(
      df_in %>%
        drop_uniform_col("Col1", include_na = FALSE) %>%
        drop_uniform_col("Col2", include_na = FALSE) %>%
        drop_uniform_col("Col3", include_na = FALSE)
    ),
    data.frame(
      Col1 = c("A", "B", "C")
    )
  )
})

test_that("add_thresholds works", {
  thresh <- tst$threshold_final

  expect_equal(
    add_thresholds(
      thresh, "001", "Coldwater", "RI", "Surface", "Dissolved oxygen (DO)"
    ),
    list(
      Calculation = "min",
      Min = 5,
      Max = NA_integer_,
      Excellent = 8,
      Good = 6.5,
      Fair = 5,
      Best = "high"
    )
  )
  expect_equal(
    add_thresholds(
      thresh, "002", "Warmwater", "MA", "Surface", "Dissolved oxygen (DO)"
    ),
    list(
      Calculation = "mean",
      Min = NA,
      Max = NA,
      Excellent = NA,
      Good = NA,
      Fair = NA,
      Best = NA
    )
  )
})

# Test standardize_threshold_units ----
test_that("update_threshold_units works", {
  df_param <- data.frame(
    "Parameter" = "Dissolved oxygen (DO)",
    "Result_Unit" = "ug/L"
  )

  df_out <- tst$threshold_final[2:4, ]
  df_out$Unit <- c("ug/L", "ug/L", "ug/L")
  df_out$Min <- c(5000, 4800, 5000)
  df_out$Excellent <- c(8000, NA, NA)
  df_out$Good <- c(6500, NA, NA)
  df_out$Fair <- c(5000, NA, NA)
  rownames(df_out) <- NULL

  expect_equal(
    update_threshold_units(tst$threshold_final, df_param),
    df_out
  )

  # Check edge cases
  df_out <- tst$threshold_final[2:4, ]
  rownames(df_out) <- NULL

  expect_equal(
    update_threshold_units(tst$threshold_final, tst$data_qaqc),
    df_out
  )

  df_thresh <- tst$threshold_final[1, ]
  df_out <- tst$threshold_final[0, ]

  expect_equal(
    update_threshold_units(df_thresh, tst$data_qaqc),
    df_out
  )
})

test_that("update_threshold_units error messages", {
  df_results <- data.frame(
    "Parameter" = "Dissolved oxygen (DO)",
    "Result_Unit" = "deg C"
  )

  expect_warning(
    update_threshold_units(tst$threshold_final, df_results),
    regexp = "Removed thresholds for Dissolved oxygen"
  )
})

test_that("try_rename works", {
  df_in <- data.frame(
    Col1 = c("foofy", "foo"),
    Col2 = c("foo", "bar")
  )

  df_var <- data.frame(
    wqdashboard = c("what", "a", "superb", "owl"),
    Custom = c(NA, NA, "foo", "bar")
  )

  expect_equal(
    try_rename(df_in, "Col2", df_var),
    data.frame(
      Col1 = c("foofy", "foo"),
      Col2 = c("superb", "owl")
    )
  )

  # Test edge cases
  df_var$Custom <- NA

  expect_message(
    try_rename(df_in, "Col2", df_var),
    regexp = "\tDid not update Col2"
  )
  expect_message(
    try_rename(df_in, "Col3", df_var),
    regexp = "\tDid not find Col3"
  )
})
