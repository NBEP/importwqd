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

test_that("convert_unit works", {
  # Check conversions
  expect_equal(convert_unit(0, "deg C", "deg F"), 32) # simple conversion
  expect_equal(convert_unit(1, "mg/L", "ug/L"), 1000) # complex conversion
  expect_equal(convert_unit(12, "cfu/100mL", "MPN/100mL"), 12) # fecal counts

  # Check edge cases
  expect_equal(convert_unit(NA, "deg C", "deg F"), NA)
  expect_equal(convert_unit(42, "deg C", "deg C"), 42)
  expect_equal(convert_unit(0, "deg C", "mg"), -999999)
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

test_that("pretty_number works", {
  expect_equal(
    pretty_number(12.234234234),
    12.23
  )
  expect_equal(
    pretty_number(0.00002342),
    0.000023
  )
})
