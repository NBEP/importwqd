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

  # Test - include_na = TRUE
  df_out <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(NA, "E", "E")
  )

  expect_equal(
    suppressMessages(
      df_in %>%
        drop_uniform_col("Col1") %>%
        drop_uniform_col("Col2") %>%
        drop_uniform_col("Col3")
    ),
    df_out
  )

  # Test - include_na = FALSE
  df_out <- data.frame(
    Col1 = c("A", "B", "C")
  )

  expect_equal(
    suppressMessages(
      df_in %>%
        drop_uniform_col("Col1", include_na = FALSE) %>%
        drop_uniform_col("Col2", include_na = FALSE) %>%
        drop_uniform_col("Col3", include_na = FALSE)
    ),
    df_out
  )
})
