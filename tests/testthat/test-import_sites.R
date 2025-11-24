# Check qaqc_sites ----
test_that("qaqc_sites works", {
  expect_equal(
    suppressMessages(qaqc_sites(tst$sites_raw)),
    tst$sites_qaqc
  )

  # Edge case
  df_in <- tst$sites_raw
  df_in$State <- NULL

  df_out <- tst$sites_qaqc
  df_out$State <- "RI"
  df_out <- df_out[c(1:6, 8:11, 7, 12:13)] # Move location of "State" column

  expect_equal(
    suppressMessages(qaqc_sites(df_in, "Rhode Island")),
    df_out
  )
})

test_that("qaqc_sites error messages", {
  # Test - missing mandatory columns
  df_in <- tst$sites_raw
  df_in$Site_ID <- NULL
  df_in$Site_Name <- NULL

  expect_error(
    suppressMessages(
      qaqc_sites(df_in)
    ),
    regexp = "The following columns are missing: Site_ID, Site_Name"
  )

  # Test - duplicate values
  df_in <- tst$sites_raw
  df_in$Site_ID <- "foo"

  expect_error(
    suppressMessages(
      qaqc_sites(df_in)
    ),
    regexp = "Duplicate values found for Site_ID: check rows 1, 2"
  )

  # Test - non-numeric values
  df_in <- tst$sites_raw
  df_in$Latitude <- c(41.83, "foofy")

  expect_error(
    suppressMessages(
      qaqc_sites(df_in)
    ),
    regexp = "Non-numeric values detected in Latitude. Check rows: 2"
  )

  # Test - illogical depth value
  df_in <- tst$sites_raw
  df_in$Max_Surface_Depth_m <- 1
  df_in$Max_Midwater_Depth_m <- 2
  df_in$Max_Depth_m <- c(1, 3)

  expect_error(
    suppressMessages(
      qaqc_sites(df_in)
    ),
    regexp = "Illogical depth values. Check rows: 1"
  )
})

# Check format_sites -----
test_that("format_sites works", {
  # Test basic
  expect_equal(
    suppressMessages(
      format_sites(tst$sites_qaqc)
    ),
    tst$sites_final
  )

  # Test edge case - no state
  df_in <- tst$sites_qaqc
  df_in$State <- NA

  df_out <- tst$sites_final
  df_out$State <- NULL
  df_out$Town_Code <- c("Providence", "Worcester")

  expect_equal(
    suppressMessages(
      format_sites(df_in)
    ),
    df_out
  )

  # Test edge case - no town
  df_in <- tst$sites_qaqc
  df_in$Town <- NA

  df_out <- tst$sites_final
  df_out$Town_Code <- NULL
  df_out$County <- NULL
  df_out$County_Code <- c("Providence County, RI", "Worcester County, MA")

  expect_equal(
    suppressMessages(
      format_sites(df_in)
    ),
    df_out
  )

  # Test edge case - weird depths
  df_in <- tst$sites_qaqc
  df_in$Max_Depth_m <- c(2, 5)

  df_out <- tst$sites_final
  df_out$Max_Surface <- c(NA, 1)
  df_out$Max_Midwater <- c(NA, 4)
  df_out$Max_Depth <- c(2, 5)

  expect_equal(
    suppressMessages(
      format_sites(df_in)
    ),
    df_out
  )
})
