# Check qaqc_sites ----
test_that("prep_sites works", {
  df_in <- tst$sites_raw
  colnames(df_in) <- c(
    "Site ID", "Site Name", "Latitude", "Longitude", "Town", "State",
    "Watershed", "Group", "Max Depth", "Red_Herring", "Blank_Herring"
  )

  df_colnames <- data.frame(
    wqdashboard = c("Site_ID", "Site_Name", "Max_Depth_m"),
    Custom = c("Site ID", "Site Name", "Max Depth")
  )

  expect_equal(
    suppressMessages(prep_sites(df_in, df_colnames)),
    tst$sites_raw
  )

  # Edge case
  df_colnames$Custom <- NA

  expect_equal(
    suppressMessages(prep_sites(df_in, df_colnames)),
    df_in
  )
})

# Check qaqc_sites ----
test_that("qaqc_sites works", {
  expect_equal(
    suppressMessages(qaqc_sites(tst$sites_raw)),
    tst$sites_qaqc
  )

  # Test edge case - default state
  df_in <- tst$sites_raw
  df_in$State <- NULL

  df_out <- tst$sites_qaqc
  df_out$State <- "RI"
  df_out <- df_out[c(1:5, 7:10, 6, 11:12)] # Move location of "State" column

  expect_equal(
    suppressMessages(qaqc_sites(df_in, "Rhode Island")),
    df_out
  )

  # Test edge case - weird depths
  df_in <- tst$sites_raw
  df_in$Max_Depth_m <- c(2, 5)

  df_out <- tst$sites_qaqc
  df_out$Max_Surface_Depth_m <- c(NA, 1)
  df_out$Max_Midwater_Depth_m <- c(NA, 4)
  df_out$Max_Depth_m <- c(2, 5)

  expect_equal(
    suppressMessages(
      qaqc_sites(df_in)
    ),
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
  df_out$Town <- c("Providence", "Worcester")

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
  df_out$Town <- NULL

  expect_equal(
    suppressMessages(
      format_sites(df_in)
    ),
    df_out
  )

  # Test edge case - drop sites
  df_out <- tst$sites_final[1, ]
  df_out[c("Town", "State", "Watershed")] <- NULL

  expect_equal(
    suppressMessages(
      format_sites(tst$sites_qaqc, site_list = "001")
    ),
    df_out
  )
})
