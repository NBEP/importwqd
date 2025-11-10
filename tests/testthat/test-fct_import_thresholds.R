test_that("Error message if missing columns", {
  df <- data.frame(
    State = c("MA", "MA"),
    Depth_Category = c("Surface", "Surface"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    Excellent = c(0.3, 0.025),
    Good = c(0.6, 0.05),
    Fair = c(0.9, 0.1),
    extra_col = c("Red", "Herring")
  )
  df_fail <- dplyr::select(df, !Unit)

  expect_no_error(qaqc_thresholds(df))
  expect_error(
    qaqc_thresholds(df_fail),
    regexp = "\tThe following columns are missing: Unit"
  )

  # Need at least one threshold column...
  df_t2 <- dplyr::select(df, !c(Excellent, Good, Fair))
  df_t1 <- dplyr::select(df_t2, !Threshold_Max)
  df_t0 <- dplyr::select(df_t1, !Threshold_Min)

  expect_warning(
    qaqc_thresholds(df_t2),
    regexp = "Adding blank columns: Excellent, Good, Fair"
  )
  expect_error(
    suppressWarnings(qaqc_thresholds(df_t1)),
    regexp = "Rows must contain at least one threshold value. Check rows 1, 2"
  )
  expect_error(
    qaqc_thresholds(df_t0),
    regexp = "Must include at least one threshold column"
  )
})

test_that("Replaces missing columns", {
  df <- data.frame(
    State = c("MA", "MA"),
    Depth_Category = c("Surface", "Surface"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    extra_col = c("Red", "Herring")
  )

  df2 <- data.frame(
    State = c("MA", "MA"),
    Depth_Category = c("Surface", "Surface"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    Excellent = c(NA, NA),
    Good = c(NA, NA),
    Fair = c(NA, NA)
  )

  expect_equal(
    suppressWarnings(qaqc_thresholds(df)),
    df2
  )
  expect_warning(
    qaqc_thresholds(df),
    regexp = "Adding blank columns: Excellent, Good, Fair"
  )
})

test_that("Drops extra columns", {
  df <- data.frame(
    State = c("MA", "MA"),
    Depth_Category = c("Surface", "Surface"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    Excellent = c(0.3, 0.025),
    Good = c(0.6, 0.05),
    Fair = c(0.9, 0.1),
    extra_col = c("Red", "Herring")
  )

  df_test <- qaqc_thresholds(df)

  expect_false("extra_col" %in% colnames(df_test))
  expect_false("Group" %in% colnames(df_test))
})

test_that("Can't list site and group on same row", {
  df <- data.frame(
    Group = c(NA, "foo"),
    Site_ID = c("Superb", "Owl"),
    Depth_Category = c("Surface", "Surface"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    Excellent = c(0.3, 0.025),
    Good = c(0.6, 0.05),
    Fair = c(0.9, 0.1)
  )

  expect_error(
    qaqc_thresholds(df),
    regexp = "Group and site thresholds must be on seperate rows. Check rows: 2"
  )
})

test_that("Provides error message if invalid depth category", {
  df <- data.frame(
    State = c("MA", "MA"),
    Depth_Category = c("Surface", "foo"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    Excellent = c(0.3, 0.025),
    Good = c(0.6, 0.05),
    Fair = c(0.9, 0.1),
    extra_col = c("Red", "Herring")
  )

  expect_error(
    qaqc_thresholds(df),
    regexp = "Invalid Depth_Category. Acceptable values: Surface, Midwater, Near Bottom, Bottom. Check rows: 2"
  )
})

test_that("Provides error message if threshold is not numeric", {
  df <- data.frame(
    State = c("MA", "MA"),
    Depth_Category = c("Surface", "Surface"),
    Parameter = c("Nitrate", "Orthophosphate"),
    Unit = c("mg/L", "mg/L"),
    Min_Max_Mean = c("mean", "mean"),
    Threshold_Min = c(NA, NA),
    Threshold_Max = c(0.6, 0.05),
    Excellent = c("0.3", "foo"),
    Good = c(0.6, 0.05),
    Fair = c(0.9, 0.1),
    extra_col = c("Red", "Herring")
  )

  expect_error(
    qaqc_thresholds(df),
    regexp = "Non-numeric entries for Excellent found in rows: 2"
  )
})
