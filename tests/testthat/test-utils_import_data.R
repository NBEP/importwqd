test_that("find_var_names works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "genus" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, NA, 2)
  )

  # Check errors
  expect_error(
    find_var_names("foo", "numbers", "species"),
    regexp = "df must be type dataframe"
  )
  expect_error(
    find_var_names(df, "class", "species"),
    regexp = "Invalid in_format. Acceptable formats: species, genus, numbers"
  )
  expect_error(
    find_var_names(df, "species", "class"),
    regexp = "Invalid out_format. Acceptable formats: species, genus, numbers"
  )
  expect_error(
    find_var_names(df, "class", "order"),
    regexp = "Invalid in_format and out_format. Acceptable formats: species, genus, numbers"
  )

  # Check works
  var_names <- find_var_names(df, "numbers", "species")
  expect_equal(var_names$old_names, c("1", "2"))
  expect_equal(var_names$new_names, c("aardvark", "chinchilla"))
  expect_equal(var_names$keep_var, c("aardvark", "bittern", "chinchilla"))
})

test_that("rename_col works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )

  expect_error(
    rename_col(df, c("species", "class"), "foo"),
    regexp = "old_colnames and new_colnames are different lengths"
  )

  df_test <- rename_col(
    df,
    c("species", "genus", "numbers"),
    c("foo", "bar", "foofy")
  )
  expect_equal(colnames(df_test), c("foo", "class", "foofy"))
})

test_that("rename_var works", {
  expect_equal(
    rename_var("cat", c("cat", "dog"), c("kitten", "puppy")),
    "kitten"
  )
  expect_equal(rename_var(1, c(1, 2), c(11, 22)), 11)
})

test_that("rename_all_var works", {
  df <- data.frame(
    "species" = c("aardvark", "bittern", "chinchilla"),
    "class" = c("mammal", "bird", "mammal"),
    "numbers" = c(1, 2, 3)
  )
  expect_error(
    rename_all_var(df, "genus", "chinchilla", "cat"),
    regexp = "col_name not in dataframe"
  )
  expect_error(
    rename_all_var(df, "species", c("ant", "chinchilla"), "cat"),
    regexp = "old_varname and new_varname are different lengths"
  )

  df_test <- rename_all_var(
    df, "class",
    c("mammal", "reptile"),
    c("Mammalia", "Reptilia")
  )
  expect_equal(df_test$class, c("Mammalia", "bird", "Mammalia"))

  df_test <- rename_all_var(df, "numbers", c(1, 2), c(12, 23))
  expect_equal(df_test$numbers, c(12, 23, 3))
})


test_that("check_column_missing works", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c(1, 2, 3)
  )

  expect_no_error(
    check_column_missing(df, c("Col1", "Col2"))
  )

  expect_error(
    df %>% check_column_missing(c("Col1", "Col3")),
    regexp = "The following columns are missing: Col3"
  )
})

test_that("skip_rows works", {
  df <- data.frame(
    Activity_Type = c(
      "Quality Control Sample-Field Blank", "Field Msr/Obs",
      "Field Msr/Obs", "Field Msr/Obs"
    ),
    Qualifier = c(NA, "Q", NA, "DL")
  )

  expect_equal(skip_rows(df), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("set_nondetect_values works", {
  df <- data.frame(
    Parameter = c(
      "Nitrate + Nitrite", "Nitrate + Nitrite", "Nitrate + Nitrite",
      "Nitrate + Nitrite"
    ),
    Result = c(3, "BDL", "BDL", 3),
    Result_Unit = c("mg/L", "ug/L", "ug/L", "ug/L"),
    Qualifier = c(NA, "Q", NA, "DL")
  )
  df_out <- data.frame(
    Parameter = c(
      "Nitrate + Nitrite", "Nitrate + Nitrite", "Nitrate + Nitrite",
      "Nitrate + Nitrite"
    ),
    Result = c(3, "BDL", 0, 0),
    Result_Unit = c("mg/L", "ug/L", "ug/L", "ug/L"),
    Qualifier = c(NA, "Q", "DL", "DL")
  )

  expect_equal(set_nondetect_values(df), df_out)

  df2 <- data.frame(
    Parameter = c(
      "Nitrate + Nitrite", "Nitrate + Nitrite", "Nitrate + Nitrite",
      "Nitrate + Nitrite"
    ),
    Result = c(3, "BDL", "BDL", 3),
    Result_Unit = c("mg/L", "ug/L", "ug/L", "ug/L"),
    Qualifier = c(NA, "Q", NA, "DL"),
    Detection_Limit = c(1, 1, 1, 1),
    Detection_Limit_Unit = c("mg/L", "mg/L", "mg/L", "mg/L")
  )
  df2_out <- data.frame(
    Parameter = c(
      "Nitrate + Nitrite", "Nitrate + Nitrite", "Nitrate + Nitrite",
      "Nitrate + Nitrite"
    ),
    Result = c(3, "BDL", 0.5, 0.5),
    Result_Unit = c("mg/L", "ug/L", "mg/L", "mg/L"),
    Qualifier = c(NA, "Q", "DL", "DL"),
    Detection_Limit = c(1, 1, 1, 1),
    Detection_Limit_Unit = c("mg/L", "mg/L", "mg/L", "mg/L")
  )

  expect_equal(set_nondetect_values(df2), df2_out)
})

test_that("check_val_missing produces error or warning if NA value in column", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c("D", NA, "F"),
    Col3 = c(
      as.Date("2024/01/01"), as.Date("2024/01/02"),
      as.Date("2024/01/03")
    )
  )

  expect_error(
    check_val_missing(df, "Col2"),
    regexp = "Col2 missing in rows 2"
  )
  expect_warning(
    check_val_missing(df, "Col2", is_stop = FALSE),
    regexp = "Col2 missing in rows 2"
  )
  expect_no_error(check_val_missing(df, "Col1"))
  expect_no_error(check_val_missing(df, "Col3"))
})

test_that("check_val_duplicate produces error or warning if duplicate values in
          column", {
  df <- data.frame(
    Col1 = c("A", "B", "C"),
    Col2 = c("D", "E", "E"),
    Col3 = c("G", "G", "G")
  )

  expect_error(
    check_val_duplicate(df, "Col2"),
    regexp = "Duplicate Col2 in rows 2, 3"
  )
  expect_warning(
    check_val_duplicate(df, "Col2", is_stop = FALSE),
    regexp = "Duplicate Col2 in rows 2, 3"
  )
  expect_error(
    check_val_duplicate(df, c("Col2", "Col3")),
    regexp = "Duplicate Col2, Col3 in rows 2, 3"
  )
  expect_no_error(check_val_duplicate(df, "Col1"))
  expect_no_error(check_val_duplicate(df, c("Col1", "Col2")))
})

test_that("check_val_count drops columns if less than two unique values", {
  df <- data.frame(
    Col1 = c("A", "A", "A"),
    Col2 = c(NA, NA, NA),
    Col3 = c("A", "B", "C"),
    Col4 = c("A", NA, "A")
  )

  # Check - drops column with only 1 variable
  expect_message(
    check_val_count(df, "Col1"),
    regexp = "Removed column Col1: Less than 2 unique values"
  )
  expect_equal(
    suppressMessages(check_val_count(df, "Col1")),
    df[c("Col2", "Col3", "Col4")]
  )

  # Check - can check multiple columns at once
  # Check - drops columns that are only NA values
  expect_message(
    check_val_count(df, c("Col1", "Col2", "Col3", "Col4")),
    regexp = "Removed column Col1, Col2: Less than 2 unique values"
  )
  expect_equal(
    suppressMessages(
      check_val_count(df, c("Col1", "Col2", "Col3", "Col4"))
    ),
    df[c("Col3", "Col4")]
  )

  # Check - ignores columns that don't exist
  expect_equal(
    suppressMessages(check_val_count(df, c("Col3", "Col5"))),
    df
  )
})

test_that("check_val_numeric produces error if non-numeric value in column", {
  df <- data.frame(
    Col1 = c("A", 2, "C"),
    Col2 = c(1, 2, 3)
  )

  expect_no_error(check_val_numeric(df, "Col2"))
  expect_error(
    check_val_numeric(df, "Col1"),
    regexp = "Non-numeric entries for Col1 found in rows: 1, 3"
  )
})

test_that("format_date works", {
  df <- data.frame(
    "good_date" = c(
      lubridate::ymd("20220301"),
      lubridate::ymd("20230418"),
      lubridate::ymd("20040612"),
      lubridate::ymd("20240926")
    ),
    "na_date" = c(
      lubridate::ymd("20220301"),
      NA,
      NA,
      lubridate::ymd("20240926")
    ),
    "mdy_date" = c("3/1/22", NA, "4/18/23", NA),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA)
  )

  df_format <- data.frame(
    "good_date" = c(
      lubridate::ymd("20220301"),
      lubridate::ymd("20230418"),
      lubridate::ymd("20040612"),
      lubridate::ymd("20240926")
    ),
    "na_date" = c(
      lubridate::ymd("20220301"),
      NA,
      NA,
      lubridate::ymd("20240926")
    ),
    "mdy_date" = c(
      lubridate::ymd("20220301"),
      NA,
      lubridate::ymd("20230418"),
      NA
    ),
    "bad_date" = c("3/1/2022", "2023/6/4", "18/4/2023", NA)
  )

  # Check error messages
  expect_error(
    format_date(df, "mdy_date", "foobar"),
    regexp = "date_format contains invalid variables: foo"
  )
  expect_error(
    format_date(df, "mdy_date", "m/d/Y"),
    regexp = 'Date does not match format "m/d/Y"'
  )
  expect_error(
    format_date(df, "bad_date", "m/d/Y"),
    regexp = "Date is improperly formatted in rows: 2, 3"
  )

  # Check works
  expect_equal(format_date(df, "good_date"), df)
  expect_equal(format_date(df, "na_date"), df)
  expect_equal(format_date(df, "mdy_date", "m/d/y"), df_format)
})

test_that("convert_unit accurately converts units", {
  # Check conversions
  expect_equal(convert_unit(0, "deg C", "deg F"), 32) # simple conversion
  expect_equal(convert_unit(1, "mg/L", "ug/L"), 1000) # complex conversion

  # Check errors
  expect_error(
    convert_unit(0, "deg C", "mg"),
    regexp = "Unable to convert C to mg"
  )
  expect_no_error(convert_unit(0, "foo", "bar", is_stop = FALSE))
})

test_that("standardize_units works", {
  df <- data.frame(
    Parameter = c("Temperature, Air", "Temperature, Air", "Temperature, Water"),
    Result = c(0, 36, 40),
    Result_Unit = c("deg C", "deg F", "deg F")
  )
  df_out <- data.frame(
    Parameter = c("Temperature, Air", "Temperature, Air", "Temperature, Water"),
    Result = c(32, 36, 40),
    Result_Unit = c("deg F", "deg F", "deg F")
  )

  expect_equal(standardize_units(df), df_out)
})

test_that("depth_to_m converts depth to meters", {
  df <- data.frame(
    Site_ID = c("001", "002", "003"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, 150, 10),
    Depth_Unit = c("m", "cm", "foo")
  )

  df_out <- data.frame(
    Site_ID = c("001", "002", "003"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, 1.5, 10),
    Depth_Unit = c("m", "m", "foo")
  )

  # Test works
  expect_equal(suppressMessages(depth_to_m(df)), df_out)

  # Test errors
  df_foo <- data.frame(
    Site_ID = c("001", "002", "003"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Nitrate"),
    Depth = c(1, 150, 10),
    Depth_Unit = c("m", "cm", "foo")
  )
  expect_error(depth_to_m(df_foo))

  df_depth <- data.frame(
    Site_ID = c("001", "002", "003"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, NA, 10),
    Depth_Unit = c("m", "cm", "foo")
  )
  expect_warning(
    suppressMessages(depth_to_m(df_depth)),
    regexp = "Depth is missing in rows 2"
  )

  df_unit <- data.frame(
    Site_ID = c("001", "002", "003"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, 150, 10),
    Depth_Unit = c("m", NA, "foo")
  )
  expect_warning(
    suppressMessages(depth_to_m(df_unit)),
    regexp = "Depth_Unit is missing in rows 2"
  )
})

test_that("assign_depth_category assigns depth category", {
  df <- data.frame(
    Site_ID = c("001", "002", "002"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, 1.5, 10),
    Depth_Unit = c("m", "m", "foo")
  )

  df_error <- data.frame(
    Site_ID = c("001", "002", "002"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, 1.5, 10),
    Depth_Unit = c("m", "m", "foo"),
    Depth_Category = c("Surface", "Midwater", "Mars")
  )

  df_s <- data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    State = c("Rhode Island", "MA")
  )

  df_out <- data.frame(
    Site_ID = c("001", "002", "002"),
    Parameter = c("Temperature, Air", "Temperature, Water", "Depth"),
    Depth = c(1, 1.5, 10),
    Depth_Unit = c("m", "m", "foo"),
    Depth_Category = c("Surface", "Midwater", NA)
  )

  # check works
  expect_equal(
    suppressMessages(assign_depth_category(df, sites = df_s)),
    df_out
  )

  # Check catches wrong depth_category
  expect_equal(
    suppressWarnings(assign_depth_category(df_error, sites = df_s)),
    df_out
  )
  expect_warning(
    assign_depth_category(df_error, sites = df_s),
    regexp = "Removed invalid Depth_Category in rows 3"
  )
})
