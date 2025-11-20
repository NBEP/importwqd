tst <- list(
  # Fake site metadata ----
  sites_raw = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "Massachusetts"),
    Watershed = c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Coldwater", "Warmwater"),
    Max_Depth_m = c(10, 12),
    Red_Herring = "foo"
  ),
  sites_qaqc = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Coldwater", "Warmwater"),
    Max_Depth_m = c(10, 12),
    Red_Herring = "foo",
    Max_Surface_Depth_m = NA_integer_,
    Max_Midwater_Depth_m = NA_integer_
  ),
  sites_final = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Coldwater", "Warmwater"),
    Max_Surface = 1,
    Max_Midwater = c(9, 11),
    Max_Depth = c(10, 12),
    Town_Code = c("Providence, RI", "Worcester, MA")
  ),
  # Threshold data ----
  threshold_raw = data.frame(
    State = c("RI", "RI", "RI", "MA", NA),
    Group = c("Coldwater", "Saltwater", "Warmwater", "Saltwater", NA),
    Depth_Category = c(NA, "Surface", NA, NA, NA),
    Parameter = c(
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "pH", "Nitrates"
    ),
    Unit = c("mg/L", "mg/L", "mg/L", "None", "mg/L"),
    Calculation = c("minimum", "minimum", "minimum", "mean", "max"),
    Threshold_Min = c(5, 4.8, 5, 6.5, NA),
    Threshold_Max = c(NA, NA, NA, 8.5, 10),
    Excellent = c(8, NA, NA, NA, NA),
    Good = c(6.5, NA, NA, NA, NA),
    Fair = c(5, NA, NA, NA, NA),
    Red_Herring = "foo"
  ),
  threshold_qaqc = data.frame(
    State = c("RI", "RI", "RI", "MA", NA),
    Group = c("Coldwater", "Saltwater", "Warmwater", "Saltwater", NA),
    Site_ID = NA,
    Depth_Category = c(NA, "Surface", NA, NA, NA),
    Parameter = c(
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "pH", "Nitrates"
    ),
    Unit = c("mg/L", "mg/L", "mg/L", "None", "mg/L"),
    Calculation = c("minimum", "minimum", "minimum", "mean", "max"),
    Threshold_Min = c(5, 4.8, 5, 6.5, NA),
    Threshold_Max = c(NA, NA, NA, 8.5, 10),
    Excellent = c(8, NA, NA, NA, NA),
    Good = c(6.5, NA, NA, NA, NA),
    Fair = c(5, NA, NA, NA, NA)
  ),
  threshold_final = data.frame(
    State = c("MA", "RI", "RI", "RI", NA),
    Group = c("Saltwater", "Coldwater", "Saltwater", "Warmwater", NA),
    Site = NA,
    Depth = c(NA, NA, "Surface", NA, NA),
    Parameter = c(
      "pH", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "Dissolved oxygen (DO)", "Nitrates"
    ),
    Unit = c("None", "mg/L", "mg/L", "mg/L", "mg/L"),
    Calculation = c("mean", "min", "min", "min", "max"),
    Min = c(6.5, 5, 4.8, 5, NA),
    Max = c(8.5, NA, NA, NA, 10),
    Excellent = c(NA, 8, NA, NA, NA),
    Good = c(NA, 6.5, NA, NA, NA),
    Fair = c(NA, 5, NA, NA, NA),
    Best = c(NA, "high", NA, NA, NA)
  ),
  # Fake data ----
  data_raw = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "002", "002", "002", "002"
    ),
    Activity_Type = "Field Msr/Obs",
    Date = c(
      as.Date("2021-06-30"), as.Date("2023-07-12"), as.Date("2021-08-05"),
      as.Date("2023-05-25")
    ),
    Depth = 0.5,
    Depth_Unit = "m",
    Depth_Category = NA,
    Parameter = "Dissolved oxygen (DO)",
    Result = c(NA, 3000, NA, 4000, 6000, 8, 7, 9),
    Result_Unit = c(NA, "ug/L", NA, "ug/L", "ug/L", "mg/L", "mg/L", "mg/L"),
    Lower_Detection_Limit = c(100, 100, 100, 100, 0.1, 0.1, 0.1, 0.1),
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = c(
      "ug/L", "ug/L", "ug/L", "ug/L", "mg/L", "mg/L", "mg/L", "mg/L"
    ),
    Qualifier = c("DL", NA, "DL", NA, NA, NA, NA, NA)
  ),
  data_qaqc = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "002", "002", "002", "002"
    ),
    Activity_Type = "Field Msr/Obs",
    Date = c(
      as.Date("2021-06-30"), as.Date("2023-07-12"), as.Date("2021-08-05"),
      as.Date("2023-05-25")
    ),
    Depth = 0.5,
    Depth_Unit = "m",
    Depth_Category = "Surface",
    Parameter = "Dissolved oxygen (DO)",
    Result = c(NA, 3, NA, 4, 6, 8, 7, 9),
    Result_Unit = "mg/L",
    Lower_Detection_Limit = 0.1,
    Upper_Detection_Limit = NA_integer_,
    Detection_Limit_Unit = "mg/L",
    Qualifier = c("DL", NA, "DL", NA, NA, NA, NA, NA),
    Year = c(2021, 2023)
  ),
  data_final = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "002", "002", "002", "002"
    ),
    Date = c(
      as.Date("2021-06-30"), as.Date("2023-07-12"), as.Date("2021-08-05"),
      as.Date("2023-05-25")
    ),
    Year = c(2021, 2023),
    Parameter = "Dissolved oxygen (DO)",
    Result = c(0.05, 3, 0.05, 4, 6, 8, 7, 9),
    Unit = "mg/L",
    Depth = "Surface",
    Calculation = c("min", "min", "min", "min", "mean", "mean", "mean", "mean"),
    Min = c(5, 5, 5, 5, NA, NA, NA, NA),
    Max = NA_integer_,
    Excellent = c(8, 8, 8, 8, NA, NA, NA, NA),
    Good = c(6.5, 6.5, 6.5, 6.5, NA, NA, NA, NA),
    Fair = c(5, 5, 5, 5, NA, NA, NA, NA),
    Best = c("high", "high", "high", "high", NA, NA, NA, NA),
    Month = c("June", "July", "August", "May")
  )
)
