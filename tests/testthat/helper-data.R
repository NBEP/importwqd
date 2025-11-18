tst <- list(
  # Fake site metadata
  sites_raw = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "Massachusetts"),
    Watershed = c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Warmwater", "Coldwater"),
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
    Group = c("Warmwater", "Coldwater"),
    Red_Herring = "foo",
    Max_Surface_Depth_m = NA_integer_,
    Max_Midwater_Depth_m = NA_integer_,
    Max_Depth_m = NA_integer_
  ),
  sites_final = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    State = c("RI", "MA"),
    Watershed = c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Warmwater", "Coldwater"),
    Max_Surface = 1,
    Max_Midwater = NA_integer_,
    Max_Depth = NA_integer_,
    Town_Code = c("Providence, RI", "Worcester, MA"),
    County_Code = c("Providence", "Worcester")
  ),
  # Threshold data
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
  # Fake data
  data_raw = data.frame(
    Site_ID = c("001", "001", "002"),
    Activity_Type = NA,
    Date = as.Date("2025-11-12"),
    Depth = NA,
    Depth_Unit = "m",
    Depth_Category = NA,
    Parameter = c(
      "DO % or whatever", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)"
    ),
    Result = c(80, 200, 12),
    Result_Unit = c("%", "ng/L", "mg/L"),
    Detection_Limit_Type = NA,
    Detection_Limit = 5,
    Detection_Limit_Unit = "mg/L",
    Qualifier = NA
  )
)
