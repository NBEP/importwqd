tst <- list(
  # Sites ----
  sites_raw = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    State = c("Rhode Island", "Massachusetts"),
    Watershed = c("Narragansett Bay", "Upper Blackstone River"),
    Group = c("Coldwater", "Warmwater"),
    Max_Depth_m = c(10, 12),
    Red_Herring = "foo",
    Blank_Herring = NA
  ),
  sites_qaqc = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed = c("Narragansett Bay", "Upper Blackstone River"),
    Group = c("Coldwater", "Warmwater"),
    Max_Depth_m = c(10, 12),
    Red_Herring = "foo",
    Max_Surface_Depth_m = 1,
    Max_Midwater_Depth_m = c(9, 11)
  ),
  sites_final = data.frame(
    Site_ID = c("001", "002"),
    Site_Name = c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence, RI", "Worcester, MA"),
    State = c("Rhode Island", "Massachusetts"),
    Watershed = c("Narragansett Bay", "Upper Blackstone River"),
    Group = c("Coldwater", "Warmwater")
  ),
  # Thresholds ----
  threshold_raw = data.frame(
    State = c("RI", "RI", "RI", "MA", NA),
    Group = c("Coldwater", "Saltwater", "Warmwater", "Saltwater", NA),
    Depth_Category = c(NA, "Surface", NA, NA, NA),
    Parameter = c(
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "pH", "Nitrate"
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
      "pH", "Nitrate"
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
      "Dissolved oxygen (DO)", "Nitrate"
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
  # Data ----
  data_raw = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "001", "002", "002", "002", "002", "002"
    ),
    Activity_Type = "Field Msr/Obs",
    Date = as.Date(
      c("2021-06-30", "2021-08-05", "2023-05-25", "2023-07-12", "2023-07-12")
    ),
    Depth = c(0.5, 0.5, 0.5, 8, NA),
    Depth_Unit = "m",
    Depth_Category = NA,
    Parameter = c(
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "Dissolved oxygen (DO)", "Depth, Secchi disk depth"
    ),
    Result = c(NA, 3000, NA, 4000, 8, 6000, 8, 7, 9, 9.2),
    Result_Unit = c(
      NA, "ug/L", NA, "ug/L", "m", "ug/L", "mg/L", "mg/L", "mg/L", "m"
    ),
    Lower_Detection_Limit = c(100, 100, 100, 100, NA, 0.1, 0.1, 0.1, 0.1, NA),
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = c(
      "ug/L", "ug/L", "ug/L", "ug/L", NA, "mg/L", "mg/L", "mg/L", "mg/L", NA
    ),
    Qualifier = c("DL", NA, "DL", NA, NA, NA, NA, NA, NA, NA)
  ),
  data_qaqc = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "001", "002", "002", "002", "002", "002"
    ),
    Activity_Type = "Field Msr/Obs",
    Date = as.Date(
      c("2021-06-30", "2021-08-05", "2023-05-25", "2023-07-12", "2023-07-12")
    ),
    Depth = c(0.5, 0.5, 0.5, 8, NA),
    Depth_Unit = "m",
    Depth_Category = c("Surface", "Surface", "Surface", "Midwater", NA),
    Parameter = c(
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "Dissolved oxygen (DO)", "Depth, Secchi disk depth"
    ),
    Result = c(NA, 3, NA, 4, 8, 6, 8, 7, 9, 9.2),
    Result_Unit = c("mg/L", "mg/L", "mg/L", "mg/L", "m"),
    Lower_Detection_Limit = c(0.1, 0.1, 0.1, 0.1, NA),
    Upper_Detection_Limit = NA_integer_,
    Detection_Limit_Unit = c("mg/L", "mg/L", "mg/L", "mg/L", NA),
    Qualifier = c("DL", NA, "DL", NA, NA, NA, NA, NA, NA, NA),
    Year = c(2021, 2021, 2023, 2023, 2023)
  ),
  data_final = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "001", "002", "002", "002", "002", "002"
    ),
    Site_Name = c(
      "Site1", "Site1", "Site1", "Site1", "Site1", "Site2", "Site2", "Site2",
      "Site2", "Site2"
    ),
    Date = as.Date(
      c("2021-06-30", "2021-08-05", "2023-05-25", "2023-07-12", "2023-07-12")
    ),
    Year = c(2021, 2021, 2023, 2023, 2023),
    Parameter = c(
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "Dissolved oxygen (DO)", "Depth, Secchi disk depth"
    ),
    Result = c(0.05, 3, 0.05, 4, 8, 6, 8, 7, 9, 9.2),
    Unit = c("mg/L", "mg/L", "mg/L", "mg/L", "m"),
    Depth = c("Surface", "Surface", "Surface", "Midwater", NA),
    Calculation = c(
      "min", "min", "min", "min", "mean", "mean", "mean", "mean", "mean", "mean"
    ),
    Min = c(5, 5, 5, 5, NA, NA, NA, NA, NA, NA),
    Max = NA_integer_,
    Excellent = c(8, 8, 8, 8, NA, NA, NA, NA, NA, NA),
    Good = c(6.5, 6.5, 6.5, 6.5, NA, NA, NA, NA, NA, NA),
    Fair = c(5, 5, 5, 5, NA, NA, NA, NA, NA, NA),
    Best = c("high", "high", "high", "high", NA, NA, NA, NA, NA, NA),
    Month = c("June", "August", "May", "July", "July"),
    Description = c(
      "<b>Site1</b><br>Date: June 30, 2021<br>Depth: Surface<br>Dissolved oxygen (DO): 0.05 mg/L",
      "<b>Site1</b><br>Date: August 05, 2021<br>Depth: Surface<br>Dissolved oxygen (DO): 3 mg/L",
      "<b>Site1</b><br>Date: May 25, 2023<br>Depth: Surface<br>Dissolved oxygen (DO): 0.05 mg/L",
      "<b>Site1</b><br>Date: July 12, 2023<br>Depth: Midwater<br>Dissolved oxygen (DO): 4 mg/L",
      "<b>Site1</b><br>Date: July 12, 2023<br>Depth, Secchi disk depth: 8 m",
      "<b>Site2</b><br>Date: June 30, 2021<br>Depth: Surface<br>Dissolved oxygen (DO): 6 mg/L",
      "<b>Site2</b><br>Date: August 05, 2021<br>Depth: Surface<br>Dissolved oxygen (DO): 8 mg/L",
      "<b>Site2</b><br>Date: May 25, 2023<br>Depth: Surface<br>Dissolved oxygen (DO): 7 mg/L",
      "<b>Site2</b><br>Date: July 12, 2023<br>Depth: Midwater<br>Dissolved oxygen (DO): 9 mg/L",
      "<b>Site2</b><br>Date: July 12, 2023<br>Depth, Secchi disk depth: 9.2 m"
    )
  ),
  data_score = data.frame(
    Year = c(2023, 2021, 2021, 2023, 2023, 2021),
    Site_Name = c(
      "Site1", "Site1", "Site1", "Site1", "Site1", "Site1", "Site2", "Site2",
      "Site2", "Site2", "Site2", "Site2"
    ),
    Site_ID = c(
      "001", "001", "001", "001", "001", "001", "002", "002", "002", "002",
      "002", "002"
    ),
    Town = c(
      "Providence, RI", "Providence, RI", "Providence, RI", "Providence, RI",
      "Providence, RI", "Providence, RI", "Worcester, MA", "Worcester, MA",
      "Worcester, MA", "Worcester, MA", "Worcester, MA", "Worcester, MA"
    ),
    Watershed = c(
      "Narragansett Bay", "Narragansett Bay", "Narragansett Bay",
      "Narragansett Bay", "Narragansett Bay", "Narragansett Bay",
      "Upper Blackstone River", "Upper Blackstone River",
      "Upper Blackstone River", "Upper Blackstone River",
      "Upper Blackstone River", "Upper Blackstone River"
    ),
    Group = c(
      "Coldwater", "Coldwater", "Coldwater", "Coldwater", "Coldwater",
      "Coldwater", "Warmwater", "Warmwater", "Warmwater", "Warmwater",
      "Warmwater", "Warmwater"
    ),
    Depth = c(NA, NA, "Surface", "Midwater", "Surface", "Midwater"),
    Parameter = c(
      "Depth, Secchi disk depth", "Depth, Secchi disk depth",
      "Dissolved oxygen (DO)", "Dissolved oxygen (DO)", "Dissolved oxygen (DO)",
      "Dissolved oxygen (DO)"
    ),
    Unit = c("m", NA, "mg/L", "mg/L", "mg/L", NA),
    score_typ = c(
      "Average", NA, "Minimum", "Minimum", "Minimum", NA, "Average", NA,
      "Average", "Average", "Average", NA
    ),
    score_num = c(8, NA, 0.05, 4, 0.05, NA, 9.2, NA, 7, 9, 7, NA),
    score_str = c(
      "No Threshold Established", "No Data Available", "Poor", "Poor", "Poor",
      "No Data Available", "No Threshold Established", "No Data Available",
      "No Threshold Established", "No Threshold Established",
      "No Threshold Established", "No Data Available"
    ),
    Latitude = c(
      41.83, 41.83, 41.83, 41.83, 41.83, 41.83, 42.28, 42.28, 42.28, 42.28,
      42.28, 42.28
    ),
    Longitude = c(
      -71.41, -71.41, -71.41, -71.41, -71.41, -71.41, -71.77, -71.77, -71.77,
      -71.77, -71.77, -71.77
    ),
    popup_loc = c(
      "<b>Site1</b> <br>Town: Providence, RI <br>Watershed: Narragansett Bay <br>Category: Coldwater",
      "<b>Site1</b> <br>Town: Providence, RI <br>Watershed: Narragansett Bay <br>Category: Coldwater",
      "<b>Site1</b> <br>Town: Providence, RI <br>Watershed: Narragansett Bay <br>Category: Coldwater <br>Depth: Surface",
      "<b>Site1</b> <br>Town: Providence, RI <br>Watershed: Narragansett Bay <br>Category: Coldwater <br>Depth: Midwater",
      "<b>Site1</b> <br>Town: Providence, RI <br>Watershed: Narragansett Bay <br>Category: Coldwater <br>Depth: Surface",
      "<b>Site1</b> <br>Town: Providence, RI <br>Watershed: Narragansett Bay <br>Category: Coldwater <br>Depth: Midwater",
      "<b>Site2</b> <br>Town: Worcester, MA <br>Watershed: Upper Blackstone River <br>Category: Warmwater",
      "<b>Site2</b> <br>Town: Worcester, MA <br>Watershed: Upper Blackstone River <br>Category: Warmwater",
      "<b>Site2</b> <br>Town: Worcester, MA <br>Watershed: Upper Blackstone River <br>Category: Warmwater <br>Depth: Surface",
      "<b>Site2</b> <br>Town: Worcester, MA <br>Watershed: Upper Blackstone River <br>Category: Warmwater <br>Depth: Midwater",
      "<b>Site2</b> <br>Town: Worcester, MA <br>Watershed: Upper Blackstone River <br>Category: Warmwater <br>Depth: Surface",
      "<b>Site2</b> <br>Town: Worcester, MA <br>Watershed: Upper Blackstone River <br>Category: Warmwater <br>Depth: Midwater"
    ),
    popup_score = c(
      "<br>Average: 8 m",
      "<br><i>No data</i>",
      "<br>Minimum: 0.05 mg/L<br>Score: Poor",
      "<br>Minimum: 4 mg/L<br>Score: Poor",
      "<br>Minimum: 0.05 mg/L<br>Score: Poor",
      "<br><i>No data</i>",
      "<br>Average: 9.2 m",
      "<br><i>No data</i>",
      "<br>Average: 7 mg/L",
      "<br>Average: 9 mg/L",
      "<br>Average: 7 mg/L",
      "<br><i>No data</i>"
    ),
    alt = c(
      "Site1, 8 m", "Site1, No data", "Site1, Poor", "Site1, Poor",
      "Site1, Poor", "Site1, No data", "Site2, 9.2 m", "Site2, No data",
      "Site2, 7 mg/L", "Site2, 9 mg/L", "Site2, 7 mg/L", "Site2, No data"
    )
  ),
  # Categorical data ----
  cat_raw = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "002", "002", "002", "002"
    ),
    Activity_Type = "Field Msr/Obs",
    Date = c(
      as.Date("2021-06-30"), as.Date("2023-07-12"), as.Date("2021-08-05"),
      as.Date("2023-05-25")
    ),
    Depth = NA,
    Depth_Unit = NA,
    Depth_Category = NA,
    Parameter = "Wind force, Beaufort scale",
    Result = c(NA, 3, NA, 4, 2, 6, 7, 5),
    Result_Unit = NA,
    Lower_Detection_Limit = NA,
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = NA,
    Qualifier = c("Q", NA, "Q", NA, NA, NA, NA, NA)
  ),
  cat_qaqc = data.frame(
    Site_ID = c(
      "001", "001", "001", "001", "002", "002", "002", "002"
    ),
    Activity_Type = "Field Msr/Obs",
    Date = c(
      as.Date("2021-06-30"), as.Date("2023-07-12"), as.Date("2021-08-05"),
      as.Date("2023-05-25")
    ),
    Depth = NA_integer_,
    Depth_Unit = "m",
    Depth_Category = NA,
    Parameter = "Wind force, Beaufort scale",
    Result = c(NA, 3, NA, 4, 2, 6, 7, 5),
    Result_Unit = NA,
    Lower_Detection_Limit = NA,
    Upper_Detection_Limit = NA,
    Detection_Limit_Unit = NA,
    Qualifier = c("Q", NA, "Q", NA, NA, NA, NA, NA),
    Year = c(2021, 2023)
  ),
  # Misc ----
  s_var = list(
    state = c("Rhode Island", "Massachusetts"),
    town = c("Providence, RI", "Worcester, MA"),
    watershed = c("Narragansett Bay", "Upper Blackstone River"),
    site_id = c("001", "002"),
    site_name = c("Site1", "Site2"),
    loc_choices = c("By Town" = "town", "By Watershed" = "watershed"),
    loc_tab = "toggle",
    param = c("Dissolved oxygen (DO)", "Depth, Secchi disk depth"),
    param_score = "Dissolved oxygen (DO)",
    param_cat = "Wind force, Beaufort scale",
    depth = c("Surface", "Midwater"),
    year = c(2021, 2023),
    month = c("May", "June", "July", "August")
  )
)
