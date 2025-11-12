tst <- list(
  # Fake site metadata
  sites_raw = data.frame(
    Site_ID	= c("001", "002"),
    Site_Name	= c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("Rhode Island", "Massachusetts"),
    Watershed	= c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Warmwater", "Coldwater"),
    Red_Herring = "foo"
  ),
  sites_qaqc = data.frame(
    Site_ID	= c("001", "002"),
    Site_Name	= c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    Town = c("Providence", "Worcester"),
    County = c("Providence", "Worcester"),
    State = c("RI", "MA"),
    Watershed	= c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Warmwater", "Coldwater"),
    Red_Herring = "foo",
    Max_Surface_Depth_m = NA_integer_,
    Max_Midwater_Depth_m = NA_integer_,
    Max_Depth_m = NA_integer_
  ),
  sites_final = data.frame(
    Site_ID	= c("001", "002"),
    Site_Name	= c("Site1", "Site2"),
    Latitude = c(41.83, 42.28),
    Longitude = c(-71.41, -71.77),
    State = c("RI", "MA"),
    Watershed	= c("Narragnasett Bay", "Upper Blackstone River"),
    Group = c("Warmwater", "Coldwater"),
    Max_Surface = 1,
    Max_Midwater = NA_integer_,
    Max_Depth = NA_integer_,
    Town_Code = c("Providence, RI", "Worcester, MA"),
    County_Code = c("Providence", "Worcester")
  )
  # Threshold data
  # Fake data
)
