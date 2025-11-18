library(readr)
library(dplyr)

# Add unit lookup table ----
varnames_units <- readr::read_csv(
  "inst/extdata/varnames_units.csv",
  show_col_types = FALSE
)

wqd_units <- varnames_units$measurements
names(wqd_units) <- varnames_units$wqdashboard

# Add qualifier codes ----

# Save data ----
usethis::use_data(wqd_units, internal = TRUE, overwrite = TRUE)
