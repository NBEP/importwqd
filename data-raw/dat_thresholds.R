# Save state, EPA thresholds
library(readr)
library(dplyr)
source("R/import_thresholds.R")
source("R/utils_import.R")

df_state <- readr::read_csv(
  "inst/extdata/state_thresholds.csv",
  show_col_types = FALSE
) |>
  qaqc_thresholds() |>
  format_thresholds()

df_epa <- readr::read_csv(
  "inst/extdata/epa_thresholds.csv",
  show_col_types = FALSE
) |>
  qaqc_thresholds() |>
  format_thresholds()

dat_thresholds <- dplyr::bind_rows(df_state, df_epa)
usethis::use_data(dat_thresholds, overwrite = TRUE)
