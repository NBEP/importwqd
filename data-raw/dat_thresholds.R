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
  format_thresholds() |>
  dplyr::mutate(dplyr::across(Min:Fair, as.character))

df_epa <- readr::read_csv(
  "inst/extdata/epa_thresholds.csv",
  show_col_types = FALSE
) |>
  qaqc_thresholds() |>
  format_thresholds() |>
  dplyr::mutate(dplyr::across(Min:Fair, as.character))

dat_thresholds <- dplyr::bind_rows(df_state, df_epa) |>
  wqformat::col_to_numeric("Min") |>
  wqformat::col_to_numeric("Max") |>
  wqformat::col_to_numeric("Excellent") |>
  wqformat::col_to_numeric("Good") |>
  wqformat::col_to_numeric("Fair")
usethis::use_data(dat_thresholds, internal = TRUE, overwrite = TRUE)
