test_that("prep_graph_table works", {
  # Test - group by Parameter
  df_in <- dplyr::filter(tst$data_final, .data$Site_ID == "001")

  df_out <- data.frame(
    Date = as.Date(
      c("2021-06-30", "2021-08-05", "2023-05-25", "2023-07-12")
    ),
    "Dissolved oxygen (DO) mg/L" = c(0.05, 3, 0.05, 4),
    "Depth, Secchi disk depth m" = c(NA, NA, NA, 8),
    check.names = FALSE
  )

  expect_equal(
    prep_graph_table(df_in, "Parameter"),
    df_out
  )

  # Test - group by Site_Name
  df_in <- tst$data_final |>
    dplyr::filter(.data$Parameter == "Dissolved oxygen (DO)")

  df_out <- data.frame(
    Date = as.Date(
      c("2021-06-30", "2021-08-05", "2023-05-25", "2023-07-12")
    ),
    Site1 = c(0.05, 3, 0.05, 4),
    Site2 = c(6, 8, 7, 9)
  )

  expect_equal(
    prep_graph_table(df_in, "Site_Name"),
    df_out
  )

  # Test - group by Depth
  df_in <- tst$data_final |>
    dplyr::filter(
      .data$Site_ID == "001",
      .data$Parameter == "Dissolved oxygen (DO)"
    )

  df_out <- data.frame(
    Date = as.Date(
      c("2021-06-30", "2021-08-05", "2023-05-25", "2023-07-12")
    ),
    Surface = c(0.05, 3, 0.05, NA),
    Midwater = c(NA, NA, NA, 4)
  )

  expect_equal(
    prep_graph_table(df_in, "Depth"),
    df_out
  )

  # Test edge case - only 1 var
  df_in <- tst$data_final |>
    dplyr::filter(
      .data$Site_ID == "001",
      .data$Parameter == "Dissolved oxygen (DO)",
      .data$Depth == "Surface"
    )

  df_out <- data.frame(
    Date = as.Date(c("2021-06-30", "2021-08-05", "2023-05-25")),
    Surface = c(0.05, 3, 0.05)
  )

  expect_equal(
    prep_graph_table(df_in, "Depth"),
    df_out
  )
})
