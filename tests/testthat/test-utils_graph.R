test_that("prep_graph_table works", {
  # Test - group by Parameter
  df_out <- data.frame(
    Date = c(
      "2021-06-30", "2023-07-12", "2021-08-05", "2023-05-25"
    ),
    "Dissolved oxygen (DO) mg/L" = c(0.05, 3, 0.05, 4, 6, 8, 7, 9),
    check.names = FALSE
  )
  df_out$Date <- as.Date(df_out$Date)

  expect_equal(
    prep_graph_table(tst$data_final, "Parameter"),
    df_out
  )

  # Test - group by Site_Name
  df_out <- data.frame(
    Date = c(
      "2021-06-30", "2023-07-12", "2021-08-05", "2023-05-25"
    ),
    Site1 = c(0.05, 3, 0.05, 4),
    Site2 = c(6, 8, 7, 9)
  )
  df_out$Date <- as.Date(df_out$Date)

  expect_equal(
    prep_graph_table(tst$data_final, "Site_Name"),
    df_out
  )

  # Test - group by Depth
  df_in <- tst$data_final
  df_in$Depth <- "Surface"

  df_out <- data.frame(
    Date = c(
      "2021-06-30", "2023-07-12", "2021-08-05", "2023-05-25"
    ),
    Surface = c(0.05, 3, 0.05, 4, 6, 8, 7, 9)
  )
  df_out$Date <- as.Date(df_out$Date)

  expect_equal(
    prep_graph_table(df_in, "Depth"),
    df_out
  )
})
