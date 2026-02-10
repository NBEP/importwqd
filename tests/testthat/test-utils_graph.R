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

test_that("prep_scatter_lines works", {
  df_in <- data.frame(
    Site_ID = "001",
    Site_Name = "Site1",
    Date = as.Date(c("2021-06-30", "2021-06-30", "2023-07-12")),
    Year = c(2021, 2021, 2023),
    Parameter = "Dissolved oxygen (DO)",
    Unit = "mg/L",
    Depth = "Surface",
    Result = c(2, 3, 5)
  )

  df_out <- data.frame(
    Site_ID = c(NA, "001", "001", NA, "001"),
    Site_Name = "Site1",
    Date = as.Date(
      c("2021-01-01", "2021-06-30", "2021-06-30", "2023-01-01", "2023-07-12")
    ),
    Parameter = "Dissolved oxygen (DO)",
    Unit = c(NA, "mg/L", "mg/L", NA, "mg/L"),
    Depth = "Surface",
    Result = c(NA, 2, 3, NA, 5)
  )

  expect_equal(
    prep_scatter_lines(df_in),
    df_out
  )

  # Edge case - no depth
  df_in$Depth <- NULL
  df_out$Depth <- NULL

  expect_equal(
    prep_scatter_lines(df_in),
    df_out
  )
})

test_that("graph_style works", {
  fig_in <- plotly::plot_ly()

  expect_snapshot(
    suppressMessages(
      graph_style(fig_in, "title", "y-axis label", c(0, 1))
    )
  )
})

test_that("plot_thresholds works", {
  df_in <- tst$data_final[1:3, ]
  date_range <- as.Date(c("2021-06-30", "2023-05-25"))
  y_range <- c(0, 10)

  thresh <- list(
    thresh_min = 5,
    thresh_max = NA,
    thresh_exc = 8,
    thresh_best = "high",
    unit = "mg/L"
  )

  # Test - thresh_min, thresh_best is "high"
  expect_snapshot(
    plot_thresholds(df_in, thresh, date_range, y_range)
  )

  # Test - thresh_max
  thresh <- list(
    thresh_min = NA,
    thresh_max = 8,
    thresh_exc = NA,
    thresh_best = NA,
    unit = "mg/L"
  )
  expect_snapshot(
    plot_thresholds(df_in, thresh, date_range, y_range)
  )

  # Test - thresh_best is "low"
  thresh <- list(
    thresh_min = NA,
    thresh_max = NA,
    thresh_exc = 4,
    thresh_best = "low",
    unit = "mg/L"
  )
  expect_snapshot(
    plot_thresholds(df_in, thresh, date_range, y_range)
  )

  # Test - NO thresh
  expect_snapshot(
    suppressMessages(
      plot_thresholds(df_in, NULL, date_range, y_range)
    )
  )
})

test_that("thresh_text works", {
  # Test1
  thresh <- list(
    thresh_min = 5,
    thresh_max = NA,
    thresh_exc = 8,
    thresh_best = "high",
    unit = "mg/L"
  )
  txt_out <- "<b>Acceptable:</b> &gt; 5 mg/L<br><b>Excellent:</b> &gt; 8 mg/L"

  expect_equal(
    thresh_text(thresh),
    txt_out
  )

  # Test2
  thresh <- list(
    thresh_min = NA,
    thresh_max = 20,
    thresh_exc = 8,
    thresh_best = "low",
    unit = "mg/L"
  )
  txt_out <- "<b>Acceptable:</b> &lt; 20 mg/L<br><b>Excellent:</b> &lt; 8 mg/L"

  expect_equal(
    thresh_text(thresh),
    txt_out
  )

  # Test3
  thresh <- list(
    thresh_min = 5,
    thresh_max = 20,
    thresh_exc = NA,
    thresh_best = NA,
    unit = "mg/L"
  )
  txt_out <- "<b>Acceptable:</b> 5 - 20 mg/L"

  expect_equal(
    thresh_text(thresh),
    txt_out
  )
})

test_that("val_range works", {
  df_in <- tst$data_final[c(1:4, 6:9), ]

  expect_equal(
    val_range(df_in),
    c(0, 10.8)
  )

  # Test edge cases
  df_in$Result <- 0
  expect_equal(
    val_range(df_in),
    c(0, 1)
  )

  df_in$Result <- c(-1, -2)
  expect_equal(
    val_range(df_in),
    c(-2.4, 0)
  )
})
