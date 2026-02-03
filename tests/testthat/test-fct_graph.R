test_that("graph_trends works", {
  df_in <- tst$data_final[1:3, ]
  thresh <- list(
    thresh_min = 5,
    thresh_max = NA,
    thresh_exc = 8,
    thresh_best = "high",
    unit = "mg/L"
  )

  expect_snapshot(
    graph_trends(df_in, thresh, create_trend = FALSE)
  )

  # Edge cases
  expect_equal(
    graph_trends(tst$data_final[0, ], thresh),
    NULL
  )
})
