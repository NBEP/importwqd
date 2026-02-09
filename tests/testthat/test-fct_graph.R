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
    graph_trends(df_in, thresh, trendline = FALSE)
  )

  display = list(lines = TRUE, trend = FALSE, thresh = TRUE)

  expect_snapshot(
    graph_trends(df_in, thresh, trendline = FALSE, display = display)
  )

  # Edge cases
  expect_equal(
    graph_trends(tst$data_final[0, ], thresh),
    NULL
  )
})

test_that("graph_compare works", {
  df_in <- tst$data_final[c(1:3,6:8), ]

  expect_snapshot(
    graph_compare(df_in, "title")
  )

  expect_snapshot(
    graph_compare(df_in, "title", add_lines = TRUE)
  )

  # Edge cases
  expect_equal(
    graph_compare(tst$data_final[0, ], "title"),
    NULL
  )
})

test_that("graph_param works", {
  df_in <- tst$data_final[c(1:3,5), ]

  expect_snapshot(
    graph_param(df_in, "title")
  )

  expect_snapshot(
    graph_param(df_in, "title", add_lines = TRUE)
  )

  # Edge cases
  expect_equal(
    graph_param(tst$data_final[0, ], "title"),
    NULL
  )
})
