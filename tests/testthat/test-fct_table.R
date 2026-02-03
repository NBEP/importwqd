test_that("report_table works", {
  df_map <- tst$data_score[c(3:6, 9:12), c(2, 4:8, 11:12)]
  df_report <- tst$data_score[3:5, c(2, 4:8, 12)]

  expect_snapshot(
    report_table(df_map, show_score = FALSE, col_title = "Average (mg/L)")
  )
  expect_snapshot(
    report_table(df_report)
  )
})

test_that("graph_table works", {
  df_graph <- tst$data_final[c(1:3, 6:8), ]

  expect_snapshot(
    graph_table(df_graph, "Site_Name")
  )
})
