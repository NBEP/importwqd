test_that("num_pal works", {
  pal <- leaflet::colorNumeric(
    palette = c(
      "#b2fd99", "#97e39d", "#7dcaa1", "#63b1a5", "#4997a9",
      "#417ea0", "#47638c", "#4e4876", "#55285d"
    ),
    domain = c(1, 2),
    na.color = "#f4f4f4"
  )

  expect_equal(
    num_pal(c(1, 2)),
    pal
  )

  # Test edge case - same value
  pal <- leaflet::colorNumeric(
    palette = c(
      "#b2fd99", "#97e39d", "#7dcaa1", "#63b1a5", "#4997a9",
      "#417ea0", "#47638c", "#4e4876", "#55285d"
    ),
    domain = c(0, 2),
    na.color = "#f4f4f4"
  )

  expect_equal(
    num_pal(c(1, 1)),
    pal
  )
})

test_that("num_shape works", {
  expect_equal(
    num_shape(NA),
    "cross"
  )
  expect_equal(
    num_shape(12),
    "circle"
  )
})

test_that("num_symbols works", {
  df_in <- tst$data_score[c(3:5, 9:12), ]
  range <- c(0.05, 9)

  expect_snapshot(
    num_symbols(df_in, range)
  )
})

test_that("cat_pal works", {
  expect_snapshot(
    cat_pal("Excellent")
  )
  expect_snapshot(
    cat_pal("Meets Criteria")
  )
  expect_snapshot(
    cat_pal(c("Excellent", "Meets Criteria"))
  )
  expect_snapshot(
    cat_pal("No Data Available", TRUE)
  )
})

test_that("cat_labels works", {
  expect_equal(
    cat_labels("Excellent"),
    c("Excellent", "Good", "Fair", "Poor", "No Data Available")
  )
  expect_equal(
    cat_labels("Meets Criteria"),
    c("Meets Criteria", "Does Not Meet Criteria", "No Data Available")
  )
  expect_equal(
    cat_labels(c("Excellent", "Meets Criteria")),
    c(
      "Excellent", "Good / Meets Criteria", "Fair",
      "Poor / Does Not Meet Criteria", "No Data Available"
    )
  )
  expect_equal(
    cat_labels("No Data Available"),
    "No Data Available"
  )
})
