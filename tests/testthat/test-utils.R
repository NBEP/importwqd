test_that("pretty_number works", {
  expect_equal(
    pretty_number(12.234234234),
    12.23
  )
  expect_equal(
    pretty_number(0.00002342),
    0.000023
  )
})
