test_that("pretty_number works", {
  expect_equal(
    pretty_number(12.234234234),
    12.23
  )
  expect_equal(
    pretty_number(0.00002342),
    0.000023
  )
  expect_equal(
    pretty_number(12345.12345),
    "12,345.12"
  )
})

test_that("pretty_unit works", {
  expect_equal(
    pretty_unit("pH", NA),
    "pH"
  )
  expect_equal(
    pretty_unit("Temperature", "deg C"),
    "Temperature (deg C)"
  )
})

test_that("unique_na works", {
  expect_equal(
    unique_na(c("foo", "bar", "foo", NA, "foo")),
    c("foo", "bar")
  )
  expect_equal(
    unique_na(c(NA, NA)),
    NA
  )
})
