test_that("wrap_text works", {
  long_text <- "This is a long string of text."

  expect_equal(
    wrap_text(long_text, str_len = 20),
    "This is a long\nstring of text."
  )
  expect_equal(
    wrap_text(long_text, str_len = 10),
    "This is\na long\nstring of\ntext."
  )
  expect_equal(
    wrap_text(long_text, str_len = 20, linebreak = "foo"),
    "This is a longfoostring of text."
  )
})

test_that("pretty_number works", {
  expect_equal(pretty_number(123.123), 123.12)
  expect_equal(pretty_number(1234), 1234)
  expect_equal(pretty_number(0.001234), 0.0012)
})

test_that("pretty_unit works", {
  expect_equal(pretty_unit("foo", "bar"), "foo (bar)")
  expect_equal(pretty_unit("foo", NA), "foo")
  expect_equal(pretty_unit("foo", "None"), "foo")
})

test_that("pretty_list works", {
  expect_equal(pretty_list("foo"), "foo")
  expect_equal(pretty_list(c("foo", "bar")), "foo and bar")
  expect_equal(pretty_list(c("foo", "bar", NA)), "foo and bar")
  expect_equal(pretty_list(c("foo", "bar", "foofy")), "foo, bar, and foofy")
  expect_equal(pretty_list(c(NA, NA, NA)), NA)
})
