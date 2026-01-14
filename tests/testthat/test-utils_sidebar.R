test_that("set_loc_choices works", {
  expect_equal(
    set_loc_choices(tst$sites_final),
    c("By Town" = "town", "By Watershed" = "watershed")
  )

  # Test edge cases ---
  df_in <- tst$sites_final
  df_in$Town <- NULL
  expect_equal(
    set_loc_choices(df_in),
    c("By State" = "town", "By Watershed" = "watershed")
  )

  df_in$Watershed <- NULL
  df_in$State <- NULL
  expect_equal(
    set_loc_choices(df_in),
    "blank"
  )
})

test_that("loc_tab works", {
  expect_equal(
    loc_tab(c("By Town", "By Watershed")),
    "toggle"
  )
  expect_equal(
    loc_tab("By Watershed"),
    "notoggle"
  )
  expect_equal(
    loc_tab("blank"),
    "blank"
  )
})

test_that("filter_towns works", {
  towns <- tst$sites_final

  expect_equal(
    filter_towns(towns, c("Rhode Island", "Massachusetts")),
    c("Providence, RI", "Worcester, MA")
  )
  expect_equal(
    filter_towns(towns, "Rhode Island"),
    "Providence, RI"
  )
  expect_equal(
    filter_towns(towns, "Connecticut"),
    NULL
  )
  expect_equal(
    filter_towns(towns, NULL),
    c("Providence, RI", "Worcester, MA")
  )
})

test_that("create_site_list works", {
  df_in <- data.frame(
    Site_ID = c(1, 2, 3),
    Site_Name = c("foo", "bar", "foofy")
  )

  site_list <- c(2, 1, 3)
  names(site_list) <- c("bar", "foo", "foofy")

  expect_equal(
    create_site_list(df_in),
    site_list
  )

  # Edge case
  expect_equal(
    create_site_list(df_in[0, ]),
    NULL
  )
})

test_that("filter_site_list works", {
  site_out <- "001"
  names(site_out) <- "Site1"

  expect_equal(
    filter_site_list(
      tst$sites_final,
      "State",
      c("Rhode Island", "Connecticut")
    ),
    site_out
  )
})
