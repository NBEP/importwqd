test_that("popup_text works", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", "Ongoing"),
    "Year" = c(2012, 2016)
  )

  # Test basic
  expect_equal(
    popup_text(df_in, c("Project_Name", "Status", "Year")),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", "Ongoing"),
      "Year" = c(2012, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> 2012",
        "<b>Project Name:</b> bar<br><b>Status:</b> Ongoing<br><b>Year:</b> 2016"
      )
    )
  )

  # Test complex
  expect_equal(
    df_in %>%
      popup_text(
        col_list = c("Project_Name", "Year"),
        col_title = c("Project", "Start Year"),
        target_col = "Info",
        style = "&emsp;in_title: in_data"
      ),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", "Ongoing"),
      "Year" = c(2012, 2016),
      "Info" = c(
        "&emsp;Project: foo<br>&emsp;Start Year: 2012",
        "&emsp;Project: bar<br>&emsp;Start Year: 2016"
      )
    )
  )
})

test_that("popup_text handles NA values", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", NA),
    "Year" = c(NA, 2016)
  )

  # Default settings
  expect_equal(
    popup_text(df_in, c("Project_Name", "Status", "Year")),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", NA),
      "Year" = c(NA, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> -",
        "<b>Project Name:</b> bar<br><b>Status:</b> -<br><b>Year:</b> 2016"
      )
    )
  )
  # Custom NA value
  expect_equal(
    popup_text(
      df_in,
      c("Project_Name", "Status", "Year"),
      na_value = ""
    ),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", NA),
      "Year" = c(NA, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> ",
        "<b>Project Name:</b> bar<br><b>Status:</b> <br><b>Year:</b> 2016"
      )
    )
  )
  # Hide lines with NA value
  expect_equal(
    popup_text(
      df_in,
      c("Project_Name", "Status", "Year"),
      hide_na = TRUE
    ),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", NA),
      "Year" = c(NA, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete",
        "<b>Project Name:</b> bar<br><b>Year:</b> 2016"
      )
    )
  )
  # Nonexistant column
  expect_equal(
    popup_text(
      df_in,
      "foofy"
    ),
    df_in
  )
})

test_that("popup_text accepts pipes", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", "Ongoing"),
    "Year" = c(2012, 2016)
  )

  # Default settings
  expect_equal(
    popup_text(df_in, "Project_Name") %>%
      popup_text("Status") %>%
      popup_text("Year"),
    data.frame(
      "Project_Name" = c("foo", "bar"),
      "Status" = c("Complete", "Ongoing"),
      "Year" = c(2012, 2016),
      "Popup" = c(
        "<b>Project Name:</b> foo<br><b>Status:</b> Complete<br><b>Year:</b> 2012",
        "<b>Project Name:</b> bar<br><b>Status:</b> Ongoing<br><b>Year:</b> 2016"
      )
    )
  )
})

test_that("popup_text error messages", {
  df_in <- data.frame(
    "Project_Name" = c("foo", "bar"),
    "Status" = c("Complete", "Ongoing"),
    "Year" = c(2012, 2016)
  )

  expect_error(
    popup_text(
      df_in,
      col_list = c("Project_Name", "Status", "Year"),
      col_title = "Project Name"
    ),
    regexp = "col_list and col_title must be same length"
  )
  expect_error(
    popup_text(df_in, "Project_Name", style = "foo"),
    regexp = "style must include in_title and in_data"
  )
})
