test_that("dropdown works", {
  # Test default settings
  picker1 <- shinyWidgets::pickerInput(
    "foo",
    label = "Title",
    choices = c("a", "b", "c"),
    selected = c("a", "b", "c"),
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `selected-text-format` = "count > 1",
      `max-options` = NULL,
      container = "body"
    ),
    multiple = TRUE
  )

  expect_equal(
    dropdown(
      id = "foo",
      label = "Title",
      choices = c("c", "a", "b")
    ),
    picker1
  )

  # Test - named variables, single selection, decreasing
  choices <- c("Struthio casuarius", "Arctictis binturong", "Orycteropus afer")
  names(choices) <- c("Cassowary", "Binturong", "Aardvark")

  picker2 <- shinyWidgets::pickerInput(
    "foo",
    label = "Title",
    choices = choices,
    selected = "Struthio casuarius",
    options = list(
      `actions-box` = FALSE,
      `live-search` = TRUE,
      `selected-text-format` = "count > 1",
      `max-options` = NULL,
      container = "body"
    ),
    multiple = FALSE
  )

  expect_equal(
    dropdown(
      id = "foo",
      label = "Title",
      choices = c(
        "Arctictis binturong", "Orycteropus afer", "Struthio casuarius"
      ),
      choice_names = c("Binturong", "Aardvark", "Cassowary"),
      decreasing = TRUE,
      multiple = FALSE
    ),
    picker2
  )

  # Test - not sorted, max options
  picker3 <- shinyWidgets::pickerInput(
    "foo",
    label = "Title",
    choices = c("c", "a", "b"),
    selected = "c",
    options = list(
      `actions-box` = FALSE,
      `live-search` = TRUE,
      `selected-text-format` = "count > 1",
      `max-options` = 2,
      container = "body"
    ),
    multiple = TRUE
  )

  expect_equal(
    dropdown(
      id = "foo",
      label = "Title",
      choices = c("c", "a", "b"),
      sorted = FALSE,
      max_options = 2
    ),
    picker3
  )
})
