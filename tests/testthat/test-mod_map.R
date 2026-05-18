library(shiny)
library(shinytest2)

testServer(
  mod_map_server,
  # Add here your module params
  args = list(
    map_bounds = list(
      lat1 = 41,
      lat2 = 43,
      lng1 = -72,
      lng2 = -71
    ),
    df_raw = tst$data_score[0, ]
  ),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
  }
)

test_that("module ui works", {
  ui <- mod_map_ui("test")

  # Check that formals have not been removed
  fmls <- formals(mod_map_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
