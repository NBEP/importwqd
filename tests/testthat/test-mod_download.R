library(shiny)
library(shinytest2)

testServer(
  mod_download_server,
  # Add here your module params
  args = list(
    sites = tst$sites_qaqc,
    results = tst$data_qaqc
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
  ui <- mod_download_ui("test")

  # Check that formals have not been removed
  fmls <- formals(mod_download_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
