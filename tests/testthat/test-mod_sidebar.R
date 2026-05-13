library(shiny)
library(shinytest2)

testServer(
  mod_sidebar_server,
  # Add here your module params
  args = list(
    df_sites = tst$sites_final,
    df_data = tst$data_final,
    df_score = tst$data_score
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
  ui <- mod_sidebar_ui("test", varlist = tst$s_var)

  # Check that formals have not been removed
  fmls <- formals(mod_sidebar_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
