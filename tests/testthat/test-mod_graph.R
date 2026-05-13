library(shiny)
library(shinytest2)

testServer(
  mod_graph_server,
  # Add here your module params
  args = list(
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
  ui <- mod_graph_ui("test", tst$s_var)

  # Check that formals have not been removed
  fmls <- formals(mod_graph_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
