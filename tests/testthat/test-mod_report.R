library(shiny)
library(shinytest2)

testServer(
  mod_report_server,
  # Add here your module params
  args = list(
    df_raw = tst$data_score[0, ] |>
      dplyr::select(
        dplyr::any_of(
          c(
            "Site_Name", "State", "Town", "Watershed", "Group", "Depth",
            "Parameter", "score_str"
          )
        )
      ),
    org_name = "ORG NAME"
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
  ui <- mod_report_ui("test")

  # Check that formals have not been removed
  fmls <- formals(mod_report_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
