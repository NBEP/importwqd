#' report_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_card_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::card(
      min_height = 250,
      full_screen = FALSE,
      htmlOutput(ns("title")),
      reactable::reactableOutput(ns("table")),
      div(
        style = "text-align:center;display:inline-block;",
        downloadButton(
          ns("download_pdf"),
          "Download as PDF",
          style = "width: fit-content;"
        )
      ),
    )
  )
}

#' report_card Server Functions
#'
#' @param in_var Reactive output from `mod_sidebar_server`.
#' @param df_raw Dataframe. Default report card data.
#' @param selected_tab Reactive string. Name of selected tab.
#'
#' @noRd
mod_report_card_server <- function(
    id, in_var, df_raw, selected_tab, org_name=NULL
  ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pass info to ui ----
    output$title <- renderUI({
      HTML(
        paste0("<h2>Report Card (", in_var$year(), ")</h2>")
      )
    })

    # Define variables -----
    # val <- reactiveValues(
    #   df_static = df_raw
    # )
    #
    # report_tab <- reactive({
    #   if (selected_tab() == "report_card") {
    #     return(TRUE)
    #   } else {
    #     return(FALSE)
    #   }
    # })
    #
    # observe({
    #   req(report_tab())
    #
    #   val$df_static <- in_var$df_report()
    # }) |>
    #   bindEvent(report_tab(), ignoreInit = TRUE, once = TRUE)

    # Table ----
    output$table <- reactable::renderReactable({
      report_table(df_raw)
    })

    # Update table
    observe({
      reactable::updateReactable("table", data = in_var$df_report())
    }) |>
      bindEvent(in_var$df_report())

    # Download PDF ----
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("report_card_", in_var$year(), ".pdf")
      },
      content = function(file) {
        src <- normalizePath(
          system.file(
            "rmd", "report_card.Rmd", package = "importwqd"
          )
        )

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        tempReport <- file.path(tempdir(), "report_card.Rmd")
        file.copy(src, tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          df_report = in_var$df_report(),
          year = in_var$year()
        )

        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
