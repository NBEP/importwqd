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
    drop_rows <- c(
      "Year", "Site_ID", "Unit", "score_typ", "score_num",
      "Latitude", "Longitude", "popup_loc", "popup_score", "alt"
    )

    df_default <- df_raw |>
      dplyr::select(!dplyr::all_of(drop_rows))


    val <- reactiveValues(
      df_static = df_default,
      df_dynamic = df_default,
      count = 0
    )

    # * Active tab ----
    report_tab <- reactive({
      if (selected_tab() == "report_card") {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    observe({
      req(report_tab())
      req(in_var$df_score_f())
      req(in_var$param_short())
      req(in_var$sites_all())

      param <- c(in_var$param_short(), NA)
      sites <- in_var$sites_all()

      # Update dataframe
      df <- in_var$df_score_f() |>
        dplyr::filter(
          "Parameter" %in% !!param,
          "Site_ID" %in% !!sites
        ) |>
        dplyr::select(!dplyr::all_of(drop_rows))

      if ("Depth" %in% colnames(df)) {
        depth_list <- c(NA, in_var$depth_all())
        df <- dplyr::filter(df, .data$Depth %in% !!depth_list)
      }

      val$df_dynamic <- df |>
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            ~tidyr::replace_na(.x, "-")  # necessary for PDF
          )
        )
    }) |>
      bindEvent(
        report_tab(), in_var$df_score_f(), in_var$param_short(),
        in_var$sites_all(), in_var$depth_all()
      )


    observe({
      if (val$count < 2 & report_tab()) {
        val$count <- val$count + 1
        val$df_static <- val$df_dynamic
      }
    }) |>
      bindEvent(selected_tab())

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      report_table(val$df_static)
    })

    # Update table
    observe({
      reactable::updateReactable("table", data = val$df_dynamic)
    }) |>
      bindEvent(val$df_dynamic)

    # Download PDF ----
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("report_card_", in_var$year(), ".pdf")
      },
      content = function(file) {
        src <- normalizePath(
          system.file(
            "rmd", "report_card.Rmd", package = "WQdashboard"
            )
          )

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        tempReport <- file.path(tempdir(), "report_card.Rmd")
        file.copy(src, tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          df_report = val$df_dynamic,
          report_title = paste0(
            org_name, " Report Card (",
            in_var$year(), ")"
          )
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
