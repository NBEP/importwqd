#' Report card UI
#'
#' @description `mod_report_ui()` produces the sidebar UI for the companion app
#' `wqdashboard`.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_report_server()`.
#'
#' @export
mod_report_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Enable javascript ----
    shinyjs::useShinyjs(),
    # UI ----
    bslib::card(
      min_height = 250,
      full_screen = FALSE,
      htmlOutput(ns("title")),
      reactable::reactableOutput(ns("table")),
      div(
        style = "text-align:center;display:inline-block;",
        downloadButton(
          ns("download_pdf"),
          "Download Report (PDF)",
          style = "width: fit-content;"
        )
      ),
    )
  )
}

#' Report card server
#'
#' @description `mod_report_server()` produces the report card server for the
#' companion app `wqdashboard`.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_report_ui()`.
#' @param in_var Reactive output from `mod_sidebar_server`.
#' @param df_raw Dataframe. Default report card data.
#' @param selected_tab Reactive string. Name of selected tab.
#'
#' @export
mod_report_server <- function(id, in_var, df_raw, selected_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pass info to ui ----
    output$title <- renderUI({
      HTML(
        paste0("<h2>Report Card (", in_var$year(), ")</h2>")
      )
    })

    # Define variables -----
    val <- reactiveValues(
      df_static = df_raw
    )

    report_tab <- reactive({
      if (selected_tab() == "report_card") {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    observe({
      req(report_tab())

      val$df_static <- in_var$df_report()
    }) |>
      bindEvent(report_tab(), ignoreInit = TRUE, once = TRUE)

    # Table ----
    output$table <- reactable::renderReactable({
      report_table(val$df_static)
    })

    # Update table
    observe({
      reactable::updateReactable("table", data = in_var$df_report())
    }) |>
      bindEvent(in_var$df_report())

    # Download PDF ----
    # * Prep site data ----
    site_data <- reactive({
      req(in_var$df_report())

      dat <- in_var$df_report()

      loc_col <- c("Site_Name", "State", "Town", "Watershed", "Group")
      loc_col <- intersect(loc_col, colnames(dat))

      if (length(loc_col) < 2) {
        return(dat[0, ])
      }

      dat <- in_var$df_report() |>
        dplyr::select(dplyr::any_of(loc_col)) |>
        dplyr::distinct() |>
        prep_pdf(na_sub = "")

      if (ncol(dat) < 2) {
        return(dat[0, ])
      }

      dat
    })

    # * Toggle button -----
    enable_button <- reactive({
      if (nrow(in_var$df_report()) > 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })

    shinyjs::disable("download_pdf")

    observe({
      if (enable_button()) {
        shinyjs::enable("download_pdf")
      } else {
        shinyjs::disable("download_pdf")
      }
    }) |>
      bindEvent(enable_button())

    # * Download ----
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("report_card_", in_var$year(), ".pdf")
      },
      content = function(file) {
        # Download progress notification
        id <- showNotification(
          "Generating PDF...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)

        # Copy file to temporary directory
        src <- normalizePath(
          system.file("reports/report_card.qmd", package = "importwqd")
        )

        temp_report <- file.path(tempdir(), "report_card.qmd")
        file.copy(src, temp_report, overwrite = TRUE)

        # Prep dataframe
        dat <- in_var$df_report() |>
          dplyr::select(
            dplyr::any_of(c("Site_Name", "Depth", "Parameter", "score_str"))
          ) |>
          dplyr::rename("Score" = "score_str") |>
          prep_pdf(na_sub = "")

        # Render Quarto
        quarto::quarto_render(
          input = temp_report,
          execute_params = list(
            df_report = dat,
            df_site = site_data(),
            year = in_var$year()
          )
        )

        # Copy quarto document to `file` argument
        file.copy(file.path(tempdir(), "report_card.pdf"), file)
      }
    )
  })
}
