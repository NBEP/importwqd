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
#' @noRd
mod_report_card_server <- function(id, selected_var, selected_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Pass info to ui ----
    output$title <- renderUI({
      HTML(paste0(
        "<h2>Report Card (", selected_var$year(), ")</h2>"
      ))
    })

    # Define variables -----
    drop_rows <- c(
      "Year", "Site_ID", "Unit", "score_typ", "score_num",
      "Latitude", "Longitude", "popup_loc", "popup_score", "alt"
    )

    df_default <- df_score %>%
      dplyr::filter(Year == max(Year)) %>%
      dplyr::select(!dplyr::all_of(drop_rows))

    val <- reactiveValues(
      df = df_default,
      count = 0
    )

    df_filter <- reactive({
      # Define var
      df <- selected_var$df_score_f()
      param <- c(selected_var$param_short(), "-")
      sites <- selected_var$sites_all()
      if (length(sites) == 0 | length(param) == 1) {
        return(df_default[0, ])
      }

      # Update dataframe
      df <- df %>%
        dplyr::filter(Parameter %in% param) %>%
        dplyr::filter(Site_ID %in% sites) %>%
        dplyr::select(!dplyr::all_of(drop_rows))

      if ("Depth" %in% colnames(df_score)) {
        depth_list <- c(NA, selected_var$depth_all())
        df <- dplyr::filter(df, Depth %in% depth_list)
      }

      if (!selected_var$score()) {
        df <- dplyr::filter(
          df,
          !(score_str %in% c("No Data Available", "No Threshold Established"))
        )
      }

      df <- df %>% replace(is.na(.), "-") # necessary for PDF

      return(df)
    })

    observe({
      if (val$count < 2 & selected_tab() == "report_card") {
        val$count <- val$count + 1
        val$df <- df_filter()
      }
    }) %>%
      bindEvent(selected_tab())

    # Table ------------------------------------------------------------------
    output$table <- reactable::renderReactable({
      reactable_table(val$df)
    })

    # Update table
    observe({
      reactable::updateReactable("table", data = df_filter())
    }) %>%
      bindEvent(df_filter())

    # Download PDF ----
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("report_card_", selected_var$year(), ".pdf")
      },
      content = function(file) {
        src <- normalizePath(system.file("rmd", "report_card.Rmd",
                                         package = "WQdashboard"
        ))

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        tempReport <- file.path(tempdir(), "report_card.Rmd")
        file.copy(src, tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          df_report = df_filter(),
          report_title = paste0(
            org_info$name, " Report Card (",
            selected_var$year(), ")"
          )
        )

        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}

## To be copied in the UI
# mod_report_card_ui("report_card_1")

## To be copied in the server
# mod_report_card_server("report_card_1")
