#' Download UI
#'
#' @description `mod_download_ui()` produces the UI code for the `wqdashboard`
#' download button.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_download_server()`.
#'
#' @export
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Enable javascript ----
    shinyjs::useShinyjs(),
    # UI ----
    downloadButton(
      ns("dl"),
      "Download Data",
      style = "width:fit-content"
    )
  )
}

#' Download server
#'
#' @description `mod_download_server()` produces the server code for the
#' `wqdashboard` download button.
#'
#' @param id Namespace ID for module. Should match ID used by
#' `mod_download_ui()`.
#' @param sites Dataframe. Site metadata.
#' @param results Dataframe. Result metadata.
#' @param in_var Reactive output from `mod_sidebar_server`.
#'
#' @export
mod_download_server <- function(id, sites, results, in_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Toggle button ----
    shinyjs::disable("dl")

    observe({
      if (length(in_var$sites_all() > 0)) {
        shinyjs::enable("dl")
      } else {
        shinyjs::disable("dl")
      }
    })

    # Download ----
    output$dl <- downloadHandler(
      filename = function() {
        "water_quality.zip"
      },
      content = function(file) {
        # Download progress notification
        id <- showNotification(
          "Downloading data...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)

        # Set temp directory
        temp_dir <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_dir)

        # Filter data
        site_list <- in_var$sites_all()
        param <- in_var$param_all()
        year <- in_var$year_all()
        depth_list <- c(NA, in_var$depth_all())

        sites_filter <- sites |>
          dplyr::filter(.data$Site_ID %in% !!site_list) |>
          prep_download(pretty_col = FALSE)

        results_filter <- results |>
          dplyr::filter(
            .data$Site_ID %in% !!site_list,
            .data$Parameter %in% !!param,
            .data$Year %in% !!year
          ) |>
          dplyr::select(!"Year")

        if (isTruthy(depth_list)) {
          results_filter <- results_filter |>
            dplyr::filter(.data$Depth_Category %in% !!depth_list)
        }

        results_filter <- results_filter |>
          prep_download(pretty_col = FALSE)

        # Create file
        temp_sites <- paste0(temp_dir, "/site_metadata.csv")
        temp_results <- paste0(temp_dir, "/result_data.csv")

        readr::write_excel_csv(sites_filter, temp_sites)
        readr::write_excel_csv(results_filter, temp_results)

        zip::zip(
          zipfile = file,
          files = dir(temp_dir),
          root = temp_dir
        )
      }
    )
  })
}
