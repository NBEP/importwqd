#' Embed quarto document
#'
#' @description `embed_quarto()` converts a `Quarto` HTML document to simple
#' HTML that can be embedded in a Shiny app.
#'
#' @param path String. Path to exported Quarto .html document.
#'
#' @return Simple HTML
#'
#' @export
embed_quarto <- function(path){
  x <- rvest::read_html(path) |>
    rvest::html_element("body") |>
    as.character()
  x <- gsub("\r", "", x)
  x <- gsub("\n", "", x)
  x <- gsub('img src="', 'img src="www/', x)

  HTML(x)
}
