#' Import shapefile
#'
#' @description `qaqc_shp()` prepares shapefiles for `wqdashboard`. Shapefiles
#' must use the `WGS 84` projection.
#'
#' @param in_shp String. Name of shapefile.
#' @param field_name String. Name of field that contains polygon names. Text in
#' this field will be used for hovertext and alt text.
#' @param path String. Path to shapefile.
#'
#' @return Updated shapefile
#'
#' @export
qaqc_shp <- function(in_shp, field_name, path = NULL) {
  in_shp <- gsub(".shp", "", in_shp)

  dat <- sf::read_sf(dsn = path, layer = in_shp)

  chk <- sf::st_crs(dat)$input == "WGS 84"
  if (!chk) {
    stop("Shapefile must use WGS 84 projection")
  }

  dat <- dplyr::select(dat, !!field_name)
  colnames(dat)[1] <- "Label"

  dat
}
