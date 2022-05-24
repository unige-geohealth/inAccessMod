#' Project and Clip Shapefile
#'
#' Internal function used to project and clip shapefiles
#' @param shp \code{sf} object to be processed
#' @param border \code{sf} object; a boundary shapefile used for clipping
#' @param epsg character; string that can be used as input in \code{sf::st_crs()} to describe a projection and datum
#' @param inputName character; the input name for console printing
#' @return \code{sf} object 
#' @export
process_shapefile <- function (shp, border, epsg, inputName) {
  cat(paste("\nProjecting:", inputName, "shapefile\n"))
  shp <- sf::st_transform(shp, sf::st_crs(epsg))
  border <- sf::st_transform(border, sf::st_crs(shp))
  cat(paste("\nClipping:", inputName, "shapefile\n\n"))
  shpInter <- sf::st_intersects(shp, border) 
  shpInter <- lengths(shpInter) > 0
  shpClip <- shp[shpInter, ]
  return(shpClip)
}