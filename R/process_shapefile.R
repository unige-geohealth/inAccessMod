#' Project and Clip Shapefile
#'
#' Internal function used to project and clip shapefiles
#' @param shp \code{sf} object to be processed
#' @param epsg character; string that can be used as input in \code{sf::st_crs()} to describe a projection and datum
#' @param inputName character; the input name for console printing
#' @return \code{sf} object 
#' @export
process_shapefile <- function (shp, epsg, inputName) {
  cat(paste("\nProjecting:", inputName, "shapefile\n"))
  shp <- sf::st_transform(shp, sf::st_crs(epsg))
  if (inputName == "vRoads") {
    # If no column with integer values
    if (!any(sapply(shp, is.integer))) {
      message("No column with integer values")
      ic <- utils::menu(colnames(shp), title = "Select the column (index) that indicates the road type")
      recl <- data.frame(class = unique(as.data.frame(shp)[, ic]), ind = 1:length(unique(as.data.frame(shp)[, ic])))
      shp <- merge(shp, recl, by.x = colnames(shp)[ic], by.y = "class")
    }
  }
  # border <- sf::st_transform(border, sf::st_crs(shp))
  # cat(paste("\nClipping:", inputName, "shapefile\n\n"))
  # shpInter <- sf::st_intersects(shp, border) 
  # shpInter <- lengths(shpInter) > 0
  # shpClip <- shp[shpInter, ]
  return(shp)
}