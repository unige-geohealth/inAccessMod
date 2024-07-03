#' Process Raster Layer
#'
#' Internal function used to crop, mask and project rasters
#' @param ras \code{SpatRaster} object; a raw input raster
#' @param border \code{sf} object; a boundary shapefile
#' @param epsg character; string that can be used as input in \code{raster::crs()} to describe a projection and datum
#' @param projMeth character; method used for estimating the new cell values. If NULL, the user is interactively
#' asked to select one of the available methods for \code{terra::project} function.
#' @return a list of length 2; The first element is the processed \code{SpatRaster} object and the second element is the selected
#' projection method (for track record)
#' @keywords internal
#' @export
process_raster <- function (ras, border, epsg, projMeth) {
  border <- sf::st_transform(as(border, "sf"), terra::crs(ras))
  cat(paste("Cropping:\n", terra::sources(ras)))
  rasCrop <- terra::crop(ras, border)
  cat(paste("\nMasking:\n", terra::sources(ras)))
  rasMask <- terra::mask(rasCrop, as(border, "SpatVector"))
  if (is.null(projMeth)) {
    projectionMethod <- c("near", "bilinear","cubic", "cubicspline")
    pm <- utils::menu(projectionMethod, title = cat(paste0("\n\nSelect projection method for:\n", terra::sources(ras), "\nSee terra::project function help for more details.")))
    if (pm == 0) {
      return(NULL)
    } else {
      projMeth <- projectionMethod[pm]
    }
  } 
  cat(paste("\nProjecting:\n", terra::sources(ras), "\n"))
  rasProject <- terra::project(rasMask, epsg, method = projMeth)
  return(list(rasProject, projMeth))
}