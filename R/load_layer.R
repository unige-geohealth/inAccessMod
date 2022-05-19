#' Load spatial layer
#'
#' Internal function that is used to load either a raster or a shapefile
#' @param folder character; folder path of the layer
#' @param multiMsg character; used for printing in case of the user is interactively asked to select one layer from multiple ones
#' @param return a list of length 2; the first element can be a \code{SpatRaster} or NULL, 
#' and the second element can be a \code{sf} object or NULL.
#' @export
load_layer <- function (folder, multiMsg) {
  rasterLayer <- FALSE
  vectorLayer <- FALSE
  files <- list.files(folder)
  if (length(files) > 0) {
    filesTif <- files[grepl("\\.tif$", files)]
    if (length(filesTif) > 0) {
      rasterLayer <- TRUE
    }
    filesShp <- files[grepl("\\.shp$", files)]
    if (length(filesShp) > 0) {
      vectorLayer <- TRUE
    }
  } else {
    # Usually not necessary this function is used on folders with available data
    # Check is performed previously
    stop("Input is missing.")
  }
  if (rasterLayer) {
    if (length(filesTif) > 1) {
      fileInd <- utils::menu(filesTif, multiMsg)
      file <- filesTif[fileInd]
    }else{
      file <- filesTif
    }
    ras <- terra::rast(paste0(folder, "/", file))
  }else{
    ras <- NULL
  }
  if (vectorLayer) {
    if (length(filesShp) > 1) {
      fileInd <- utils::menu(filesShp, multiMsg)
      file <- filesShp[fileInd]
    }else{
      file <- filesShp
    }
    shp <- sf::st_read(paste0(folder, "/", file), quiet=TRUE)
  }else{
    shp <- NULL
  }
  return(list(ras, shp))
}