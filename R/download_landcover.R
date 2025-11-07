#' Download Landcover
#'
#' Download the Land Cover 100 m from the Copernicus Global Land Service and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param globalLandCover character; the path to the global land cover data (Cpernicus 100m discrete classification);
#' if NULL, the function will download the data from Zenodo and copy it to pathStoreLandCover
#' @param pathStoreLandCover character; the path to the folder where the global land cover data should be stored. When it is NULL
#' it saves the data in a temporary directory.
#' @param mostRecent logical; should the most recent boundary shapefile be selected to define the required landcover area?
#' If FALSE and if there are multiple available inputs, the user is interactively asked to select the input based on file creation
#' time.
#' @param timeout numeric; the maximum time to wait for downloading the global land cover data, in seconds (default 600s)
#' @details The function will take the global land cover data and crop it to the extent of the boundaries of the location.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myLocation with the location name you are working on (workDir subfolder)
#' \dontrun{
#' location <- "myLocation"
#' download_boundaries(mainPath, location, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)
#' download_landcover(mainPath, location, globalLandCover = NULL, mostRecent = TRUE)}
#' @export
download_landcover <- function (mainPath, location, globalLandCover = NULL, pathStoreLandCover = NULL, mostRecent = FALSE, timeout = 600) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  if (!is.null(globalLandCover) && !is.character(globalLandCover)) {
    stop("globalLandCover must be 'character'")
  }

  if (!is.null(pathStoreLandCover)) {
    if (!is.character(pathStoreLandCover)) {
      stop("pathStoreLandCover must be 'character'")
    } 
    if (!dir.exists(pathStoreLandCover)) {
      stop("pathStoreLandCover is not a valid directory")
    }
    pathStoreLandCover_usr <- TRUE
  } else {
    pathStoreLandCover_usr <- FALSE
    pathStoreLandCover <- tempdir()
  }

  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  
  if (!is.numeric(timeout)){
    stop("timeout must be 'numeric'")
  }
  timeout <- round(timeout)
  # Check directory
  pathLandcover <- file.path(mainPath, location, "data", "rLandcover")
  border <- get_boundaries(mainPath, location, "raw", mostRecent)
  # Is the raw boundary in lon lat ?
  if (terra::linearUnits(as(border, "SpatVector")) != 0) {
    # Projection transformation
    border <- sf::st_transform(border, crs = "+proj=longlat +datum=WGS84")
  }
  
  # Load the raster with the land cover data
  if (is.null(globalLandCover)) {
    download <- TRUE
    currentTimeout <- getOption("timeout")
    options(timeout = timeout) 
    message("Downloading the global land cover data; can take time, please be patient !")
    # Download the land cover data from Zenodo
    url <- "https://zenodo.org/records/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif?download=1"
    globalLandCover <- file.path(pathStoreLandCover, "LC2019_ProbaV.tif")
    # Download, method = "libcurl"
    utils::download.file(url, globalLandCover, mode = "wb")
    options(timeout = currentTimeout) 
  } else {
    download <- FALSE
  }
  landcover <- tryCatch({terra::rast(globalLandCover)}, error = function(e) NULL)
  if (is.null(landcover)) {
    stop("Error: cannot open land cover raster file")
  }
  message("Cropping land cover data...")
  cropedLC <- terra::crop(landcover, terra::vect(border))
  logTxt <- file.path(mainPath, location, "data", "log.txt")
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  pathLandcover <- file.path(pathLandcover, timeFolder, "raw")
  check_path_length(pathLandcover)
  dir.create(pathLandcover, recursive = TRUE)
  check_path_length(file.path(pathLandcover, paste0(location, "_LC100.tif")))
  cropedLC <- terra::as.int(cropedLC)
  terra::writeRaster(cropedLC, file.path(pathLandcover, paste0(location, "_LC100.tif")), overwrite = TRUE)
  write(paste0(Sys.time(), ": Land cover data prepared - Input folder ", timeFolder), file = logTxt, append = TRUE)
  cat(paste0("Done: ", pathLandcover, "/", location, "_LC100.tif", "\n"))
  if (download & pathStoreLandCover_usr) {
    cat(paste0("For other projects, you will find the downloaded global land cover data at: ", globalLandCover, "\n"))
  }
  return(TRUE)
}