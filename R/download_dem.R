#' Download Digital Elevation Model
#'
#' Download a SRTM 90m resolution (3 arc-seconds) DEM for the entire country and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param alwaysDownload logical; should the raster always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param mostRecent logical; should the most recent boundary shapefile be selected to define the required DEM tiles? 
#' If FALSE and if there are multiple available inputs, the user is interactively asked to select the input based on file
#' creation time.
#' @details The function first downloads a SRTM tile grid shapefile from \url{https://github.com/sikli/srtm_country}.
#' The SRTM tiles to be downloaded are selected based on the extent of the boundary shapefile and are downloaded using the 
#' \code{getData} function from the \pkg{raster} package. If there are multiple tiles, a mosaic is produced.
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
#' download_dem(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)}
#' @export
download_dem <- function (mainPath, location, alwaysDownload = FALSE, mostRecent = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  # Check directory
  pathDEM <- paste0(mainPath, "/", location, "/data/rDEM")
  folders <- check_exists(pathDEM, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  
  border <- get_boundaries(mainPath, location, "raw", mostRecent)
    # Download SRTM tiles shapefile in a temporary folder
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  pathDEM <- file.path(pathDEM, timeFolder, "raw")
  check_path_length(pathDEM)
  dir.create(pathDEM, recursive = TRUE)
  # Download tiles shapefile in a temporary folder (tempfile, this way is new and empty)
  tmpFolder <- tempfile()
  dir.create(tmpFolder)
  urlSRTM <- "https://github.com/sikli/srtm_country/archive/master.zip"
  utils::download.file(url = urlSRTM, destfile = paste0(tmpFolder, "/srtm.zip"))
  utils::unzip(zipfile = paste0(tmpFolder, "/srtm.zip"), overwrite = TRUE, exdir= tmpFolder)
  shp <- sf::st_read(file.path(tmpFolder, "srtm_country-master", "srtm", "tiles.shp"))
  
  # Here it does not matter if border is not in lon lat (for land cover, we have a check)
  border <- sf::st_transform(border, crs = sf::st_crs(shp))
  intersects <- suppressWarnings(sf::st_intersects(border, shp))
  tiles <- shp[unique(unlist(intersects)), ]
  logTxt <- file.path(mainPath, location, "data", "log.txt")
  revertDownloadOptions <- FALSE
  # Download tiles
  if (nrow(tiles) > 1) {
    srtmList  <- list()
    for (i in 1:nrow(tiles)) {
      cat(paste0("Downloading tile ", i, "/", nrow(tiles), "...\n"))
      lon <- raster::extent(tiles[i,])[1]  + (raster::extent(tiles[i,])[2] - raster::extent(tiles[i,])[1]) / 2
      lat <- raster::extent(tiles[i,])[3]  + (raster::extent(tiles[i,])[4] - raster::extent(tiles[i,])[3]) / 2
      tile <- tryCatch({geodata::elevation_3s(lon = lon, lat = lat, path = tmpFolder)}, error = function (e) NULL)
      if (is.null(tile)) {
        message("Cannot open URL. Trying with 'curl' and ignoring potential SSL issues.")
        options(download.file.method="curl", download.file.extra="-k -L")
        tile <- geodata::elevation_3s(lon = lon, lat = lat, path = tmpFolder)
        revertDownloadOptions <- TRUE
      }
      srtmList[[i]] <- tile
    }
    if (revertDownloadOptions) {
      options(download.file.method=NULL, download.file.extra=NULL)
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    rasCollect <- terra::sprc(srtmList) 
    newRas <- tryCatch({terra::merge(rasCollect)}, error = function (e) NULL)
    if (is.null(newRas)) {
      message("Memory issues: Too large ? Trying to mosaicking the tiles incrementally...")
      newRas <- do.call(terra::merge, srtmList[1:2])
      srtmList <- srtmList[-c(1:2)]
      while (length(srtmList) > 0) {
        srtmList[[length(srtmList) + 1]] <- newRas
        newRas <- do.call(terra::merge, srtmList[c(1, length(srtmList))])
        srtmList <- srtmList[-c(1, length(srtmList))]
      }
    }
    terra::writeRaster(newRas, file.path(pathDEM, "srtm.tif"))
    write(paste0(Sys.time(), ": Multiple DEM tiles downloaded and mosaicked"), file = logTxt, append = TRUE)
  } else {
    lon <- raster::extent(tiles[1,])[1]  + (raster::extent(tiles[1,])[2] - raster::extent(tiles[1,])[1]) / 2
    lat <- raster::extent(tiles[1,])[3]  + (raster::extent(tiles[1,])[4] - raster::extent(tiles[1,])[3]) / 2
    tile <- geodata::elevation_3s(lon = lon, lat = lat, path = pathDEM)
    write(paste0(Sys.time(), ": Single DEM tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }
  files <- list.files(pathDEM)
  filesTif <- files[grepl("^.*\\.tif$", files)]
  mtime <- file.info(list.files(path = pathDEM, pattern="\\.tif", full.names = TRUE))[,"mtime"]
  mostRecent <- which(order(as.POSIXct(mtime)) == 1)
  cat(paste0("Done: ", pathDEM, "/", filesTif[mostRecent], "\n"))
  return(TRUE)
}
