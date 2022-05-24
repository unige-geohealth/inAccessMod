#' Download Digital Elevation Model
#'
#' Download a SRTM 90m resolution (3 arc-seconds) DEM for the entire country and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param alwaysDownload logical; should the raster always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param mostRecent logical; should the most recent boundary shapefile be selected to define the required DEM tiles? 
#' If FALSE and if there are multiple available inputs, the user is interactively asked to select the input based on file
#' creation time.
#' @details The function first downloads a SRTM tile grid shapefile from \url{https://github.com/sikli/srtm_country}.
#' The SRTM tiles to be downloaded are selected based on the extent of the boundary shapefile and are downloaded using the 
#' \code{getData} function from the \pkg{raster} package. If there are multiple tiles, a mosaic is produced.
#' @export
download_dem <- function (mainPath, region, alwaysDownload = FALSE, mostRecent = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  # Check directory
  pathDEM <- paste0(mainPath, "/", region, "/data/rDEM")
  folders <- check_exists(pathDEM, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  message("\nLoading raw boundary shapefile...")
  border <- get_boundaries(mainPath, region, "raw", mostRecent)
  border <- as(border, "Spatial")
  border <- rgeos::gUnaryUnion(border)
  # Download SRTM tiles shapefile in a temporary folder
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathDEM, "/", timeFolder, "/raw"), recursive = TRUE)
  pathDEM <- paste0(pathDEM, "/", timeFolder, "/raw")
  tmpFolder <- paste0(pathDEM, "/temp")
  dir.create(tmpFolder)
  urlSRTM <- "https://github.com/sikli/srtm_country/archive/master.zip"
  utils::download.file(url = urlSRTM, destfile = paste0(tmpFolder, "/srtm.zip"))
  utils::unzip(zipfile = paste0(tmpFolder, "/srtm.zip"), overwrite = TRUE, exdir= tmpFolder)
  shp <- raster::shapefile(paste0(tmpFolder, "/srtm_country-master/srtm/tiles.shp"))
  intersects <- rgeos::gIntersects(border, shp, byid=TRUE)
  tiles <- shp[intersects[,1],]
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  #Download tiles
  if (length(tiles) > 1) {
    srtmList  <- list()
    for (i in 1:length(tiles)) {
      cat(paste0("Downloading tile ", i, "/", length(tiles), "...\n"))
      lon <- raster::extent(tiles[i,])[1]  + (raster::extent(tiles[i,])[2] - raster::extent(tiles[i,])[1]) / 2
      lat <- raster::extent(tiles[i,])[3]  + (raster::extent(tiles[i,])[4] - raster::extent(tiles[i,])[3]) / 2
      tile <- raster::getData('SRTM', lon = lon, lat = lat, path = paste0(tmpFolder,"/"))
      srtmList[[i]] <- tile
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    # Gdal mosaic (faster)
    files <- list.files(tmpFolder, pattern = "tif", full.names = TRUE)
    gdalUtils::mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathDEM, "/srtm.tif") ,of = "GTiff")
    write(paste0(Sys.time(), ": Multiple DEM tiles downloaded and mosaicked"), file = logTxt, append = TRUE)
  }else{
    lon <- raster::extent(tiles[1,])[1]  + (raster::extent(tiles[1,])[2] - raster::extent(tiles[1,])[1]) / 2
    lat <- raster::extent(tiles[1,])[3]  + (raster::extent(tiles[1,])[4] - raster::extent(tiles[1,])[3]) / 2
    tile <- raster::getData('SRTM', lon = lon, lat = lat, path = pathDEM)
    write(paste0(Sys.time(), ": Single DEM tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }
  unlink(tmpFolder, recursive = TRUE)
  files <- list.files(pathDEM)
  filesTif <- files[grepl("^.*\\.tif$", files)]
  mtime <- file.info(list.files(path = pathDEM, pattern="*.tif", full.names = TRUE))[,"mtime"]
  mostRecent <- which(order(as.POSIXct(mtime)) == 1)
  cat(paste0("\n", pathDEM, "/", filesTif[mostRecent], "\n"))
}