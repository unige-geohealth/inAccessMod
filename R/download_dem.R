#' Download Digital Elevation Model
#'
#' Download a SRTM 90m resolution (3 arc-seconds) DEM for the entire country and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
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
download_dem <- function (mainPath, country, alwaysDownload = FALSE, mostRecent = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  # Check directory
  pathDEM <- paste0(mainPath, "/", country, "/data/rDEM")
  folders <- check_exists(pathDEM, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  border <- get_boundaries(mainPath, country, "raw", mostRecent)
  border <- as(sf::st_combine(border), "Spatial")
  # Download SRTM tiles shapefile in a temporary folder
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathDEM, "/", timeFolder, "/raw"), recursive = TRUE)
  pathDEM <- paste0(pathDEM, "/", timeFolder, "/raw")
  tmpFolder <- paste0(pathDEM, "/temp")
  check_path_length(tmpFolder)
  dir.create(tmpFolder)
  urlSRTM <- "https://github.com/sikli/srtm_country/archive/master.zip"
  utils::download.file(url = urlSRTM, destfile = paste0(tmpFolder, "/srtm.zip"))
  utils::unzip(zipfile = paste0(tmpFolder, "/srtm.zip"), overwrite = TRUE, exdir= tmpFolder)
  shp <- raster::shapefile(paste0(tmpFolder, "/srtm_country-master/srtm/tiles.shp"))
  # Here it does not matter if border is not in lon lat (for land cover, we have a check)
  border <- sp::spTransform(border, shp@proj4string)
  intersects <- rgeos::gIntersects(border, shp, byid=TRUE)
  tiles <- shp[intersects[,1],]
  logTxt <- paste0(mainPath, "/", country, "/data/log.txt")
  revertDownloadOptions <- FALSE
  #Download tiles
  if (length(tiles) > 1) {
    srtmList  <- list()
    for (i in 1:length(tiles)) {
      cat(paste0("Downloading tile ", i, "/", length(tiles), "...\n"))
      lon <- raster::extent(tiles[i,])[1]  + (raster::extent(tiles[i,])[2] - raster::extent(tiles[i,])[1]) / 2
      lat <- raster::extent(tiles[i,])[3]  + (raster::extent(tiles[i,])[4] - raster::extent(tiles[i,])[3]) / 2
      # List and write (list ready if mosaic_gdal doesn't work)
      tile <- tryCatch({geodata::elevation_3s(lon = lon, lat = lat, path = paste0(tmpFolder,"/"))}, error = function (e) NULL)
      if (is.null(tile)) {
        message("Cannot open URL. Trying with 'curl' and ignoring potential SSL issues.")
        options(download.file.method="curl", download.file.extra="-k -L")
        tile <- geodata::elevation_3s(lon = lon, lat = lat, path = paste0(tmpFolder,"/"))
        revertDownloadOptions <- TRUE
      }
      srtmList[[i]] <- tile
    }
    if (revertDownloadOptions) {
      options(download.file.method=NULL, download.file.extra=NULL)
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    # Gdal mosaic (faster)
    files <- list.files(tmpFolder, pattern = "tif", full.names = TRUE)
    # Try gdal (tryCatch to avoid function to stop)
    mosaicGDAL <- tryCatch({gdalUtils::mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathDEM, "/srtm.tif"), of = "GTiff")}, error = function (e) NULL, warning = function (e) NULL)
    # Warning can prevent the function from running. Let's check if the output has been created. 
    if (!file.exists(paste0(pathDEM, "/srtm.tif"))) {
      mosaicGDAL <- 1
    }
    if (!is.null(mosaicGDAL) && mosaicGDAL == 1) {
      message("GDAL library not found/issues -> mosaicking the tiles using the terra::merge function (slower)...")
      newRas <- tryCatch({do.call(terra::merge, srtmList)}, error = function (e) NULL)
      if (is.null(newRas)) {
        message("Memory issues: Too large ? Trying to mosaicking the tiles incrementally...")
        newRas <- do.call(terra::merge, srtmList[1:2])
        srtmList <- srtmList[-c(1:2)]
        while(length(srtmList) > 0) {
          srtmList[[length(srtmList) + 1]] <- newRas
          newRas <- do.call(terra::merge, srtmList[c(1, length(srtmList))])
          srtmList <- srtmList[-c(1, length(srtmList))]
        }
      }
      terra::writeRaster(newRas, paste0(pathDEM, "/srtm.tif"))
    }
    write(paste0(Sys.time(), ": Multiple DEM tiles downloaded and mosaicked"), file = logTxt, append = TRUE)
  }else{
    lon <- raster::extent(tiles[1,])[1]  + (raster::extent(tiles[1,])[2] - raster::extent(tiles[1,])[1]) / 2
    lat <- raster::extent(tiles[1,])[3]  + (raster::extent(tiles[1,])[4] - raster::extent(tiles[1,])[3]) / 2
    # tile <- raster::getData('SRTM', lon = lon, lat = lat, path = pathDEM)
    tile <- geodata::elevation_3s(lon = lon, lat = lat, path = pathDEM)
    write(paste0(Sys.time(), ": Single DEM tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }
  unlink(tmpFolder, recursive = TRUE)
  files <- list.files(pathDEM)
  filesTif <- files[grepl("^.*\\.tif$", files)]
  mtime <- file.info(list.files(path = pathDEM, pattern="\\.tif", full.names = TRUE))[,"mtime"]
  mostRecent <- which(order(as.POSIXct(mtime)) == 1)
  cat(paste0("\n", pathDEM, "/", filesTif[mostRecent], "\n"))
}
