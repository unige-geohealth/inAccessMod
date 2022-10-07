#' Download Landcover
#'
#' Download the Land Cover 100 m from the Copernicus Global Land Service and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param alwaysDownload logical; should the raster always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param mostRecent logical; should the most recent boundary shapefile be selected to define the required landcover tiles? 
#' If FALSE and if there are multiple available inputs, the user is interactively asked to select the input based on file creation
#' time.
#' @details The function downloads the landcover tiles from the AWS cloud and determines the file names based on the extent
#' of the boundary shapefile. If there are multiple tiles, it produces a mosaic.
#' @export
download_landcover <- function (mainPath, country, alwaysDownload = FALSE, mostRecent = FALSE) {
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
  pathLandcover <- paste0(mainPath, "/", country, "/data/rLandcover")
  folders <- check_exists(pathLandcover, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  awsLCFolder <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
  awsLCSuffix <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"
  border <- get_boundaries(mainPath, country, "raw", mostRecent)
  # Based on https://lcviewer.vito.be/download and the names of available files for downloading
  # Coordinate intervals
  seqCoord <- list(X = seq(from = -180, to = 180, by = 20), Y = seq(from = -40, to = 80, by = 20))
  # Get extent of the boundaries
  minMax <- list(X = c(raster::extent(border)[1], raster::extent(border)[2]), Y = c(raster::extent(border)[3], raster::extent(border)[4]))
  # The file name: lower X limit, upper Y limit (see tiles at https://lcviewer.vito.be/download)
  # findInterval function: upper limit
  adjustTile <- c(X = -1, y = 0)
  prefixes <- list(c("E", "N") ,c("W", "S"))
  partialTileDeg <- vector(mode = "list", length = 2)
  tileName <- NULL
  for (i in 1:length(seqCoord)) {
    seqDeg <- seqCoord[[i]]
    coords <- minMax[[i]]
    for (j in 1:length(coords)) {
      coord <- coords[j]
      if (coord %in% seqDeg) {
        if (j==1) {
          coord <- coord+1
        }else{
          coord <- coord-1
        }
      }
      if (coord > max(seqDeg) | coord < min(seqDeg)) {
        stop("Country outside the limits of the Land Cover availability.")
      }
      getPosition <- findInterval(seqDeg, vec=coord)
      partialTileDeg[[i]][j] <- seqDeg[min(which(getPosition == 1)) + adjustTile[i]]
    }
  }
  seqTilesLst <- vector(mode = "list", length = 2)
  for (i in 1:length(partialTileDeg)) {
    seqTilesLst[[i]] <- seq(partialTileDeg[[i]][1], partialTileDeg[[i]][2], by = 20)
  }
  urls <- NULL
  codeFiles <- NULL
  for (i in seqTilesLst[[1]]) {
    if (i < 0) {
      pref <- prefixes[[2]][1]
    }else{
      pref <- prefixes[[1]][1]
    }
    missing0 <- (nchar(as.character(max(seqCoord[[1]]))) - nchar(as.character(abs(i))))
    if (missing0 == 2) {
      charX <- paste0(pref, "00", abs(i))
    }else if (missing0==1) {
      charX <- paste0(pref, "0", abs(i))
    }else{
      charX <- paste0(pref, abs(i))
    }
    for (j in seqTilesLst[[2]]) {
      if (j < 0) {
        pref <- prefixes[[2]][2]
      }else{
        pref <- prefixes[[1]][2]
      }
      missing0 <- (nchar(as.character(max(seqCoord[[2]]))) - nchar(as.character(abs(j))))
      if (missing0 == 1) {
        charY <- paste0(pref, "0", abs(j))
      }else{
        charY <- paste0(pref, abs(j))
      }
      codeFiles <- c(codeFiles, paste0(charX, charY))
      urls <- c(urls, paste0(awsLCFolder, charX, charY, "/", charX, charY, awsLCSuffix))
    }
  }
  logTxt <- paste0(mainPath, "/", country, "/data/log.txt")
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathLandcover, "/", timeFolder, "/raw"), recursive = TRUE)
  pathLandcover <- paste0(pathLandcover, "/", timeFolder, "/raw")
  if (length(urls) == 1) {
    dw <- tryCatch({utils::download.file(urls, destfile = paste0(pathLandcover, "/", country, awsLCSuffix, ".tif"), mode = "wb")}, error = function(e) NULL)
    if (is.null(dw)) {
      stop(paste("Error: cannot open URL (single tile)", urls[i]))
    }
    write(paste0(Sys.time(), ": Single landcover tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }else{
    # Download tiles shapefile in a temporary folder
    tmpFolder <- paste0(pathLandcover, "/temp")
    dir.create(tmpFolder)
    for (i in 1:length(urls)) {
      cat(paste0("Downloading tile ", i, "/", length(urls), "...\n"))
      # If extent is outside the available tiles
      dw <- tryCatch({utils::download.file(urls[i], destfile = paste0(tmpFolder, "/", codeFiles[i], ".tif"), mode = "wb")}, error = function(e) NULL)
      if (is.null(dw)) {
        next
      }
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    files <- list.files(tmpFolder, pattern = "\\.tif", full.names=TRUE)
    # Gdal mosaic
    mosaicGDAL <- tryCatch({gdalUtils::mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathLandcover, "/", country, awsLCSuffix, ".tif"), of="GTiff")}, error = function (e) 0)
    if (!is.null(mosaicGDAL) && mosaicGDAL == 0) {
      message("GDAL not found/issues -> mosaicking the tiles using the terra::merge function (slower)")
      lcLst <- list()
      for (i in 1:length(urls)) {
        ras <- tryCatch({terra::rast(paste0(tmpFolder, "/", codeFiles[i], ".tif"))}, error = function (e) NULL)
        if (!is.null(ras)){
          lcLst[[i]] <- ras
        }
      }
      newRas <- do.call(terra::merge, lcLst)
      terra::writeRaster(newRas, paste0(pathLandcover, "/", country, awsLCSuffix, ".tif"))
    } 
    write(paste0(Sys.time(), ": Multiple landcover tiles downloaded and mosaicked - Input folder ", timeFolder), file = logTxt, append = TRUE)
    unlink(tmpFolder, recursive = TRUE)
  }
  cat(paste0(pathLandcover, "/", country, awsLCSuffix, "\n"))
}
