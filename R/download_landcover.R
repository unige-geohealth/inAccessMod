#' Download Landcover
#'
#' Download the Land Cover 100 m from the Copernicus Global Land Service and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param alwaysDownload logical; should the raster always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param mostRecent logical; should the most recent boundary shapefile be selected to define the required landcover tiles? 
#' If FALSE and if there are multiple available inputs, the user is interactively asked to select the input based on file creation
#' time.
#' @details The function downloads the landcover tiles from the AWS cloud and determines the file names based on the extent
#' of the boundary shapefile. If there are multiple tiles, it produces a mosaic.
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
#' download_landcover(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)}
#' @export
download_landcover <- function (mainPath, location, alwaysDownload = FALSE, mostRecent = FALSE) {
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
  pathLandcover <- file.path(mainPath, location, "data", "rLandcover")
  folders <- check_exists(pathLandcover, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  awsLCFolder <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
  awsLCSuffix <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"
  border <- get_boundaries(mainPath, location, "raw", mostRecent)
  # Is the raw boundary in lon lat ?
  if (terra::linearUnits(as(border, "SpatVector")) != 0) {
    # Projection transformation
    border <- sf::st_transform(border, crs = "+proj=longlat +datum=WGS84")
  }
  # Based on https://lcviewer.vito.be/download and the names of available files for downloading
  # Coordinate intervals
  seqCoord <- list(X = seq(from = -180, to = 180, by = 20), Y = seq(from = -40, to = 80, by = 20))
  # Get extent of the boundaries
  minMax <- list(X = c(terra::ext(border)[1], terra::ext(border)[2]), Y = c(terra::ext(border)[3], terra::ext(border)[4]))
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
        stop("Location outside the limits of the Land Cover availability.")
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
  logTxt <- file.path(mainPath, location, "data", "log.txt")
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  pathLandcover <- file.path(pathLandcover, timeFolder, "raw")
  check_path_length(pathLandcover)
  dir.create(pathLandcover, recursive = TRUE)
  if (length(urls) == 1) {
    check_path_length(file.path(pathLandcover, paste0(location, awsLCSuffix)))
    dw <- tryCatch({utils::download.file(urls, destfile = file.path(pathLandcover, paste0(location, awsLCSuffix)), mode = "wb")}, error = function(e) NULL)
    if (is.null(dw)) {
      stop(paste("Error: cannot open URL (single tile)", urls[i]))
    }
    write(paste0(Sys.time(), ": Single landcover tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }else{
    # Download tiles shapefile in a temporary folder (tempfile, this way is new and empty)
    tmpFolder <- tempfile()
    dir.create(tmpFolder)
    for (i in 1:length(urls)) {
      cat(paste0("Downloading tile ", i, "/", length(urls), "...\n"))
      # If extent is outside the available tiles
      check_path_length(file.path(tmpFolder, paste0(codeFiles[i], ".tif")))
      dw <- tryCatch({utils::download.file(urls[i], destfile = file.path(tmpFolder, paste0(codeFiles[i], ".tif")), mode = "wb")}, error = function(e) NULL)
      if (is.null(dw)) {
        next
      }
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    lcLst <- list()
    for (i in 1:length(urls)) {
      check_path_length(file.path(tmpFolder, paste0(codeFiles[i], ".tif")))
      ras <- tryCatch({terra::rast(file.path(tmpFolder, paste0(codeFiles[i], ".tif")))}, error = function (e) NULL)
      if (!is.null(ras)){
        lcLst[[i]] <- ras
      }
    }
    rasCollect <- terra::sprc(lcLst) 
    newRas <- tryCatch({terra::merge(rasCollect)}, error = function (e) NULL)
    if (is.null(newRas)) {
      message("Memory issues: Too large ? Trying to mosaicking the tiles incrementally...")
      newRas <- do.call(terra::merge, lcLst[1:2])
      lcLst <- lcLst[-c(1:2)]
      while (length(lcLst) > 0) {
        lcLst[[length(lcLst) + 1]] <- newRas
        newRas <- do.call(terra::merge, lcLst[c(1, length(lcLst))])
        lcLst <- lcLst[-c(1, length(vlcLst))]
      }
    }
    check_path_length(file.path(pathLandcover, paste0(location, awsLCSuffix)))
    terra::writeRaster(newRas, file.path(pathLandcover, paste0(location, awsLCSuffix)))
    write(paste0(Sys.time(), ": Multiple landcover tiles downloaded and mosaicked - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }
  cat(paste0("Done: ", pathLandcover, "/", location, awsLCSuffix, "\n"))
  return(TRUE)
}
