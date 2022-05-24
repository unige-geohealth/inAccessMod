#' Process Population Raster
#'
#' Internal function used to process the population raster and copy it to its corresponding process folder
#' @param mainPath character; the parent directory of the country/region name folder
#' @param region character; the country name
#' @param border \code{sf} object; a boundary shapefile
#' @param epsg character; string that can be used as input in \code{raster::crs()} to describe a projection and datum
#' @param mostRecent logical; should the most recent input be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @param defaultMethods logical; should be the default methods be used for projecting and resampling, respectively. These
#' are the 'bilinear' method for projecting and the 'sum' or the 'bilinear' for the resampling, depending on if the new resolution
#' is lower or higher.
#' @param changeRes logical; does the user want to change the raster resolution? If NULL, the resolution is printed and it is
#' interactively asked the user if they want to change it. IF FALSE, there is no resampling.
#' @param newRes numeric; new resolution in meters. Ignored if the newRes is FALSE. If NULL and if \code{changeRes} is TRUE,
#' the user is interactively asked to provide the new resolution.
#' @param popCorrection logical; should the raster correction algorithm be run. If it is NULL, the user is interactively asked
#' whether they want to run it or not.
#' @param gridRes numeric; the resolution of the grid shapefile used for correcting the raster. Ignored if popCorrection is FALSE.
#' If NULL and popCorrection is TRUE, the user is interactively asked to provide the grid resolution.
#' @return a list of length 2; The first element is the processed \code{SpatRaster} object and the second element is the selected
#' projection method (for track record)
#' @details The algorithm for correcting the population raster works as following: it creates a grid shapefile, it sums up the population
#' in each of the cell considering  both the 'raw' and the 'processed' population raster. Then a ratio is calculated between both
#' values and related to each grid cell. The grid shapefile is rasterized using the ratios as values, and finally
#' the 'processed' raster is multiplied by the rasterized ratio. The lower is the grid resolution, the finer is the correction.
#' @export
process_pop <- function (mainPath, region, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes) {
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  message("\nProcessing population raster...")
  popFolder <- paste0(mainPath, "/", region, "/data/rPopulation")
  popFolders <- check_exists(popFolder, "raw", layer = TRUE)
  if (is.null(popFolders)) {
    stop("No input population raster available.")
  }
  timeFolder <- select_input(popFolders, "Raster downloaded at", mostRecent)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")
  }
  popFolder <- paste0(popFolder, "/", timeFolder, "/raw")
  multipleFilesMsg <- "Select the population raster that you would like to process."
  popRas <- load_layer(popFolder, multipleFilesMsg)[[1]]
  if (defaultMethods) {
    projMeth <- "bilinear"
  } else {
    projMeth <- NULL
  }
  popReprojMeth <- process_raster(popRas, border, epsg, projMeth = projMeth)
  popReproj <- popReprojMeth[[1]]
  projMeth <- popReprojMeth[[2]]
  if (is.null(popReproj)) {
    stop_quietly("You exit the function.")
  }
  write(paste0(Sys.time(), ": Population raster cropped, masked and projected (", epsg, ") using the '", projMeth, "' method - From input folder: ", timeFolder), file = logTxt, append = TRUE)
  # Initial resolution
  if (is.null(changeRes)) {
    resInit <- terra::res(popReproj)
    yn <- utils::menu(c("YES", "NO"), title = paste("\nThe resolution of the population raster is", round(resInit[1], 2), "m. Would you like to modify it?"))
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
  } else if (changeRes) {
    resInit <- terra::res(popReproj)
    yn <- 1
  } else {
    yn <- 2
  }
  if (yn == 1) {
    if (is.null(newRes)) {
      cat("\nEnter the new resolution (m)\n")
      newRes <- readline(prompt = "Selection: ")
      newRes <- as.numeric(newRes)
      k <- 0
      while ((is.na(newRes) | newRes < 0) & k < 3) {
        message("Resolution must be a real positive number.")
        k <- k + 1
        newRes <- readline(prompt = "Selection: ")
        newRes <- as.numeric(newRes)
      }
      if ((is.na(newRes) | newRes < 0) & k == 3) {
        stop("Invalid resolution!")
      }
    }
    popReprojNew <- popReproj
    res(popReproj) <- newRes
    if (defaultMethods) {
      if (newRes[1] >= resInit[1]) {
        resampMeth <- "sum"
      } else {
        resampMeth <- "bilinear"
      }
    } else {
      resampMeth <- NULL
    }
    popFinalMeth <- terra::resample_raster(popReprojNew, popReproj, popRas, resampMeth)
    popFinal <- popFinalMeth[[1]]
    resampMeth <- popFinalMeth[[2]]
    write(paste0(Sys.time(), ": Population raster resampled using the '", resampMeth, "' method - From input folder: ", timeFolder), file = logTxt, append = TRUE)
  }else{
    popFinal <- popReproj
  }
  if (is.null(popCorrection)) {
    ynCorr <- utils::menu(c("YES", "NO"), title = "\nReprojecting a raster always causes some (small) distortion in the grid of a raster.\nWould you like to correct it (see 'help' for more details)?")
    if (ynCorr == 0) {
      stop_quietly("You exit the function.")
    }
  } else if (popCorrection) {
    ynCorr <- 1
  } else {
    ynCorr <- 2
  }
  if (ynCorr == 1) {
    if (is.null(gridRes)) {
      cat("\nEnter the resolution of the grid for the zonal statistic used for correcting the population raster (m)\n")
      gridRes <- readline(prompt = "Selection: ")
      gridRes <- as.numeric(gridRes)
      k <- 0
      while ((is.na(gridRes) | gridRes < 0) & k < 3) {
        message("Resultion must be a real positive number.")
        k <- k + 1
        gridRes <- readline(prompt = "Selection: ")
        gridRes <- as.numeric(gridRes)
      }
      if ((is.na(gridRes) | gridRes < 0) & k == 3) {
        stop("Invalid resolution!")
      }
    }
    # Transform border for following process
    border <- sf::st_transform(border, terra::crs(popFinal))
    grd <- sf::st_as_sf(sf::st_make_grid(border, cellsize = gridRes))
    # We don't do that, because then, partial cells are not pixelized with fasterize
    # So border areas may become NA, leading to a loss of population when multiplied by the zonalStat raster
    # grdInter <- gIntersection(gUnaryUnion(as(border, "Spatial")), as(grd, "Spatial"), byid = TRUE)
    # grdInterPoly <- st_cast(as(grdInter, "sf"), "MULTIPOLYGON")
    cat("\nSumming values of the original population raster per grid cell\n")
    popSum <- exactextractr::exact_extract(popRas, sf::st_transform(grd, terra::crs(popRas)), "sum")
    cat("\nSumming values of the processed population raster per grid cell before correction\n")
    popFinalSum <- exactextractr::exact_extract(popFinal, grd, "sum")
    # Ratio per grid cell
    grd$pop_diff <- popSum / popFinalSum
    # The only zones that are not going to be corrected are the ones that
    # initially had some population but that lost them with projection.
    # Ratio is infinite, which became NA in R.
    zonalStat  <- fasterize::fasterize(grd, as(popFinal, "Raster"), "pop_diff")
    popOut <- popFinal * as(zonalStat, "SpatRaster")
    sysTime <- Sys.time()
    outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
    popOutFolder <- paste0(gsub("raw", "processed", popFolder), "/", outTimeFolder)
    dir.create(popOutFolder, recursive = TRUE)
    raster::writeRaster(popOut, paste0(popOutFolder, "/rPopulation.tif"), overwrite=TRUE)
    write(paste0(Sys.time(), ": Population raster corrected using a grid of ", gridRes, " x ", gridRes, " m cells"), file = logTxt, append = TRUE)
    cat("\nSumming values of the processed population raster per grid cell after correction\n")          
    popOutSum <- exactextractr::exact_extract(popOut, grd, "sum")
    message(paste0("Mean difference per grid cell (", gridRes, " x ", gridRes, ") ", "before correction: ", round(mean(popFinalSum - popSum), 2)))
    message(paste0("Mean difference per grid cell (", gridRes, " x ", gridRes, ") ", "after correction: ", round(mean(popOutSum - popSum), 2)))
    write(paste0(Sys.time(), ": Mean difference per grid cell before correction: ", round(mean(popFinalSum - popSum), 2), " - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    write(paste0(Sys.time(), ": Mean difference per grid cell after correction: ", round(mean(popOutSum - popSum), 2), " - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    write(paste0(Sys.time(), ": Processed population raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
  } else {
    popOut <- popFinal
    sysTime <- Sys.time()
    outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
    popOutFolder <- paste0(gsub("raw", "processed", popFolder), "/", outTimeFolder)
    dir.create(popOutFolder, recursive = TRUE)
    terra::writeRaster(popOut, paste0(popOutFolder, "/rPopulation.tif"), overwrite=TRUE)
    write(paste0(Sys.time(), ": Processed population raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
  }
}