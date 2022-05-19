#' Process Input Layers
#' 
#' Process any input layer and copy it to its corresponding folder
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param selectedInputs character; vector indicating the inputs to be processed. Raw inputs must be available. Argument
#' can be set to "All" to consider all the available 'raw' inputs. If NULL, the user is interactively asked to select the available
#' inputs to be processed.
#' @param mostRecent logical; should the most recent input be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on date and time.
#' @param alwaysProcess logical; should always the input be processed? If alwaysProcess = FALSE and if the input has already
#' been processed, the user is interactively asked whether they want to process it or not.
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
#' @details A 'processed' boundary shapefile is required for processing any other inputs. A 'processed' population raster is required
#' for processing any other raster. These conditions are taken into account and the processing of these
#' layers is performed even if they are not selected, if 'processed' layers are not available.
#' @export
process_inputs <- function (mainPath, region, selectedInputs = NULL, mostRecent = FALSE, alwaysProcess = FALSE, defaultMethods = NULL, changeRes = NULL, newRes = NULL, popCorrection = NULL, gridRes = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  rawFolders <- check_input(mainPath, region, "raw")
  if (length(rawFolders) == 0) {
    stop("No input data available.")
  }
  if (!all(is.character(selectedInputs))) {
    stop("selectedInputs must be 'character'")
  } else {
    if (!all(selectedInputs %in% c("All", rawFolders))) {
      stop(paste0("selectedInputs (", c(selectedInputs), ") must be available"))
    }
  }
  if (!is.logical(mostRecent)) {
    stop("mostRecent must be 'logical'")
  }
  if (!is.logical(alwaysProcess)) {
    stop("alwaysProcess must be 'logical'")
  }
  if (!is.null(changeRes)) {
    if (!is.logical(changeRes)) {
      stop("mostRecent must be NULL or 'logical'")
    }
  }
  if (!is.null(defaultMethods)) {
    if (!is.logical(changeRes)) {
      stop("defaultMethods must be NULL or 'logical'")
    }
  } else {
    defaultMethods <- FALSE
  }
  if (!is.null(newRes)) {
    if (!is.numeric(newRes) & newRes > 0) {
      stop("newRes must be NULL or a real positive number'")
    }
  }
  if (!is.null(popCorrection)) {
    if (!is.logical(popCorrection)) {
      stop("popCorrection must be NULL or 'logical'")
    }
  }
  if (!is.null(gridRes)) {
    if (!is.numeric(gridRes) & newRes > 0) {
      stop("gridRes must be NULL or a real positive number'")
    }
  }
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  epsg <- get_param(mainPath = mainPath, region = region, "EPSG")
  epsg <- paste0("EPSG:", epsg)
  if (length(epsg) == 0) {
    stop("EPSG for projection is not set. Run the set_projection function.")
  }
  if (selectedInputs == "All") {
    selectedFolders <- rawFolders
  } else if (is.null(selectedInputs)) {
    selectedFolders <- select_folder(rawFolders, "Enter all the indices that correspond to the inputs you want to process.\nOn the same line separated by a space, or just skip to select all inputs.")
  } else {
    selectedFolders <- selectedInputs
  }
  # Border is required for any input processing; if wanted, first process this layer
  borderPath <- paste0(mainPath, "/", region, "/data/vBorders")
  borderPr <- check_exists(borderPath, "processed", layer = TRUE)
  # If we want to process border or if no processed shapefile exists (required for any other processing)
  if (("vBorders" %in% selectedFolders) | is.null(borderPr)) {
    borderFolders <- check_exists(borderPath, "raw", layer = TRUE)
    if (is.null(borderFolders)) {
      stop("\nBoundary shapefile is required for input processing. Run the download_boundaries function.")
    }
    message("\nLoading raw shapefile of boundaries...")
    timeFolder <- choose_input(borderFolders, "Shapefile downloaded at", mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    }
    borderFolder <- paste0(borderPath, "/", timeFolder, "/raw")
    toProcess <- already_processed(borderFolder, alwaysProcess)
    if (toProcess) {
      multipleFilesMsg <- "Select the boundary shapefile that you would like to use."
      border <- load_layer(borderFolder, multipleFilesMsg)[[2]]
      border <- sf::st_transform(border, sf::st_crs(epsg))
      write(paste0(Sys.time(), ": vBorders shapefile projected (", epsg, ") - From input folder: ", timeFolder), file = logTxt, append = TRUE)
      sysTime <- Sys.time()
      outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      borderOutFolder <- paste0(gsub("raw", "processed", borderFolder), "/", outTimeFolder)
      dir.create(borderOutFolder, recursive = TRUE)
      st_write(border, paste0(borderOutFolder, "/vBorders.shp"), append=FALSE)
      write(paste0(Sys.time(), ": Processed vBorders shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
      selectedFolders <- selectedFolders[!grepl("vBorders", selectedFolders)]
    }
  }
  if (length(selectedFolders) < 1) {
    stop_quietly("No more input to be processed!")
  }
  message("\nLoading processed boundary shapefile...")
  border <- get_boundaries(mainPath, region, "processed", mostRecent)
  if ("rPopulation" %in% selectedFolders) {
    process_pop(mainPath, region, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes)
    selectedFolders <- selectedFolders[!grepl("rPopulation", selectedFolders)]
  }
  # Check if other rasters to be processed
  filesRasTrue <- NULL
  for (i in 1:length(selectedFolders)) {
    files <- list.files(paste0(mainPath, "/", region, "/data/", selectedFolders[i]), recursive = TRUE)
    filesRasTrue <- c(filesRasTrue, any(grepl("raw/.*\\.tif",files)))
  }
  if (any(filesRasTrue)) {
    popFolder <- paste0(mainPath, "/", region, "/data/rPopulation")
    popFolders <- check_exists(popFolder, "processed", layer = TRUE)
    if (is.null(popFolders)) {
      message("\nNo processed population raster is available.\nProcessing raw population raster...")
      process_pop(mainPath, region, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes)
      popFolders <- check_exists(popFolder, "processed", layer = TRUE)
    }
    message("\nLoading processed population raster...")
    timeFolder <- choose_input(popFolders, "Population raster processed at:", mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    } else {
      popFolderLst <- list.dirs(popFolder)
      popFolder <- popFolderLst[grepl(paste0("processed/", timeFolder), popFolderLst)]
      multipleFilesMsg <- "Select the population raster that you would like to process."
      popOut <- load_layer(popFolder, multipleFilesMsg)[[1]]
    }
  }
  for (i in 1:length(selectedFolders)) {
    cat("\n")
    message(selectedFolders[i])
    inputFolder <- paste0(mainPath, "/", region, "/data/", selectedFolders[i])
    inputFolders <- check_exists(inputFolder, "raw", layer = TRUE)
    timeFolder <- choose_input(inputFolders, paste(selectedFolders[i], "downloaded at:"), mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    }
    inputFolder <- paste0(inputFolder, "/", timeFolder, "/raw")
    processLayer <- already_processed(inputFolder, alwaysProcess)
    if (!processLayer) {
      if (i == length(selectedFolders)) {
        stop_quietly("No more input to be processed!")
      }else{
        next
      }
    }
    multipleFilesMsg <- "Select the input that you want to process."
    inputLayers <- load_layer(inputFolder, multipleFilesMsg)
    if (!is.null(inputLayers[[1]])) {
      if (defaultMethods) {
        if (grepl("Land|land", selectedFolders[i])) {
          projMeth <- "near"
          resampMeth <- "near"
        } else {
          projMeth <- "bilinear"
          resampMeth <- "bilinear"
        }
      } else {
        projMeth <- NULL
        resampMeth <- NULL
      }
      rasReprojMeth <- process_raster(inputLayers[[1]], border, epsg, projMeth)
      rasReproj <- rasReprojMeth[[1]]
      projMeth <- rasReprojMeth[[2]]
      write(paste0(Sys.time(), ": ", selectedFolders[i], " raster cropped, masked and projected using the '", projMeth, "' method - From input folder: ",timeFolder), file = logTxt, append = TRUE)
      rasResampledMeth <- resample_raster(rasReproj, popOut, inputLayers[[1]], resampMeth)
      rasResampled <- rasResampledMeth[[1]]
      resampMeth <- rasResampledMeth[[2]]
      write(paste0(Sys.time(), ": ", selectedFolders[i], " raster resampled using the '", resampMeth, "' method - From input folder: ",timeFolder), file = logTxt, append = TRUE)
      sysTime <- Sys.time()
      outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      outFolder <- paste0(gsub("raw", "processed", inputFolder), "/", outTimeFolder)
      dir.create(outFolder, recursive = TRUE)
      writeRaster(rasResampled, paste0(outFolder, "/", selectedFolders[i], ".tif"), overwrite=TRUE)
      write(paste0(Sys.time(), ": Processed ", selectedFolders[i], " raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    }
    if (!is.null(inputLayers[[2]])) {
      shpProcessed <- process_shapefile(inputLayers[[2]], border, epsg, selectedFolders[i])
      write(paste0(Sys.time(), ": ", selectedFolders[i], " shapefile projected and clipped - From input folder: ", timeFolder), file = logTxt, append = TRUE)
      sysTime <- Sys.time()
      outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      outFolder <- paste0(gsub("raw", "processed", inputFolder), "/", outTimeFolder)
      dir.create(outFolder, recursive = TRUE)
      st_write(shpProcessed, paste0(outFolder, "/", selectedFolders[i], ".shp"), append=FALSE)
      write(paste0(Sys.time(), ": Processed ", selectedFolders[i], " shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    }
  }
  cat("\nDone!\n")
}