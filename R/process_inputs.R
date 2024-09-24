#' Process Input Layers
#' 
#' Process any input layer and copy it to its corresponding folder
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param selectedInputs character; vector indicating the inputs to be processed. Raw inputs must be available. Argument
#' can be set to "All" to consider all the available 'raw' inputs. If NULL, the user is interactively asked to select the available
#' inputs to be processed.
#' @param mostRecent logical; should the most recent input be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @param alwaysProcess logical; should always the input be processed? If alwaysProcess = FALSE and if the input has already
#' been processed, the user is interactively asked whether they want to process it or not.
#' @param defaultMethods logical; should be the default methods be used for projecting and resampling, respectively. For the
#' population raster, these are the 'bilinear' method for projecting and the 'sum' or the 'bilinear' for the resampling, 
#' depending on if the new resolution is lower or higher. For the landcover raster, the 'near' method is used for both the 
#' projection and resampling. For the the DEM, the 'bilinear' method is used for both the projection and resampling. If FALSE, 
#' the user is interactively asked to choose the methods from a list of options.
#' @param changeRes logical; does the user want to change the raster resolution of the population raster? If NULL, the resolution 
#' is printed and it is interactively asked the user if they want to change it. IF FALSE, there is no resampling.
#' @param newRes numeric; new resolution in meters. Ignored if the changeRes is FALSE. If NULL and if \code{changeRes} is TRUE,
#' the user is interactively asked to provide the new resolution.
#' @param popCorrection logical; should the raster correction algorithm be run. If it is NULL, the user is interactively asked
#' whether they want to run it or not.
#' @param gridRes numeric; the resolution (meters) of the grid shapefile used for correcting the raster. Ignored if \code{popCorrection} is FALSE.
#' If NULL and \code{popCorrection} is TRUE, the user is interactively asked to provide the grid resolution.
#' @param allowInteractivity logical; if TRUE, the user can choose a label for each processed layer; if FALSE, label default is used ('pr')
#' @param testMode logical; used for testing. If TRUE labels of processed inputs are not interactively asked.
#' @details A 'processed' boundary shapefile is required for processing any other inputs. A 'processed' population raster is required
#' for processing any other raster. These conditions are taken into account and the processing of these
#' layers is performed even if they are not selected and if 'processed' layers are not available.
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
#' set_projection(mainPath, location, mostRecent = TRUE, alwaysSet = TRUE, bestCRS = TRUE)
#' download_landcover(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)
#' download_population(mainPath, location, alwaysDownload = TRUE)
#' download_dem(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)
#' download_osm(mainPath, location, type = "roads", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(mainPath, location, type = "waterLines", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(mainPath, location, type = "waterPolygons", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' process_inputs(mainPath, location, selectedInputs = "All", mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = FALSE, popCorrection = TRUE, gridRes = 3000)}
#' @export
process_inputs <- function (mainPath, location, selectedInputs = NULL, mostRecent = FALSE, 
                            alwaysProcess = FALSE, 
                            defaultMethods = NULL, 
                            changeRes = NULL, 
                            newRes = NULL, 
                            popCorrection = NULL, 
                            gridRes = NULL,
                            allowInteractivity = TRUE,
                            testMode = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  # Load the available raw input paths
  rawFolders <- check_inputs(mainPath, location, "raw", onlyPrint = FALSE)
  if (length(rawFolders) == 0) {
    stop("No input data available.")
  }
  if (!is.null(selectedInputs)) {
    if (!all(is.character(selectedInputs))) {
      stop("selectedInputs must be 'character'")
    } else {
      if (!all(selectedInputs %in% c("All", rawFolders))) {
        stop(paste0("selectedInputs (", paste(selectedInputs, collapse = ", "), ") must be available"))
      }
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
      stop("changeRes must be NULL or 'logical'")
    }
  }
  if (!is.null(defaultMethods)) {
    if (!is.logical(defaultMethods)) {
      stop("defaultMethods must be NULL or 'logical'")
    }
  } else {
    defaultMethods <- FALSE
  }
  if (!is.null(newRes)) {
    if (!(is.numeric(newRes) & newRes > 0)) {
      stop("newRes must be NULL or a real positive number'")
    }
  }
  if (!is.null(popCorrection)) {
    if (!is.logical(popCorrection)) {
      stop("popCorrection must be NULL or 'logical'")
    }
  }
  if (!is.null(gridRes)) {
    if (!is.numeric(gridRes) & gridRes > 0) {
      stop("gridRes must be NULL or a real positive number'")
    }
  }
  if (!is.logical(allowInteractivity)) {
    stop("allowInteractivity must be 'logical'")
  }
  logTxt <- paste0(mainPath, "/", location, "/data/log.txt")
  epsg <- get_param(mainPath = mainPath, location = location, "EPSG")
  if (length(epsg) == 0) {
    stop("CRS is missing. Run the set_projection.")
  }
  epsg <- paste0("EPSG:", epsg)
  if (length(epsg) == 0) {
    stop("EPSG for projection is not set. Run the set_projection function.")
  }
  if ("All" %in% selectedInputs) {
    selectedFolders <- rawFolders
  } else if (is.null(selectedInputs)) {
    selectedFolders <- select_folder(rawFolders, "Enter all the indices that correspond to the inputs you want to process.\nOn the same line separated by a space, or just skip to select all inputs.")
  } else {
    selectedFolders <- selectedInputs
  }
  
  # Check whether we have rasters to be processed
  filesRasTrue <- NULL
  for (i in 1:length(selectedFolders)) {
    files <- list.files(file.path(mainPath, location, "data", selectedFolders[i]), recursive = TRUE)
    filesRasTrue <- c(filesRasTrue, any(grepl("raw/.*\\.tif",files)))
  }
  # If so, we require the processed boundaries
  if (any(filesRasTrue)) {
    # Border is required for raster processing; if wanted, first process this layer
    borderPath <- file.path(mainPath, location, "data", "vBorders")
    borderPr <- check_exists(borderPath, "processed", layer = TRUE)
    # If we want to process border or if no processed shapefile exists (required for any other processing)
    if (("vBorders" %in% selectedFolders) | is.null(borderPr)) {
      borderFolders <- check_exists(borderPath, "raw", layer = TRUE)
      if (is.null(borderFolders)) {
        stop("\nBoundary shapefile is required for raster processing. Run the download_boundaries function.")
      }
      message("\nLoading raw shapefile of boundaries...")
      timeFolder <- select_input(borderFolders, "Shapefile timestamped at", mostRecent)
      if (is.null(timeFolder)) {
        stop_quietly("You exit the function.")
      }
      borderFolder <- file.path(borderPath, timeFolder, "raw")
      toProcess <- to_process(borderFolder, alwaysProcess)
      if (toProcess) {
        multipleFilesMsg <- "Select the boundary shapefile that you would like to use."
        border <- load_layer(borderFolder, multipleFilesMsg)[[2]]
        border <- sf::st_transform(border, sf::st_crs(epsg))
        write(paste0(Sys.time(), ": vBorders shapefile projected (", epsg, ") - From input folder: ", timeFolder), file = logTxt, append = TRUE)
        outTimeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
        borderOutFolder <- file.path(gsub("raw", "processed", borderFolder), outTimeFolder)
        check_path_length(borderOutFolder)
        dir.create(borderOutFolder, recursive = TRUE)
        if (testMode | !allowInteractivity) {
          label <- "pr"
        } else {
          label <- readline(prompt = "Enter a label for vBorders: ")
        }
        check_path_length(file.path(borderOutFolder, paste0("vBorders", "_", label,".shp")))
        sf::st_write(border, file.path(borderOutFolder, paste0("vBorders", "_", label,".shp")), append=FALSE)
        write(paste0(Sys.time(), ": Processed vBorders shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
      }
      selectedFolders <- selectedFolders[!grepl("vBorders", selectedFolders)]
      border <- get_boundaries(mainPath, location, "processed", mostRecent)
      # If we don't need/want to process the boundary shapefile, load it
    } else {
      border <- get_boundaries(mainPath, location, "processed", mostRecent)
    }
    # If we want to process the population raster
    if ("rPopulation" %in% selectedFolders) {
      process_pop(mainPath, location, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes, alwaysProcess, allowInteractivity, testMode)
      selectedFolders <- selectedFolders[!grepl("rPopulation", selectedFolders)]
      # Check if other inputs to be processed
      if (length(selectedFolders) < 1) {
        stop_quietly("No more input to be processed!")
      }
      # Check whether we still have rasters to be processed
      filesRasTrue <- NULL
      for (i in 1:length(selectedFolders)) {
        files <- list.files(file.path(mainPath, location, "data", selectedFolders[i]), recursive = TRUE)
        filesRasTrue <- c(filesRasTrue, any(grepl("raw/.*\\.tif",files)))
      }
    }
    # filesRasTrue might have changed; if still TRUE, load a processed population raster
    if (any(filesRasTrue)) {
      # Population is required for raster processing
      popFolder <- file.path(mainPath, location, "data", "rPopulation")
      popFolders <- check_exists(popFolder, "processed", layer = TRUE)
      if (is.null(popFolders)) {
        message("\nNo processed population raster is available.\nProcessing raw population raster...")
        process_pop(mainPath, location, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes, alwaysProcess, allowInteractivity, testMode)
        popFolders <- check_exists(popFolder, "processed", layer = TRUE)
      }
      message("\nLoading processed population raster...")
      timeFolder <- select_input(popFolders, "Population raster processed at:", mostRecent)
      if (is.null(timeFolder)) {
        stop_quietly("You exit the function.")
      } else {
        popFolderLst <- list.dirs(popFolder)
        popFolder <- popFolderLst[grepl(paste0("processed/", timeFolder), popFolderLst)]
        multipleFilesMsg <- "Select the population raster that you would like to process."
        popOut <- load_layer(popFolder, multipleFilesMsg)[[1]]
      }
    }
  }
  
  for (i in 1:length(selectedFolders)) {
    cat("\n")
    message(selectedFolders[i])
    inputFolder <- file.path(mainPath, location, "data", selectedFolders[i])
    inputFolders <- check_exists(inputFolder, "raw", layer = TRUE)
    timeFolder <- select_input(inputFolders, paste(selectedFolders[i], "timestamped at:"), mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    }
    inputFolder <- file.path(inputFolder, timeFolder, "raw")
    processLayer <- to_process(inputFolder, alwaysProcess)
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
      outTimeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
      outFolder <- file.path(gsub("raw", "processed", inputFolder), outTimeFolder)
      check_path_length(outFolder)
      dir.create(outFolder, recursive = TRUE)
      if (testMode | !allowInteractivity) {
        label <- "pr"
      } else {
        label <- readline(prompt = paste0("Enter a label for ", selectedFolders[i], ": "))
      }
      check_path_length(file.path(outFolder, paste0(selectedFolders[i], "_", label, ".tif")))
      terra::writeRaster(rasResampled, file.path(outFolder, paste0(selectedFolders[i], "_", label, ".tif")), overwrite=TRUE)
      write(paste0(Sys.time(), ": Processed ", paste0(selectedFolders[i], "_", label), " raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    }
    if (!is.null(inputLayers[[2]])) {
      shpProcessed <- process_shapefile(inputLayers[[2]], epsg, selectedFolders[i])
      write(paste0(Sys.time(), ": ", selectedFolders[i], " shapefile projected and clipped - From input folder: ", timeFolder), file = logTxt, append = TRUE)
      outTimeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
      outFolder <- paste0(gsub("raw", "processed", inputFolder), "/", outTimeFolder)
      check_path_length(outFolder)
      dir.create(outFolder, recursive = TRUE)
      if (testMode | !allowInteractivity) {
        label <- "pr"
      } else {
        label <- readline(prompt = paste0("Enter a label for ", selectedFolders[i], ": "))
      }
      shpName <- paste0(selectedFolders[i], "_", label, ".shp")
      # In case we have scenario for HeRAMS data
      shpName <- gsub("/scenario[0-9]{3}", "", shpName)
      check_path_length(file.path(outFolder, shpName))
      sf::st_write(shpProcessed, file.path(outFolder, shpName), append=FALSE)
      write(paste0(Sys.time(), ": Processed ", shpName, " shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    }
  }
  cat("\nDone !\n")
  return(TRUE)
}
