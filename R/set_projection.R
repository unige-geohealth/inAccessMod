#' Set Projection
#'
#' Set the projected coordinate reference system of the project based on the extent of the boundary shapefile.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param mostRecent logical; should the most recent downloaded boundary shapefile be selected? If FALSE and if there 
#' are multiple available inputs, the user is interactively asked to select the input based on date and time.
#' @param alwaysSet logical; should the projected coordinate reference system always be set, even if it has already been 
#' set? If FALSE and if the projected coordinate reference system has already been set the user is 
#' interactively asked whether they want to set it again or not.
#' @param bestCRS logical; should the projected coordinate reference system be set automatically based on the "best-fit" 
#' projected coordinate reference system? If FALSE, the user is interactively asked to select the projected coordinate reference 
#' system from a list of suitable reference systems.
#' @details The "best-fit" and the suitable projected coordinate reference system are obtained with the 
#' \code{suggest_top_crs} and the \code{suggest_crs}, respectively, from the \pkg{crsuggest} package.
#' Both function work by analyzing the extent of the spatial dataset and comparing it to the area extents
#' in the EPSG's coordinate reference system database.
#' @export
set_projection <- function (mainPath, region, mostRecent = FALSE, alwaysSet = FALSE, bestCRS = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  message("\nSuitable coordinate reference systems based on boundaries")
  # Write the EPSG in the config.txt file
  fileConn=file(paste0(mainPath, "/", region, "/data/config.txt"), open = "r")
  configTxt <- readLines(fileConn)
  close(fileConn)
  if (any(grepl(paste0("EPSG:"), configTxt))) {
    if (!alwaysSet) {
      yn <- utils::menu(c("YES", "NO"), title = paste("\nThe projected coordinate system has already been set. Would you like to modify it?"))
      if (yn == 0) {
        stop_quietly("You exit the function.")
      }
      if (yn == 2) {
        epsg <- get_param(mainPath, region, "EPSG")
        stop_quietly(paste("EPSG previously set:", epsg))
      }
    } else {
      epsg <- get_param(mainPath, region, "EPSG")
      message(paste("EPSG previously set:", epsg))
    }
  }
  # Get the admin boundaries
  pathBorder <- paste0(mainPath, "/", region, "/data/vBorders")
  if (!dir.exists(pathBorder)) {
    stop(paste(pathBorder,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  folders <- check_exists(pathBorder, "raw", layer = TRUE)
  if (is.null(folders)) {
    stop("Raw boundary shapefile is missing.")
  } else {
    timeFolderBound <- select_input(folders, "Shapefile downloaded at", mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    } else {
      boundFolder <- paste0(pathBorder, "/", timeFolderBound, "/raw/")
      multipleFilesMsg <- "Select the boundary shapefile that you would like to use."
      message(paste("Loading raw boundaries..."))
      border <- load_layer(boundFolder, multipleFilesMsg)[[2]]
    }
  } 
  validEPSG <- crsuggest::crs_sf$crs_code[!is.na(crsuggest::crs_sf$crs_units) & crsuggest::crs_sf$crs_units=="m" & crsuggest::crs_sf$crs_type == "projected"]
  best <- crsuggest::suggest_top_crs(border)
  # Select projection
  if (bestCRS) {
    epsg <- best
  } else {
    suggestedCRS <- tryCatch({crsuggest::suggest_crs(input = border, type = "projected", limit = 100, units = "m")}, error=function(e){NULL})
    if (is.null(suggestedCRS)) {
      cat("\nNo reference system can be suggested. Enter the EPSG code that you want to use for this project.")
      cat("\n ")
      selInd <- readline(prompt = "Selection: ")
      epsg <- as.numeric(selInd)
      if (!epsg %in% validEPSG) {
        stop("EPSG not valid !")
      }
    } else {
      suggestedCRS <- paste(paste("EPSG:", suggestedCRS$crs_code), gsub(" .*$", "", suggestedCRS$crs_proj4))
      suggestedCRS <- c(suggestedCRS, "Other")
      cat(paste("\nEPSG:", best, "seems to be the best projected coordinate reference for this region/country.\n"))
      valid <- FALSE
      while (!valid) {
        selectedProj <- utils::menu(suggestedCRS, title = "Select projection for this project", graphics=TRUE)
        if (selectedProj == 0 | !exists("selectedProj")) {
          stop_quietly("You exit the function.")
        }
        if (selectedProj == length(suggestedCRS)) {
          cat("\n\nEnter the EPSG code that you want to use for this project.")
          cat("\n ")
          selInd <- readline(prompt = "Selection: ")
          epsg <- selInd
          if (!epsg %in% validEPSG) {
            message("EPSG not valid !")
            valid <- FALSE
          }else{
            valid <- TRUE
          }
        }else{
          epsg <- unlist(str_split(string = suggestedCRS[selectedProj], pattern = " "))[2]
          valid <- TRUE
        }
      }
    }
  }
  # Write the EPSG in the config.txt file
  fileConn = file(paste0(mainPath, "/", region, "/data/config.txt"), open = "r")
  configTxt <- readLines(fileConn)
  close(fileConn)
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  if(any(grepl(paste0("EPSG:"), configTxt))){
    epsgOld <- get_param(mainPath, region, "EPSG")
    if (epsgOld == epsg) {
      stop_quietly("\nNew projection equal to the one previously set. No change has been made.")
    }
    newValues <- gsub("EPSG:.*", paste0("EPSG:", epsg), configTxt)
    fileConn <- file(paste0(mainPath, "/", region, "/data/config.txt"), open = "w")
    writeLines(newValues, fileConn)
    close(fileConn)
    write(paste0(Sys.time(), ": Projection parameter changed (", epsg, ")"), file = logTxt, append = TRUE)
    warning("\nProjection parameter had already been set and has been changed. Inputs might have to be processed again.")
  }else{
    write(paste0("EPSG:", epsg), file = paste0(mainPath, "/", region, "/data/config.txt"), append = TRUE)
    write(paste0(Sys.time(), ": Projection parameter set (", epsg, ")"), file = logTxt, append = TRUE)
  }
  # Project the boundary shapefile
  message("\nProjecting the boundary shapefile...")
  border <- sf::st_transform(border, sf::st_crs(paste0("EPSG:", epsg)))
  write(paste0(Sys.time(), ": vBorders shapefile projected (", paste0("EPSG:", epsg), ") - From input folder: ", timeFolder), file = logTxt, append = TRUE)
  sysTime <- Sys.time()
  outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  borderOutFolder <- paste0(gsub("raw", "processed", boundFolder), "/", outTimeFolder)
  dir.create(borderOutFolder, recursive = TRUE)
  st_write(border, paste0(borderOutFolder, "/vBorders.shp"), append=FALSE)
  write(paste0(Sys.time(), ": Processed vBorders shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
  message("\nProjection parameter has been set and the boundary shapefile has been projected.")
}