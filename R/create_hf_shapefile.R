#' Create a health facility shapefile
#'
#' Create a point shapefile of health facilities based on a pre-processed HeRAMS health facility table obtained with the
#' \code{filter_hf()}.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param mostRecentBoudaries logical; should the most recent processed boundary shapefile be used? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @param lonlat logical; are the coordinates indicated in the health facility table given in lon/lat?
#' @param epsg numeric or character (coerced to character); ESPG code - Coordinate systems worldwide (EPSG/ESRI).
#' @export
create_hf_shapefile <- function (mainPath, region, mostRecentBoundaries = TRUE, lonlat = TRUE, epsg = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(lonlat)) {
    stop("lonlat must be 'logical'")
  }
  if (lonlat) {
    epsg <- "EPSG:4326"
  } else {
    if (is.null(epsg)) {
      stop("If lonlat = FALSE, epsg is required.")
    } else {
      epsg <- as.character(epsg)
      validEPSG <- crsuggest::crs_sf$crs_code[!is.na(crsuggest::crs_sf$crs_units)]
      if (!epsg %in% validEPSG) {
        stop("EPSG not valid.")
      }
      else {
        epsg <- paste0("EPSG:", epsg)
      }
    }
  }
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  border <- get_boundaries(mainPath = mainPath, region = region, type = "processed", mostRecentBoundaries)
  subProjDirs <- list.dirs(pathFacilities, recursive = FALSE)
  subProjDirs <- subProjDirs[grepl("subProj", subProjDirs)]
  if (length(subProjDirs) == 0) {
    stop("Filtered health facility table is missing. Run the filter_hf function.")
  }
  subProj <- stringr::str_extract(subProjDirs, "subProj[0-9]{3}$")
  if (length(subProj) > 1) {
    subProj <- c(subProj, "VIEW")
    subInd <- utils::menu(subProj, title = "Select the sub-project or the VIEW option to see the selected HFs for each sub-project.")
    while (subInd == length(subProj)) {
      for (i in 1:(length(subProj)-1)) {
        message(subProj[i])
        cat(paste(readLines(paste(pathFacilities, subProj[i], "selected_hf.txt", sep = "/")), collapse = "\n"))
        readline(prompt="Press [enter] to continue")
      }
      subInd <- utils::menu(subProj, title = "Select the sub-project or the VIEW option to see the selected HFs for each sub-project.")
    }
    subProj <- subProjDirs[subInd]
  }
  subProjTime <- list.dirs(subProj, recursive = FALSE)
  subProjTime <- subProjTime[grepl("[0-9]{14}", subProjTime)]
  subProjTime <- stringr::str_extract(subProjTime, "[0-9]{14}")
  subProjTimeForm <- paste0(substr(subProjTime, 1, 4), "-", substr(subProjTime, 5, 6), "-", substr(subProjTime, 7, 8), " ", substr(subProjTime, 9, 10), ":", substr(subProjTime, 11, 12), ":", substr(subProjTime, 13, 14), " CEST")
  if (length(subProjTime) > 1) {
    subProjTimeForm <- c(subProjTimeForm, "VIEW")
    subInd <- utils::menu(subProjTimeForm, title = "Select the filtered table time creation or the VIEW option to see the selection parameters for each time.")
    while (subInd == length(subProjTimeForm)) {
      for (i in 1:(length(subProjTimeForm)-1)) {
        message(subProjTimeForm[i])
        cat(paste(readLines(paste(subProj, subProjTime[i], "time_frame.txt", sep = "/")), collapse = "\n"))
        readline(prompt="Press [enter] to continue")
      }
      subInd <- utils::menu(subProjTimeForm, title = "Select the filtered table time creation or the VIEW option to see the selection parameters for each time.")
    }
    subProjTime <- subProjTime[subInd]
  }
  hfFolder <- paste(subProj, subProjTime, "raw", sep = "/")
  filesCsv <- list.files(hfFolder)[grepl("\\.csv$", list.files(hfFolder))]
  multiMsg <- "Select the CSV table that you would like to process."
  if (length(filesCsv) > 1) {
    fileInd <- utils::menu(filesCsv, multiMsg)
    file <- filesShp[fileInd]
  }else{
    file <- filesCsv
  }
  df <- read.csv(paste(hfFolder, file, sep = "/"))
  xy <- data.frame(Lat = df[, "MoSDGPS_SQ002", drop = TRUE], Lon = df[, "MoSDGPS_SQ001", drop = TRUE])
  if (nrow(xy[complete.cases(xy), ]) == 0) {
    stop_quietly(paste("Coordinates are not available! Add them manually in the CSV file:\n", paste(hfFolder, file, sep = "/")))
  }
  if (!all(complete.cases(xy))) {
    message(paste("Coordinates are missing for the following facilities:"))
    cat("\n")
    dfNA <- df[!complete.cases(xy), ]
    print(dfNA[, c("external_id", "workspace_id", "date", "MoSD3", "HFNAME")])
    yn <- utils::menu(c("Exit the script and add the coordinates manually in the CSV file", "Remove these HFs"), title = paste("\nWhat would you like to do?"))
    if (yn == 1) {
      stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", paste(hfFolder, file, sep = "/")))
    } else {
      write.table(dfNA, paste(hfFolder, "coordinates_NA.txt", sep = "/"))
      message(paste("\nYou can access the removed HFs at:\n", paste(hfFolder, "coordinates_NA.txt", sep = "/")))
    }
  }
  pts <- sp::SpatialPointsDataFrame(coords = xy[complete.cases(xy), ], data = df[complete.cases(xy), ], proj4string = terra::crs(epsg))
  border <- rgeos::gUnaryUnion(as(sf::st_transform(border, terra::crs(pts)), "Spatial"))
  inter <- rgeos::gIntersects(border, pts, byid = TRUE)
  interOutside <- FALSE
  if (!all(inter[, 1])) {
    interOutside <- TRUE
    message("The follwing HFs are outside the region/country boundaries:")
    print(df[!inter, c("external_id", "workspace_id", "date", "MoSD3", "HFNAME")])
    yn <- utils::menu(c("Exit the script and correct the coordinates manually in the CSV file", "Remove these HFs and create a HFs' shapefile"), title = paste("\nWhat would you like to do?"))
    if (yn == 1) {
      stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", paste(hfFolder, file, sep = "/")))
    }
  }
  shp <- sf::st_as_sf(pts[inter[, 1], c("external_id", "workspace_id", "date", "MoSD3", "HFNAME")])
  cat("\nSaving the HFs' shapefile...")
  sf::st_write(shp, paste(hfFolder, "health_facilities.shp", sep = "/"), append = FALSE)
  inputFolder <- stringr::str_extract(hfFolder, "subProj[0-9]{3}/[0-9]{14}")
  write(paste0(Sys.time(), ": Health facility shapefile created - Input folder: ", inputFolder), file = logTxt, append = TRUE)
  if (interOutside) {
    write.table(df[!inter[, 1]], paste(hfFolder, "coordinates_outside.txt", sep = "/"))
    message(paste("\nYou can access the removed HFs at:\n", paste(hfFolder, "coordinates_outside.txt", sep = "/")))
  }
}
