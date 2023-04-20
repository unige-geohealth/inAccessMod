#' Create A Health Facility Point Shapefile (HeRAMS)
#'
#' Create a point shapefile of health facilities based on a pre-processed HeRAMS health facility table obtained with the
#' \code{HeRAMS_filter_hf} function.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param mostRecentBoundaries logical; should the most recent processed boundary shapefile be used? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @param lonlat logical; are the coordinates indicated in the health facility table given in lon/lat?
#' @param epsg numeric or character (coerced to character); ESPG code - Coordinate systems worldwide (EPSG/ESRI)
#' @param rmNA logical; should the health facilities with non-available coordinates be removed? If NULL or FALSE the user is interactively
#' asked whether they want to remove them or not.
#' @param rmOut logical; should the health facilities falling outside of the country be removed? If NULL or FALSE the user is 
#' interactively asked whether they want to remove them or not.
#' @param scenario character; a string of three characters that correspond to the scenario folder suffix like '001', '002'...'010'...'099'...'100'
#' If NULL, the user is interactively asked to choose the scenario from the available ones.
#' @param nameCSV character; name of csv file WITHOUT extension corresponding to filtered facilities. If null, it will take the default name used in 
#' the HeRAMS_filter_hf function (health_facilities.csv). 
#' @details Once the missing coordinate issue is addressed, the function checks whether the health facilities fall within the
#' country boundary. There is a track record of both the facilities with missing coordinates and the ones that fall
#' outside the country boundary.
#' @export
HeRAMS_create_hf_shapefile <- function (mainPath, country, mostRecentBoundaries = TRUE, lonlat = TRUE, epsg = NULL, rmNA = NULL, rmOut = NULL, scenario = NULL, nameCSV = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
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
  if (!is.null(rmNA)) {
    if(!is.logical(rmNA)) {
      stop("rmNA must be 'NULL' or 'logical'")
    }
  } else {
    rmNA <- FALSE
  }
  if (!is.null(rmOut)) {
    if(!is.logical(rmOut)) {
      stop("rmOut must be 'NULL' or 'logical'")
    }
  } else {
    rmOut <- FALSE
  }
  pathFacilities <- file.path(mainPath, country, "data", "vFacilities")
  if (!dir.exists(pathFacilities)) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  if (!is.null(scenario)) {
    if(!is.character(scenario)){
      stop("If not NULL, scenario must be 'character'")
    }
    if(!grepl("[0-9]{3}", scenario)) {
      stop("If not NULL, scenario must contains three characters that correspond to the scenario folder suffix like '001', '002'...'010'...'099'...'100'")
    }
    if(!dir.exists(file.path(pathFacilities, "scenario", scenario))) {
      stop(paste0(pathFacilities, "/scenario", scenario, "does not exist"))
    }
  }
  if (is.null(nameCSV)) {
    nameCSV <- "health_facilities"
  }
  
  logTxt <- file.path(mainPath, country, "data", "log.txt")
  border <- get_boundaries(mainPath = mainPath, country = country, type = "raw", mostRecentBoundaries)
  scenarioDirs <- list.dirs(pathFacilities, recursive = FALSE)
  scenarioDirs <- scenarioDirs[grepl("scenario", scenarioDirs)]
  if (is.null(scenario)) {
    scenario <- select_scenario(scenarioDirs)
  } else {
    scenario <- paste0("scenario", scenario)
    scenario <- scenarioDirs[grepl(scenario, scenarioDirs)]
  }
  scenarioTime <- select_scenarioTime(scenario)
  hfFolder <- file.path(scenario, scenarioTime, "raw")
  filesCsv <- list.files(hfFolder)[grepl(paste0(nameCSV, ".csv$"), list.files(hfFolder))]
  if (length(filesCsv) == 0) {
    stop(paste(paste0(nameCSV, ".csv"), "is missing. Run the HeRAMS_filter_hf function first."))
  }
  multiMsg <- "Select the CSV table that you would like to process."
  if (length(filesCsv) > 1) {
    fileInd <- utils::menu(filesCsv, multiMsg)
    fi <- filesCsv[fileInd]
  }else{
    fi <- filesCsv
  }
  df <- read.csv(file.path(hfFolder, fi))
  
  colNamesHeRAMS <- inAccessMod::HeRAMS_parameters$print
  if (!all(unlist(colNamesHeRAMS) %in% colnames(df))) {
    message(paste("Check the column names of:", file.path(hfFolder, fi), "and change the parameters accordingly"))
    codeColumns <- unlist(set_HeRAMS_table_parameters(colNamesHeRAMS))
  }
  if (!all(codeColumns %in% colnames(df))) {
    stop_quietly("Invalid parameters!")
  }
  
  xy <- data.frame(Lon = df[, "GPS_002", drop = TRUE], Lat = df[, "GPS_001", drop = TRUE])
  if (nrow(xy[complete.cases(xy), ]) == 0) {
    stop_quietly(paste("Coordinates are not available! Add them manually in the CSV file:\n", file.path(hfFolder, fi)))
  }
  if (!all(complete.cases(xy))) {
    dfNA <- df[!complete.cases(xy), ]
    df <- df[complete.cases(xy), ]
    # Try with names (if not available => without)
    dfPrint <- tryCatch({dfNA[, codeColumns]}, error = function (e) NULL)
    cat("\n")
    if (is.null(dfPrint)) {
      dfPrint <- dfNA[, codeColumns[-which(names(codeColumns) == "name")]]
    }
    print(dfPrint)
    cat("\n")
    message(paste("Coordinates are missing the facilities printed above."))
    if (rmNA) {
      yn <- 2
    } else {
      yn <- utils::menu(c("Exit the script and add the coordinates manually in the CSV file", "Remove these HFs"), title = paste("\nWhat would you like to do?"))
    }
    if (yn == 1) {
      stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", file.path(hfFolder, fi)))
    } else {
      write.table(dfNA, file.path(hfFolder, paste0(nameCSV, "_coordinates_NA.txt")))
      message(paste("\nYou can access the removed HFs at:\n", file.path(hfFolder, paste0(nameCSV, "_coordinates_NA.txt")), "\n"))
    }
  }
  pts <- sp::SpatialPointsDataFrame(coords = xy[complete.cases(xy), ], data = df, proj4string = sp::CRS(epsg))
  border <- rgeos::gUnaryUnion(as(sf::st_transform(border, raster::crs(pts)), "Spatial"))
  border <- sp::spTransform(border, pts@proj4string)
  # Tolerance
  border <- suppressWarnings(rgeos::gBuffer(border, width = 0.2))
  # Intersection
  inter <- rgeos::gIntersects(border, pts, byid = TRUE)
  interOutside <- FALSE
  if (!all(inter[, 1])) {
    interOutside <- TRUE
    # Try with names (if not available => without)
    dfPrint <- tryCatch({df[!inter, codeColumns]}, error = function (e) NULL)
    if (is.null(dfPrint)) {
      dfPrint <- df[!inter, codeColumns[-which(names(codeColumns) == "name")]]
    }
    print(dfPrint)
    message("The facilities indicated above are outside the country boundaries")
    if (rmOut) {
      yn <- 2
    } else {
      yn <- utils::menu(c("Exit the script and correct the coordinates manually in the CSV file", "Remove these HFs and create a HFs' shapefile"), title = paste("\nWhat would you like to do?"))
    }
    if (yn == 1) {
      stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", file.path(hfFolder, fi)))
    }
  }
  if (interOutside) {
    write.table(df[!inter[, 1], ], file.path(hfFolder, paste0(nameCSV, "_coordinates_outside.txt")))
    message(paste("\nYou can access the removed HFs at:\n", file.path(hfFolder, paste0(nameCSV, "_coordinates_outside.txt")), "\n"))
  }
  # shp <- pts[inter[, 1], -1]
  shp <- sf::st_as_sf(pts[inter[, 1], -1])
  cat("Saving the HFs' shapefile...\n")
  sf::st_write(shp, file.path(hfFolder, paste0(nameCSV, ".shp")), append = FALSE)
  inputFolder <- stringr::str_extract(hfFolder, "scenario[0-9]{3}/[0-9]{14}")
  write(paste0(Sys.time(), ": Health facility shapefile created - Input folder: ", inputFolder), fi = logTxt, append = TRUE)
}
