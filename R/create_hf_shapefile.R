#' Create A Health Facility Point Shapefile
#'
#' Create a point shapefile of health facilities based on a pre-processed HeRAMS health facility table obtained with the
#' \code{filter_hf} function.
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
#' @param nameCSV character; name of csv file WITH extension corresponding to filtered facilities. If null, it will take the default name used in 
#' the filter_hf function (health_facilities.csv). 
#' @details Once the missing coordinate issue is addressed, the function checks whether the health facilities fall within the
#' country boundary. There is a track record of both the facilities with missing coordinates and the ones that fall
#' outside the country boundary.
#' @export
create_hf_shapefile <- function (mainPath, country, mostRecentBoundaries = TRUE, lonlat = TRUE, epsg = NULL, rmNA = NULL, rmOut = NULL, scenario = NULL, nameCSV = NULL) {
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
  pathFacilities <- paste0(mainPath, "/", country, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  if (!is.null(scenario)) {
    if(!is.character(scenario)){
      stop("If not NULL, scenario must be 'character'")
    }
    if(!grepl("[0-9]{3}", scenario)) {
      stop("If not NULL, scenario must contains three characters that correspond to the scenario folder suffix like '001', '002'...'010'...'099'...'100'")
    }
    if(!dir.exists(paste0(pathFacilities, "/scenario", scenario))) {
      stop(paste0(pathFacilities, "/scenario", scenario, "does not exist"))
    }
  }
  if (is.null(nameCSV)) {
    nameCSV <- "health_facilities.csv"
  }
  
  logTxt <- paste0(mainPath, "/", country, "/data/log.txt")

  border <- get_boundaries(mainPath = mainPath, country = country, type = "raw", mostRecentBoundaries)
  scenarioDirs <- list.dirs(pathFacilities, recursive = FALSE)
  scenarioDirs <- scenarioDirs[grepl("scenario", scenarioDirs)]
  if (is.null(scenario)) {
    scenario <- select_scenario(scenarioDirs)
    # if (length(scenarioDirs) == 0) {
    #   stop("Filtered health facility table is missing. Run the filter_hf function.")
    # }
    # scenario <- stringr::str_extract(scenarioDirs, "scenario[0-9]{3}$")
    # if (length(scenario) > 1) {
    #   scenario <- c(scenario, "VIEW")
    #   subInd <- utils::menu(scenario, title = "Select the scenario or the VIEW option to see the selected HFs for each scenario.")
    #   while (subInd == length(scenario)) {
    #     for (i in 1:(length(scenario)-1)) {
    #       message(scenario[i])
    #       cat(paste(readLines(paste(pathFacilities, scenario[i], "selected_hf.txt", sep = "/")), collapse = "\n"))
    #       readline(prompt="Press [enter] to continue")
    #     }
    #     subInd <- utils::menu(scenario, title = "Select the scenario or the VIEW option to see the selected HFs for each scenario.")
    #   }
    #   scenario <- scenarioDirs[subInd]
    # } else {
    #   scenario <- scenarioDirs
    # }
  } else {
    scenario <- paste0("scenario", scenario)
    scenario <- scenarioDirs[grepl(scenario, scenarioDirs)]
  }
  scenarioTime <- select_scenarioTime(scenario)
  # scenarioTime <- list.dirs(scenario, recursive = FALSE)
  # scenarioTime <- scenarioTime[grepl("[0-9]{14}", scenarioTime)]
  # scenarioTime <- stringr::str_extract(scenarioTime, "[0-9]{14}")
  # scenarioTimeForm <- paste0(substr(scenarioTime, 1, 4), "-", substr(scenarioTime, 5, 6), "-", substr(scenarioTime, 7, 8), " ", substr(scenarioTime, 9, 10), ":", substr(scenarioTime, 11, 12), ":", substr(scenarioTime, 13, 14), " CEST")
  # if (length(scenarioTime) > 1) {
  #   scenarioTimeForm <- c(scenarioTimeForm, "VIEW")
  #   subInd <- utils::menu(scenarioTimeForm, title = "Select the filtered table time creation or the VIEW option to see the selection parameters for each time.")
  #   while (subInd == length(scenarioTimeForm)) {
  #     for (i in 1:(length(scenarioTimeForm)-1)) {
  #       message(scenarioTimeForm[i])
  #       cat(paste(readLines(paste(scenario, scenarioTime[i], "time_frame.txt", sep = "/")), collapse = "\n"))
  #       readline(prompt="Press [enter] to continue")
  #     }
  #     subInd <- utils::menu(scenarioTimeForm, title = "Select the filtered table time creation or the VIEW option to see the selection parameters for each time.")
  #   }
  #   scenarioTime <- scenarioTime[subInd]
  # }
  hfFolder <- paste(scenario, scenarioTime, "raw", sep = "/")
  filesCsv <- list.files(hfFolder)[grepl(paste0(nameCSV, "$"), list.files(hfFolder))]
  if (length(filesCsv) == 0) {
    stop(paste(nameCSV, "is missing. Run the filter_hf function first."))
  }
  multiMsg <- "Select the CSV table that you would like to process."
  if (length(filesCsv) > 1) {
    fileInd <- utils::menu(filesCsv, multiMsg)
    fi <- filesCsv[fileInd]
  }else{
    fi <- filesCsv
  }
  df <- read.csv(paste(hfFolder, fi, sep = "/"))
  xy <- data.frame(Lat = df[, "GPS_002", drop = TRUE], Lon = df[, "GPS_001", drop = TRUE])
  if (nrow(xy[complete.cases(xy), ]) == 0) {
    stop_quietly(paste("Coordinates are not available! Add them manually in the CSV file:\n", paste(hfFolder, fi, sep = "/")))
  }
  if (!all(complete.cases(xy))) {
    dfNA <- df[!complete.cases(xy), ]
    df <- df[complete.cases(xy), ]
    # Try with names (if not available => without)
    dfPrint <- tryCatch({dfNA[, c("extern_id", "worksp_id", "date", "MoSD3", "NAME")]}, error = function (e) NULL)
    cat("\n")
    if (is.null(dfPrint)) {
      dfPrint <- dfNA[, c("extern_id", "worksp_id", "date", "MoSD3")]
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
      stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", paste(hfFolder, fi, sep = "/")))
    } else {
      write.table(dfNA, paste(hfFolder, "coordinates_NA.txt", sep = "/"))
      message(paste("\nYou can access the removed HFs at:\n", paste(hfFolder, "coordinates_NA.txt", sep = "/"), "\n"))
    }
  }
  pts <- sp::SpatialPointsDataFrame(coords = xy[complete.cases(xy), ], data = df[complete.cases(xy), ], proj4string = terra::crs(epsg))
  border <- rgeos::gUnaryUnion(as(sf::st_transform(border, terra::crs(pts)), "Spatial"))
  border <- sp::spTransform(border, pts@proj4string)
  inter <- rgeos::gIntersects(border, pts, byid = TRUE)
  interOutside <- FALSE
  if (!all(inter[, 1])) {
    interOutside <- TRUE
    # Try with names (if not available => without)
    dfPrint <- tryCatch({df[!inter, c("extern_id", "worksp_id", "date", "MoSD3", "NAME")]}, error = function (e) NULL)
    if (is.null(dfPrint)) {
      dfPrint <- df[!inter, c("extern_id", "worksp_id", "date", "MoSD3")]
    }
    print(dfPrint)
    message("The facilities indicated above are outside the country boundaries")
    if (rmOut) {
      yn <- 2
    } else {
      yn <- utils::menu(c("Exit the script and correct the coordinates manually in the CSV file", "Remove these HFs and create a HFs' shapefile"), title = paste("\nWhat would you like to do?"))
    }
    if (yn == 1) {
      stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", paste(hfFolder, fi, sep = "/")))
    }
  }
  if (interOutside) {
    write.table(df[!inter[, 1], ], paste(hfFolder, "coordinates_outside.txt", sep = "/"))
    message(paste("\nYou can access the removed HFs at:\n", paste(hfFolder, "coordinates_outside.txt", sep = "/"), "\n"))
  }
  # shp <- pts[inter[, 1], -1]
  shp <- sf::st_as_sf(pts[inter[, 1], -1])
  cat("Saving the HFs' shapefile...\n")
  # rgdal::writeOGR(shp, paste(hfFolder, "health_facilities.shp", sep = "/"), layer = "hf", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  sf::st_write(shp, paste(hfFolder, "health_facilities.shp", sep = "/"), append = FALSE)
  inputFolder <- stringr::str_extract(hfFolder, "scenario[0-9]{3}/[0-9]{14}")
  write(paste0(Sys.time(), ": Health facility shapefile created - Input folder: ", inputFolder), fi = logTxt, append = TRUE)
}
