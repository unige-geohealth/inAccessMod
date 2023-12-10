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
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myCountry with the country name you are working on (workDir subfolder)
#' \dontrun{
#' country <- "myCountry"
#' download_boundaries(mainPath, country, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)}
#' 
#' # Replace myHeRAMScodeTable with the path of the HeRAMS table that contains codes; set to NULL to use example data
#' # Replace myHeRAMStextTable with the path of the HeRAMS table that contains text; set to NULL to use example data
#' \dontrun{
#' pathTableCode <- "myHeRAMScodeTable"
#' pathTableText <- "myHeRAMStextTable"
#' HeRAMS_filter_hf(mainPath, country, pathTableCode, pathTableText, barriers = FALSE, mostRecentObs = TRUE)
#' HeRAMS_create_hf_shapefile(mainPath, country, mostRecentBoundaries = TRUE, lonlat = TRUE, rmNA = TRUE, rmOut = TRUE, scenario = NULL)} 
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
    if(!dir.exists(file.path(pathFacilities, paste0("scenario", scenario)))) {
      stop(paste0(pathFacilities, "/scenario", scenario, " does not exist"))
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
    message(paste("\nCheck the column names of:", file.path(hfFolder, fi), "and change the parameters accordingly"))
    codeColumns <- unlist(set_HeRAMS_table_parameters(colNamesHeRAMS, regex = FALSE))
    if (!all(codeColumns %in% colnames(df))) {
      stop_quietly("Column name not valid !")
    }
  } else {
    codeColumns <- unlist(colNamesHeRAMS)
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
      dfPrint <- dfNA[, codeColumns[-which(names(codeColumns) == "HF name")]]
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
  pts <- cbind(sf::st_as_sf(xy[complete.cases(xy), ], coords = c(1,2), crs = epsg), df)
  border <- sf::st_union(sf::st_transform(border, crs = sf::st_crs(pts)))
  # Tolerance
  border <- sf::st_buffer(border, dist = 0.2)
  # Intersection
  inter <- suppressWarnings(sf::st_intersects(border, pts, sparse = FALSE))[1, ]
  interOutside <- FALSE
  if (!all(inter)) {
    interOutside <- TRUE
    # Try with names (if not available => without)
    dfPrint <- tryCatch({df[!inter, codeColumns]}, error = function (e) NULL)
    if (is.null(dfPrint)) {
      dfPrint <- df[!inter, codeColumns[-which(names(codeColumns) == "name")]]
    }
    print(dfPrint)
    message("The facilities indicated above are outside the selected boundaries")
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
    write.table(df[!inter, ], file.path(hfFolder, paste0(nameCSV, "_coordinates_outside.txt")))
    message(paste("\nYou can access the removed HFs at:\n", file.path(hfFolder, paste0(nameCSV, "_coordinates_outside.txt")), "\n"))
  }
  shp <- sf::st_as_sf(pts[inter, -1])
  cat("Saving the HFs' shapefile...\n")
  tempShp <- tempfile()
  tryWrite <- tryCatch({sf::st_write(shp, dsn = paste0(tempShp, ".shp"), append = FALSE)}, error = function (e) 0)
  # Check if the lock file exists
  k <- 0
  # If tryWrite = 0; dim(tryWrite) is null; if writing succeeded, tryWrite is the shp
  while (is.null(dim(tryWrite)) & k < 10) {
    k <- k + 1
    colNames <- colnames(shp)[-ncol(shp)]
    # nCat <- 1:length(colNames)
    # indCat <- paste(paste0("\n", nCat, ": ", colNames))
    # cat(indCat)
    # So we have time to see the message (when k > 0)
    Sys.sleep(1.5)
    print(colNames, max = length(colNames))
    message("\n\nError when writing the shapefile, let's try reducing the number of fields")
    cat(paste0("\nSelect the indices of the fields to be removed.\nOn the same line separated by a comma, or by an hyphen for sequences (e.g. 1,2,3-5). Type 0 to exit.\n"))
    selInd <- readline(prompt = "Selection: ")
    selInd <- gsub("-", ":", selInd)
    if (selInd == "0") {
      stop_quietly("You exit the function.")
    }
    elements <- unlist(strsplit(selInd, ","))
    result <- c(sapply(elements, function(x) {
      if (grepl(":", x)) {
        eval(parse(text = x))
      } else {
        as.numeric(x)
      }
    }))
    if (is.list(result)) {
      toRm <- do.call(c, result)
    } else {
      toRm <- result
    }
    names(toRm) <- NULL
    if (length(selInd) == 0) {
      message("\nEmpty selection!")
    } else if (!all(colNames[toRm] %in% colNames)) {
       message("\nWrong indices!")
    } else {
      shp <- shp[, -toRm]
      tempShp <- tempfile()
      tryWrite <- tryCatch({sf::st_write(shp, dsn = paste0(tempShp, ".shp"), append = FALSE)}, error = function (e) 0)
    }
  }
  if (k == 10) {
    stop_quietly("Too many attempts.")
  }
  filesToCopy <- list.files(dirname(paste0(tempShp, ".shp")), pattern = basename(tempShp), full.names = TRUE)
  for (i in 1:length(filesToCopy)) {
    fi <- basename(filesToCopy[i])
    fi <- paste0(nameCSV, ".", gsub("^.*\\.", "", fi))
    file.copy(filesToCopy[i], file.path(hfFolder, fi))
  }
  inputFolder <- stringr::str_extract(hfFolder, "scenario[0-9]{3}/[0-9]{14}")
  write(paste0(Sys.time(), ": Health facility shapefile created - Input folder: ", inputFolder), fi = logTxt, append = TRUE)
  cat(paste0("Done: ", file.path(hfFolder, paste0(nameCSV, ".shp")), "\n"))
  return(TRUE)
}
