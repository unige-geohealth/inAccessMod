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
#' @details Once the missing coordinate issue is addressed, the function checks whether the health facilities fall within the
#' country boundary. There is a track record of both the facilities with missing coordinates and the ones that fall
#' outside the country boundary.
#' @export
create_hf_shapefile_multi <- function (mainPath, country, scenario = NULL, service = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
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
  scenarioDirs <- list.dirs(pathFacilities, recursive = FALSE)
  scenarioDirs <- scenarioDirs[grepl("scenario", scenarioDirs)]
  if (is.null(scenario)) {
    scenario <- select_scenario(scenarioDirs)
  } else {
    scenario <- paste0("scenario", scenario)
    scenario <- scenarioDirs[grepl(scenario, scenarioDirs)]
  }
  scenarioTime <- select_scenarioTime(scenario)
  hfFolder <- paste(scenario, scenarioTime, "raw", sep = "/")
  print(hfFolder)
  filesShp <- list.files(hfFolder)[grepl("\\.shp$", list.files(hfFolder))]
  print(filesShp)
  multiMsg <- "Select the shapefile that you would like to process."
  if (length(filesShp) > 1) {
    fileInd <- utils::menu(filesShp, multiMsg)
    fi <- filesShp[fileInd]
  }else{
    fi <- filesShp
  }
  shp <- sf::st_read(paste(hfFolder, fi, sep = "/"))
  colCodes <- read.csv(paste(hfFolder, "column_codes.csv", sep = "/"))
  pillars <- c("General clinical and emergency care services",
               "Child health and nutrition",
               "Communicable diseases",
               "Sexual and reproductive health",
               "Noncommunicable diseases")
  selInd <- utils::menu(pillars, title = "\nSelect the pillar that includes the health service you would like to focus on.")
  pillarServices <- colCodes$label[grepl(paste0("^QH", selInd, "[0-9]{2}$"), colCodes$code)]
  selInd <- utils::menu(pillarServices, title = "Select the health service you would like to focus on.")
  print(selInd)
  code <- colCodes$code[colCodes$label == pillarServices[selInd]]
  print(code)
  responses <- unique(shp[, code, drop = TRUE])
  accessLst <- vector("list", 3L)
  names(accessLst) <- c("no access", "partial access", "full access")
  message(paste("Service:", pillarServices[selInd]))
  message(paste("HeRAMS column code:", code))
  for (i in 1:length(accessLst)) {
    categories <- c(responses, "NONE", "CANCEL THE FILTERING PROCESS")
    nCat <- 1:length(categories)
    indCat <- paste(paste0("\n", nCat, ": ", categories))
    cat(indCat)
    cat(paste0("\n\nSelect the responses that correspond to '", names(accessLst)[i], "'", "\nOn the same line separated by a space."))
    k <- 0
    while (k < 3) {
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
      if (length(selInd) == 1) {
        if (selInd == length(categories)) {
          stop_quietly("You canceled the filtering process.")
        } else if (selInd == (length(categories) - 1)) {
          break
        } else {
          if (selInd %in% nCat) {
            accessLst[[i]] <- categories[selInd]
            responses <- responses[!responses %in% categories[selInd]]
            break
          } else {
            message("\nInvalid selection!")
            k <- k + 1
          }
        }
      } else {
        if (any(selInd %in% c(length(categories), (length(categories) - 1)))) {
          message("\nInvalid selection!")
          k <- k + 1
        } else if (all(selInd %in% nCat)) {
          accessLst[[i]] <- categories[selInd]
          responses <- responses[!responses %in% categories[selInd]]
          break
        } else {
          message("\nInvalid selection!")
          k <- k + 1
        }
      }
    }
  }
  for (i in 1:length(accessLst)) {
    print(i)
  }
}
