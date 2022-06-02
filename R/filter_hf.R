#' Filter health facilities
#'
#' Filter the HeRAMS health facility raw table based on a set of variables and export a table that contains only 
#' the selected facilities.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param pathTable character; path to the HeRAMS Excel Table
#' @param scenario character; a string of three characters that correspond to the scenario folder suffix like '001', '002'...'010'...'099'...'100'
#' The criteria for selection are those of the specified scenario. If NULL, an interactive selection by attribute is run in the console.
#' @param mostRecentObs logical; should the most recent observation per health facility be taken into account? If NULL or FALSE, 
#' the user is asked to choose among four methods for selection based on time observation (most recent, date limit and most recent, closest to a specific date or case by case).
#' @details The selection is recorded within a text file (selected_hf.txt) stored in the scenario folder. Different
#' selection criteria create new scenario folders. In the same scenario folder different 'raw' sub-folders may be created
#' depending on the original Excel document modification time, and the selection of observations based on time. 
#' @export
filter_hf <- function (mainPath, country, pathTable, scenario = NULL, mostRecentObs = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  yn <- 2
  if(!is.null(pathTable)) {
    if (!is.character(pathTable)) {
      stop("pathTable must be 'character'")
    } else {
      if (!file.exists(pathTable)) {
        stop("pathTable does not exists!")
      }
    }
  } else {
    yn <- utils::menu(c("YES", "NO"), title = "pathTable is NULL; would like to load ficticious example data for Switzerland?")
    if (yn == 2) {
      stop_quietly("No table to be filtered!")
    } 
  }

  pathFacilities <- paste0(mainPath, "/", country, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  newTib <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 2)}, error = function(e){NULL})
  if (is.null(newTib)) {
    if (yn == 1) {
      newTib <- inAccMod::fictitious_herams_data
    } else {
      stop(paste(paste(pathFacilities, file, sep = "/"), "could not be opened."))
    }
  }
  logTxt <- paste0(mainPath, "/", country, "/data/log.txt")
  mtime <- tryCatch({file.info(pathTable)$mtime}, error = function(e){NULL})
  if(is.null(mtime)) {
    mtime <- "00-00-00 00:00:00 CEST (example data)"
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
  
  if (!is.null(mostRecentObs)) {
    if (!is.logical(mostRecentObs)) {
      stop("mostRecentObs must be 'NULL' or 'logical")
    }
  } else {
    mostRecentObs <- FALSE
  }
  
  
  
  variables <- c(MoSD_status = "MoSD4", 
                 building_damage = "CONDB", 
                 functionality_status = "HFFUNCT",
                 accessibility_status = "HFACC"
  )
  
  if (is.null(scenario)) {
    ## Sub Project
    tempDir <- paste0(pathFacilities, "/temp")
    dir.create(tempDir)
    for (i in 1:length(variables)){
      backupTib <- newTib
      newTib <- tibble_subset(newTib, variables[i], tempDir)
      if (is.null(newTib)) {
        message("\nInvalid index ! All options were kept.")
        newTib <- backupTib
        next
      }
    }
    lines1 <- readLines(paste(tempDir, "selected_hf.txt", sep = "/"))
    fileLst <- list.files(pathFacilities, recursive = TRUE)
    logTxtLst <- fileLst[grepl("selected_hf\\.txt$", fileLst)]
    logTxtLst <- logTxtLst[!grepl("temp/", logTxtLst)]
    scenarioDir <- NULL
    if (length(logTxtLst) > 0) {
      for (i in 1:length(logTxtLst)) {
        lines2 <- readLines(paste(pathFacilities, logTxtLst[i], sep = "/"))
        if (all(lines1 == lines2)){
          scenarioDir <- gsub("/selected_hf\\.txt$", "", logTxtLst[i])
          message(paste("Existing scenario:", scenarioDir))
          break
        }
      }
    }
    if (is.null(scenarioDir)) {
      scenarioDir <- list.dirs(pathFacilities, recursive = FALSE)
      scenarioDir <- scenarioDir[grepl("scenario[0-9]{3}", scenarioDir)]
      if (length(as.character(length(scenarioDir) + 1)) == 1) {
        scenarioDir <- paste0("scenario00", length(scenarioDir) + 1)
      } else if (length(as.character(length(scenarioDir) + 1)) == 2) {
        scenarioDir <- paste0("scenario0", length(scenarioDir) + 1)
      } else {
        scenarioDir <- paste0("scenario", length(scenarioDir) + 1)
      }
      message(paste("\nNew scenario:", scenarioDir))
      dir.create(paste(pathFacilities, scenarioDir, sep = "/"))
      file.copy(paste(tempDir, "selected_hf.txt", sep = "/"), paste(pathFacilities, scenarioDir, sep = "/"))
    }
    unlink(tempDir, recursive = TRUE)
  } else {
    txt <- paste0(pathFacilities, "/scenario", scenario, "/selected_hf.txt")
    txt <- file(txt, open = "r")
    txtLines <- readLines(txt)
    close(txt)
    for ( i in 1:length(txtLines)){
      colN <- stringr::str_extract(txtLines[i], "^.*\\:")
      colN <- stringr::str_to_lower(gsub(":", "", colN))
      colN <- gsub(" ", "_", colN)
      colN <- variables[which(names(variables) == colN)]
      cont <- unlist(strsplit(gsub("^.*\\: ", "", txtLines[i]), ", "))
      newTib <- newTib[newTib[, colN, drop = TRUE] %in% cont, ]
    }
    scenarioDir <- paste0("scenario", scenario)
  }
  
  sysTime <- Sys.time()
  outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste(pathFacilities, scenarioDir, outTimeFolder, "raw", sep = "/")
  dir.create(outFolder, recursive = TRUE)
  logscenarioTxt <-  paste(pathFacilities, scenarioDir, outTimeFolder, "time_frame.txt", sep = "/")
  write(paste0("Modification time of the raw Excel table: ", mtime), file = logscenarioTxt, append = TRUE)
  optionsID <- c("Most recent", "Date limit and most recent", "Closest to a specific date", "Case by case")
  
  if (mostRecentObs) {
    cat("For each facility, the most recent observation is kept.\n")
    optInd <- 1
  } else {
    tableID <- table(newTib$subject_id)
    message(paste("\nThere are between", min(tableID), "and", max(tableID), "observations per health facility."))
    optInd <- utils::menu(optionsID, title = "\nChoose one of the following options for selecting observations")
  }
  write(paste0("Option for selecting observations: ", optionsID[optInd]), file = logscenarioTxt, append = TRUE)
  if (optInd == 2 | optInd == 3) {
    cat("\nEnter a date formatted as following: YYYY/MM/DD")
    k <- 0
    isDate <- NULL
    while (is.null(isDate) & k < 3) {
      if (k > 0) {
        message("Invalid date!")
      }
      k <- k + 1
      dateThr <- readline(prompt = "Date: ")
      isDate <- tryCatch({lubridate::is.Date(as.Date(dateThr))}, error = function(e){NULL})
    }
    if (is.null(isDate)) {
      stop("Invalid date (too many attemps)!")
    }
    write(paste0("Selected date: ", as.Date(dateThr)), file = logscenarioTxt, append = TRUE)
  }
  ids <- unique(newTib$subject_id)
  for (i in 1:length(ids)) {
    subTib <- newTib[newTib$subject_id == ids[i], ]
    if (nrow(subTib) > 1) {
      idDates <- as.Date(subTib$date)
      if (optInd == 1) {
        rmInd <- which(order(idDates, decreasing = TRUE) != 1)
        toRm <- subTib[rmInd, "external_id"]
      } else if (optInd == 2) {
        rmInd <- dateThr > idDates
        # If at least one, let's take the most recent
        if (sum(rmInd) > 1) {
          rmInd <- which(order(idDates, decreasing = TRUE) != 1)
        }
        toRm <- subTib[rmInd, "external_id"]
      } else if (optInd == 3) {
        diffDays <- abs(as.Date(dateThr) - idDates)
        rmInd <- which(order(diffDays) != 1)
        toRm <- subTib[rmInd, "external_id"]
      } else {
        message(paste("Subject ID:", ids[i]))
        toKeep <- utils::menu(idDates, title = "Select the observation that you would like to keep:")
        toRm <- subTib[-toKeep, "external_id"]
        write(paste0("Subject ID: ", ids[i], "; ", as.Date(subTib$date)[toKeep]), file = logscenarioTxt, append = TRUE)
      }
      newTib <- newTib[!newTib$external_id %in% toRm$external_id, ]
    }
  }
  write.csv(newTib, file = paste(outFolder, "health_facilities.csv", sep = "/"))
  write(paste0(Sys.time(), ": Health facilities where filtered - scenario folder: ", scenarioDir, " - input folder: ", outTimeFolder), file = logTxt, append = TRUE)
  cat(paste0("\n", outFolder, "/health_facilities.csv\n"))
}
