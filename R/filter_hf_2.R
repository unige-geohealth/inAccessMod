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
filter_hf_2 <- function (mainPath, country, pathTable, scenario = NULL, mostRecentObs = NULL) {
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
    yn <- utils::menu(c("YES", "NO"), title = "\npathTable is NULL; would like to load ficticious example data for Switzerland?")
    if (yn == 2) {
      stop_quietly("No table to be filtered!")
    } 
  }

  pathFacilities <- paste0(mainPath, "/", country, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  herams <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 2)}, error = function(e){NULL})
  if (is.null(herams)) {
    if (yn == 1) {
      herams <- inAccMod::fictitious_herams_data
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
      stop(paste0(pathFacilities, "/scenario", scenario, " does not exist"))
    }
  }
  
  if (!is.null(mostRecentObs)) {
    if (!is.logical(mostRecentObs)) {
      stop("mostRecentObs must be 'NULL' or 'logical")
    }
  } else {
    mostRecentObs <- FALSE
  }

  if (is.null(scenario)) {
    ## Sub Project
    tempDir <- paste0(pathFacilities, "/temp")
    dir.create(tempDir)
    analysis_scenario(inAccMod::hf_attributes)
    
    txtLines <- readLines(paste(tempDir, "selected_hf.txt", sep = "/"))
    fileLst <- list.files(pathFacilities, recursive = TRUE)
    logTxtLst <- fileLst[grepl("selected_hf\\.txt$", fileLst)]
    logTxtLst <- logTxtLst[!grepl("temp/", logTxtLst)]
    scenarioDir <- NULL
    if (length(logTxtLst) > 0) {
      for (i in 1:length(logTxtLst)) {
        txtLines2 <- readLines(paste(pathFacilities, logTxtLst[i], sep = "/"))
        if (all(txtLines %in% txtLines2) & all(txtLines2 %in% txtLines)){
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
  }
 
  for ( i in 1:length(txtLines)){
    print(txtLines[i])
    #############################
    if (grepl("Not relevant", txtLines[i])) {
      next
    }
    vals <- unlist(strsplit(gsub("^.*\\: ", "", txtLines[i]), " [+] "))
    colN <- stringr::str_extract(txtLines[i], "^[A-z]*")
    colN <- unlist(inAccMod::col_correspondence[names(inAccMod::col_correspondence) == colN])
    condMat1 <- matrix(NA, nrow = nrow(herams), ncol = length(colN))
    for (i in 1:length(colN)) {
      condMat2 <- matrix(NA, nrow = nrow(herams), ncol = length(vals))
      for (j in 1:length(vals)) {
        condMat2[, j] <- grepl(vals[j], herams[, colN[i], drop = TRUE], ignore.case = TRUE)
      }
      condMat1[, i] <- apply(condMat2, 1, any)
    }
    
    # colN <- gsub(":", "", colN)
    # colN <- variables[which(names(variables) == colN)]
    # cont <- unlist(strsplit(gsub("^.*\\: ", "", txtLines[i]), " [+] "))
    # cont[grepl("^NA$", cont)] <- NA
    # herams <- herams[herams[, colN, drop = TRUE] %in% cont, ]
    #############################
  }
  stop_quietly("Bye bye")
  scenarioDir <- paste0("scenario", scenario)
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
    tableID <- table(herams$subject_id)
    if (min(tableID) == max(tableID) & min(tableID) == 1) {
      optInd <- 0
    } else if (min(tableID) != max(tableID)) {
      message(paste("\nThere are between", min(tableID), "and", max(tableID), "observations per health facility."))
      optInd <- utils::menu(optionsID, title = "\nChoose one of the following options for selecting observations")
    } else {
      message(paste("\nThere are", min(tableID), "observations per health facility."))
      optInd <- utils::menu(optionsID, title = "\nChoose one of the following options for selecting observations")
    }
  }
  if (optInd == 0) {
    message("\nOnly 1 observation per health facility")
    write("Only 1 observation per health facility)", file = logscenarioTxt, append = TRUE)
  } else {
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
        if (!grepl("[0-9]{4}/[0-9]{2}/[0-9]{2}", dateThr)) {
          isDate <- NULL
        } else {
          isDate <- tryCatch({lubridate::is.Date(as.Date(dateThr))}, error = function(e){NULL})
        }
      }
      if (is.null(isDate)) {
        stop("Invalid date (too many attempts)!")
      }
      write(paste0("Selected date: ", as.Date(dateThr)), file = logscenarioTxt, append = TRUE)
    }
    ids <- unique(herams$subject_id)
    for (i in 1:length(ids)) {
      subTib <- herams[herams$subject_id == ids[i], ]
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
        herams <- herams[!herams$external_id %in% toRm$external_id, ]
      }
    }
  }
  write.csv(herams, file = paste(outFolder, "health_facilities.csv", sep = "/"))
  write(paste0(Sys.time(), ": Health facilities where filtered - scenario folder: ", scenarioDir, " - input folder: ", outTimeFolder), file = logTxt, append = TRUE)
  cat(paste0("\n", outFolder, "/health_facilities.csv\n"))
}
