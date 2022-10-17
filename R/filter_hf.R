#' Filter health facilities
#'
#' Filter the HeRAMS health facility raw table for different analysis scenario and export a table that contains only 
#' the selected facilities.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param pathTableCode character; path to the HeRAMS CSV table with text for responses ("Available")
#' @param pathTableText character; path to the HeRAMS CSV table with codes for responses ("A1")
#' @param scenario character; a string of three characters that correspond to the scenario folder suffix like '001', '002'...'010'...'099'...'100'
#' The criteria for selection are those of the specified scenario. If NULL, an interactive selection by attribute is run in the console.
#' @param mostRecentObs logical; should the most recent observation per health facility be taken into account? If NULL or FALSE, 
#' the user is asked to choose among four methods for selection based on time observation (most recent, date limit or case by case).
#' @param defaultParameters logical; should the default HeRAMS table parameters (e.g. column names) be used? If not, the user is able to modify it.
#' @param type logical; should the facilities be filtered based on their types.
#' @param ownership logical; should the facilities be filtered based on their ownership.
#' @param status logical; should the facilities be filtered based on their status.
#' @param building logical; should the facilities be filtered based on the building condition.
#' @param equipment logical; should the facilities be filtered based on the equipment condition.
#' @param functionality logical; should the facilities be filtered based on their functionality.
#' @param accessibility logical; should the facilities be filtered based on their accessibility.
#' @param support logical; should the facilities be filtered based on whether they receive support from partners.
#' @param services logical; should the facilities be filtered based on health services information.
#' @param partners logical; should the facilities also be filtered on the different possible supporting partners (ignored if support is FALSE)
#' @param barriers logical; should the facilities also be filtered on the causes of possible impairment (e.g. service not available).
#' @details The selection is recorded within a text file (selected_hf.txt) stored in the scenario folder. Different
#' analysis scenario create new scenario folders. In the same scenario folder different 'raw' sub-folders may be created
#' depending on the original Excel document modification time, and the selection of observations based on time. 
#' @export
filter_hf <- function (mainPath, country, pathTableCode = NULL, pathTableText = NULL, scenario = NULL, mostRecentObs = NULL, 
                       defaultParameters = TRUE,
                       type = TRUE,
                       ownership = FALSE,
                       status = TRUE,
                       building = FALSE,
                       equipment = FALSE,
                       functionality = FALSE,
                       accessibility = FALSE,
                       support = FALSE,
                       services = FALSE,
                       partners = FALSE,
                       barriers = FALSE) {

  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  yn <- 2
  if(!is.null(pathTableCode)) {
    if (!is.character(pathTableCode)) {
      stop("pathTableCode must be 'character'")
    } else {
      if (!file.exists(pathTableCode)) {
        stop(paste(pathTableCode, "does not exists!"))
      }
    }
  } else {
    yn <- utils::menu(c("YES", "NO"), title = "\npathTableCode is NULL; would like to load ficticious example data for Switzerland?")
    if (yn == 2) {
      stop_quietly("No table to be filtered!")
    } 
  }
  
  if(!is.null(pathTableText)) {
    if (!is.character(pathTableText)) {
      stop("pathTableText must be 'character'")
    } else {
      if (!file.exists(pathTableText)) {
        stop(paste(pathTableText, "does not exists!"))
      }
    }
  } else {
    yn <- utils::menu(c("YES", "NO"), title = "\npathTableText is NULL; would like to load ficticious example data for Switzerland?")
    if (yn == 2) {
      stop_quietly("No table to be filtered!")
    } 
  }
  
  # Process the filtering with txt table, but check with code for stop filtering
  # tibTxtNames table are with full column names, for interactive selection (health services)
  tibTxt <- tryCatch({tibble::as_tibble(read.csv(pathTableText, skip = 2))}, error = function(e){NULL})
  tibCode <- tryCatch({tibble::as_tibble(read.csv(pathTableCode, skip = 2))}, error = function(e){NULL})
  tibTxtNames <- tryCatch({tibble::as_tibble(read.csv(pathTableText, header = TRUE, check.names = FALSE), .name_repair = "minimal")}, error = function(e){NULL})
  
  if (is.null(tibTxt) | is.null(tibCode) | is.null(tibTxtNames)) {
    if (yn == 1) {
      tibTxt <- inAccessMod::fictitious_herams_data_txt
      tibCode <- inAccessMod::fictitious_herams_data_code
      tibTxtNames <- inAccessMod::fictitious_herams_data_txt_colnames
    } else {
      stop(paste(pathTableCode, "could not be opened."))
    }
  }
  
  # Check same order
  numRows <- nrow(tibCode) == nrow(tibTxt) & nrow(tibCode) == (nrow(tibTxtNames) - 1)
  if (!numRows) {
    stop_quietly("Different number of rows for the two HeRAMS tables.")
  }
  matchRows <- all(tibCode$external_id %in% tibTxt$external_id) & all(tibTxt$external_id %in% tibTxtNames$`External ID`[-1])
  if (!matchRows) {
    stop_quietly("Tables with labels, codes and full column names are not matching.")
  }
  # Same order
  if (!all(tibCode$external_id == tibTxt$external_id)) {
    cat("\nReordering row orders of HeRAMS tables.")
    tibCode <- tibCode[match(tibCode$external_id, tibTxt$external_id), ]
  }
  cat("\nHeRAMS tables: OK\n")
  pathFacilities <- paste0(mainPath, "/", country, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }

  # Get column code and label
  defaultCodeColumns <- HeRAMS_table_parameters()
  defaultStopLst <- HeRAMS_stop_filtering()
  # Get values that indicate that there is a partner support
  defaultPartnership <- HeRAMS_partnership_values()
  if (!defaultParameters) {
    codeColumns <- set_HeRAMS_table_parameters(defaultCodeColumns)
    stopLst <- set_HeRAMS_stop(defaultStopLst)
    partnershipValues <- set_HeRAMS_key_values(defaultPartnership, "Values that indicate that there is a partner support\n")
  } else {
    codeColumns <- defaultCodeColumns
    stopLst <- defaultStopLst
    partnershipValues <- defaultPartnership
  }
 
  if (barriers) {
  # Get values that indicate that there is an impairment
    defaultImpairmentValues <- HeRAMS_impairment_values()
    if (!defaultParameters) {
      impairmentValues <- set_HeRAMS_key_values(defaultImpairmentValues, "Values that indicate that there is an impairment")
    } else {
      impairmentValues <- defaultImpairmentValues
    }
  } else {
    impairmentValues <- NULL
  }
  
  # All columns taken into account
  cols <- NULL
  for (i in 1:length(codeColumns)) {
    if (grepl("suffix", names(codeColumns)[i], ignore.case = TRUE)) {
      next
    }
    cols <- c(cols, colnames(tibTxt)[grep(codeColumns[[i]], colnames(tibTxt))])
  }
  # Get the columns where the questionnaire may have stopped
  colStop <- NULL
  for (varX in names(stopLst)) {
    colStop <- c(colStop, unlist(codeColumns)[grep(varX, names(unlist(codeColumns)))])
  }
  
  # From and to which column the questionnaire can be stopped
  indStatus <- which(cols == codeColumns$Health_facility_status)
  remainCols <- cols[indStatus:length(cols)]
  
  # Adding "does not apply" value when questionnaire has stopped
  for (i in 1:length(names(stopLst))) {
    varX <- names(stopLst)[i]
    colCode <- codeColumns[[varX]]
    varStop <- stopLst[[varX]]
    if (grepl("\\|", varStop)) {
      varStop <- unlist(strsplit(varStop, split = "\\|"))
    }
    remainCols <- remainCols[!grepl(colCode, remainCols)]
    if (varX == "Functionality") {
      remainCols <- remainCols[!grepl(codeColumns[["Equipment_condition"]], remainCols)]
    }
    if (any(is.na(tibCode[, colCode, drop = TRUE]))) {
      message(paste0("\n", gsub("_", " ", varX)))
      cat("Values for the following facilities are missing.\n")
      print(tibTxt[is.na(tibCode[, colCode, drop = TRUE]), c(1:10, grep(colCode, tibCode))])
      yn <- utils::menu(c("Ignore these facilities", "Exit the script and solve the issue manually"), title = "Select an option.")
      if (yn == 2) {
        stop_quietly("You exit the script.")
      }
    }
    if (sum(tibCode[, colCode, drop = TRUE] %in% varStop) > 0) {
      tibCode[tibCode[, colCode, drop = TRUE] %in% varStop, remainCols] <- "Does not apply (questionnaire was stopped before)"
      tibTxt[tibCode[, colCode, drop = TRUE] %in% varStop, remainCols] <- "Does not apply (questionnaire was stopped before)"
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
  tempDir <- paste0(pathFacilities, "/temp")
  if (dir.exists(tempDir)) {
    unlink(tempDir, recursive = TRUE)
  }
  dir.create(tempDir)
  logscenarioTxt <-  paste(tempDir, "time_frame.txt", sep = "/")
  write(paste0("Modification time of the raw Excel table: ", mtime), file = logscenarioTxt, append = TRUE)
  optionsID <- c("Most recent", "Date limit", "Case by case")
  if (mostRecentObs) {
    cat("For each facility, the most recent observation is kept.\n")
    optInd <- 1
  } else {
    tableID <- table(tibTxt$subject_id)
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
    if (optInd == 2) {
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
    ids <- unique(tibTxt$subject_id)
    for (i in 1:length(ids)) {
      subTib <- tibTxt[tibTxt$subject_id == ids[i], ]
      if (nrow(subTib) > 1) {
        idDates <- as.Date(subTib$date)
        if (optInd == 1) {
          rmInd <- which(order(idDates, decreasing = TRUE) != 1)
          toRm <- subTib[rmInd, "external_id"]
        } else if (optInd == 2) {
          # Which ones should be removed
          rmInd <- dateThr < idDates
          # If there are nothing left
          if (nrow(subTib[!rmInd, ]) == 0) {
            # Remove all
            toRm <- subTib[, "external_id"]
          } else {
            # If there are some left
            subSubTib <- subTib[!rmInd, ]
            # If multiple, remove the ones that are not the most recent
            if (nrow(subSubTib) > 1) {
              idDates <- as.Date(subSubTib$date)
              rmInd <- which(order(idDates, decreasing = TRUE) != 1)
              toRm <- subSubTib[rmInd, "external_id"]
            } else {
              # If only one, remove the other ones
              toRm <- subTib[rmInd, "external_id"]
            }
          }
        } else {
          message(paste("Subject ID:", ids[i]))
          toKeep <- utils::menu(idDates, title = "Select the observation that you would like to keep:")
          toRm <- subTib[-toKeep, "external_id"]
          write(paste0("Subject ID: ", ids[i], "; ", as.Date(subTib$date)[toKeep]), file = logscenarioTxt, append = TRUE)
        }
        tibTxt <- tibTxt[!tibTxt$external_id %in% toRm, ]
        tibCode <- tibCode[!tibCode$external_id %in% toRm, ]     
      }
    }
  }
  if (!support & partners) {
    partners = FALSE
    message("\nsupport = FALSE; partners argument is ignored")
  }
  if (is.null(scenario)) {
    ## Sub Project
    # Position of parameter in the HeRAMS_table_parameters
    posParam <- c(1, 2, 3, 4, 5, 6, 7, 8)
    for (i in 1:length(codeColumns)) {
      # Look if there is a perfect match
      varCol <- colnames(tibTxt)[grep(paste0("^", codeColumns[[i]], "$"), colnames(tibTxt))]
      # If not (e.g. suffix element)
      if (length(varCol) == 0) {
        next
      }
      # If only one match (main info or operationality columns)
      if (length(varCol) == 1) {
        go <- FALSE
        if (i == posParam[1] & type) {
          go <- TRUE
        }
        if (i == posParam[2] & ownership) {
          go <- TRUE
        }
        if (i == posParam[3] & status) {
          go <- TRUE
        }
        if (i == posParam[4] & building) {
          go <- TRUE
        }
        if (i == posParam[5] & equipment) {
          go <- TRUE
        }
        if (i == posParam[6] & functionality) {
          go <- TRUE
        }
        if (i == posParam[7] & accessibility) {
          go <- TRUE
        }
        if (i == posParam[8] & support) {
          go <- TRUE
        }
        if (go) {
          codeName <- names(codeColumns)[i]
          message(paste0("\n", gsub("_", " ", codeName)))
          newTib <- HeRAMS_table_subset(tibT = tibTxt, tibC = tibCode, varCol = varCol, stopQuest = TRUE, codeName = codeName, stopLst = stopLst, tempDir = tempDir, barriers = barriers, codeColumns = codeColumns, impairmentValues = impairmentValues, partners, partnershipValues = partnershipValues)
          tibTxt <- newTib[[1]]
          tibCode <- newTib[[2]]
          stopFiltering <- tryCatch(newTib[[3]], error = function(e) FALSE)
          if (stopFiltering) {
            break
          }
        }
      } else {
        # Skip partner name columns
        if (all(grepl(codeColumns$Partners, varCol))) {
          next
        }
        # If not, has to be services
        if (services) {
          message("\n\nEssential health services")
          yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on specific health services ?"))
          if (yn == 1) {
            pillars <- c("General clinical and emergency care services",
                         "Child health and nutrition",
                         "Communicable diseases",
                         "Sexual and reproductive health",
                         "Noncommunicable diseases")
            nCat <- 1:length(pillars)
            indCat <- paste(paste0("\n", nCat, ": ", pillars))
            cat(indCat)
            cat(paste("\n\nEnter all the indices that correspond to the pillars that include the services you would like to focus on.\nOn the same line separated by a space, or just skip to select all options.\n"))
            selInd <- readline(prompt = "Selection: ")
            selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
            if (length(selInd) == 0){
              selInd <- nCat
            }
            for (j in selInd){
              message(paste("\nPillar: ", pillars[j]))
              subVarCol <- varCol[grepl(paste0(j, "[0-9]{2}"), varCol)]
              pillarServices <- NULL
              for (k in 1:length(subVarCol)) {
                pillarServices <- c(pillarServices, colnames(tibTxtNames)[grep(paste0("^", subVarCol[k], "$"), tibTxtNames[1, ])])
              }
              nCat <- 1:length(pillarServices)
              indCat <- paste(paste0("\n", nCat, ": ", pillarServices))
              cat(indCat)
              cat(paste("\n\nEnter all the indices that correspond to the services you would like to focus on.\nOn the same line separated by a space, or just skip to select all options.\n"))
              selInd <- readline(prompt = "Selection: ")
              selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
              if (length(selInd) == 0){
                selInd <- nCat
              }
              for (ind in selInd) {
                subSubVarCol <- subVarCol[ind]
                message(paste0("\n", colnames(tibTxtNames)[grep(paste0("^", subSubVarCol, "$"), tibTxtNames[1, ])]))
                subSubVarCol <- subVarCol[ind]
                newTib <- HeRAMS_table_subset(tibT = tibTxt, tibC = tibCode, varCol = subSubVarCol, stopQuest = FALSE, codeName = NULL, stopLst = NULL, tempDir = tempDir, barriers = barriers, codeColumns = codeColumns, impairmentValues = impairmentValues, partners, partnershipValues = partnershipValues)
                tibTxt <- newTib[[1]]
                tibCode <- newTib[[2]]
              }
            }
          }
        }
      }
    }
    if (!file.exists(paste(tempDir, "selected_hf.txt", sep = "/"))){
      write("All facilities", file = paste(tempDir, "selected_hf.txt", sep = "/"))
    }
    lines1 <- readLines(paste(tempDir, "selected_hf.txt", sep = "/"))
    fileLst <- list.files(pathFacilities, recursive = TRUE)
    logTxtLst <- fileLst[grepl("selected_hf\\.txt$", fileLst)]
    logTxtLst <- logTxtLst[!grepl("temp/", logTxtLst)]
    scenarioDir <- NULL
    if (length(logTxtLst) > 0) {
      for (i in 1:length(logTxtLst)) {
        lines2 <- readLines(paste(pathFacilities, logTxtLst[i], sep = "/"))
        if (all(lines1 %in% lines2) & all(lines2 %in% lines1)){
          scenarioDir <- gsub("/selected_hf\\.txt$", "", logTxtLst[i])
          message(paste("Existing scenario:", scenarioDir))
          break
        }
      }
    }
    if (is.null(scenarioDir)) {
      scenarioDir <- list.dirs(pathFacilities, recursive = FALSE)
      scenarioDir <- scenarioDir[grepl("scenario[0-9]{3}", scenarioDir)]
      if ((length(scenarioDir) + 1) < 9) {
        scenarioDir <- paste0("scenario00", length(scenarioDir) + 1)
      } else if ((length(scenarioDir) + 1) < 99) {
        scenarioDir <- paste0("scenario0", length(scenarioDir) + 1)
      } else {
        scenarioDir <- paste0("scenario", length(scenarioDir) + 1)
      }
      message(paste("\nNew scenario:", scenarioDir))
      dir.create(paste(pathFacilities, scenarioDir, sep = "/"))
      file.copy(paste(tempDir, "selected_hf.txt", sep = "/"), paste(pathFacilities, scenarioDir, sep = "/"))
    }
  } else {
    txt <- paste0(pathFacilities, "/scenario", scenario, "/selected_hf.txt")
    txt <- file(txt, open = "r")
    txtLines <- readLines(txt)
    close(txt)
    if (!(txtLines)[1] == "All facilities") {
      for (i in 1:length(txtLines)) {
        colN <- stringr::str_extract(txtLines[i], "^.* -> ")
        colN <- gsub(" -> ", "", colN)
        cont <- unlist(strsplit(gsub("^.* -> ", "", txtLines[i]), " [+] "))
        cont[grepl("^NA$", cont)] <- NA
        cond1 <- grepl(paste0("^", colN, "$"), cols)
        if (sum(cond1) == 0) {
          stop("NOT RECOGNIZED COLUMN !")
        } else if (sum(cond1) > 1) {
          indCol <- which(cond1)
          condMat1 <- matrix(NA, nrow = nrow(tibTxt), ncol = sum(cond1))
          for (j in 1:length(indCol)) {
            condMat2 <- matrix(NA, nrow = nrow(tibTxt), ncol = length(cont))
            for (k in 1:length(cont)) {
              condMat2[, k] <- cont[k] %in% tibTxt[, cols[indCol][j], drop = TRUE]
            }
            condMat1[, j] <- apply(condMat2, 1, any)
          }
          tibTxt <- tibTxt[apply(condMat1, 1, any, na.rm = TRUE), ]
          tibCode <- tibCode[apply(condMat1, 1, any, na.rm = TRUE), ]
        } else {
          tibCode <- tibCode[tibTxt[, colN, drop = TRUE] %in% cont, ]
          tibTxt <- tibTxt[tibTxt[, colN, drop = TRUE] %in% cont, ]
        } 
      }
    }
    if (nrow(tibTxt) == 0) {
      unlink(tempDir, recursive = TRUE)
      stop_quietly(paste("The process has stopped has the number of remaining observations is 0. \nTry to modifiy the time option for selecting observations for scenario", scenario, "."))
    }
    scenarioDir <- paste0("scenario", scenario)
  }
  sysTime <- Sys.time()
  outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste(pathFacilities, scenarioDir, outTimeFolder, "raw", sep = "/")
  dir.create(outFolder, recursive = TRUE)
  file.copy(paste(tempDir, "time_frame.txt", sep = "/"), paste(pathFacilities, scenarioDir, outTimeFolder, sep = "/"))
  unlink(tempDir, recursive = TRUE)
  tibTxt <- abbr_col_names(tibTxt)
  tibCode <- abbr_col_names(tibCode)
  dfColnames <- data.frame(code = colnames(tibTxt), label = colnames(tibTxtNames))
  codeCol <- tibCode[, colnames(tibCode)[grepl("^QH[0-9]{3}$", colnames(tibCode))]]
  colnames(codeCol) <- paste0(colnames(codeCol), "_c")
  tibTxt <- dplyr::bind_cols(tibTxt, codeCol)
  write.csv(dfColnames, file = paste(outFolder, "column_codes.csv", sep = "/"))
  write.csv(tibTxt, file = paste(outFolder, "health_facilities.csv", sep = "/"))
  write(paste0(Sys.time(), ": Health facilities where filtered - scenario folder: ", scenarioDir, " - input folder: ", outTimeFolder), file = logTxt, append = TRUE)
  cat(paste0("\n", outFolder, "/health_facilities.csv\n"))
}
