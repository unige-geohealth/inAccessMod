#' Filter health facilities
#'
#' Filter the HeRAMS health facility raw table based on a set of variables and export a table that contains only 
#' the selected facilities. The selection is recorded within a log.txt file stored in the output folder.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param mostRecent logical; should the most recent 'raw' health facility table be used? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on date and time.
#' @param pathTable character; path to the HeRAMS Excel Table
#' @export
filter_hf <- function (mainPath, region, pathTable) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.character(pathTable)) {
    stop("pathTable must be 'character'")
  } else {
    if (!file.exists(pathTable)) {
      stop("pathTable does not exists!")
    }
  }
  pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }

  timeFolder <- choose_input(rawHF, "Excel table copied at", mostRecent)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")
  } else {
    pathFacilities <- paste0(pathFacilities, "/", timeFolder, "/raw/")
    files <- list.files(pathFacilities)[grepl("xlsx", list.files(pathFacilities))]
    if (length(files) > 1) {
      fileInd <- utils::menu(files, "Select the index corresponding to the health facility table to be processed.")
      file <- files[fileInd]
    }else{
      file <- files
    }
    sysTime <- Sys.time()
    t0 <- gsub("-|[[:space:]]|\\:","", sysTime)
    outFolder <- paste(gsub("raw", "processed", pathFacilities), t0, sep = "/")
    dir.create(outFolder, recursive = TRUE)
    fileConn <- file(paste(outFolder, "log.txt", sep = "/"))
    writeLines(sysTime %>% as.character(), fileConn)
    close(fileConn)
    newTib <- tryCatch({readxl::read_excel(paste(pathFacilities, file, sep = "/"), skip = 1, sheet = 2)}, error = function(e){NULL})
    if (is.null(newTib)) {
      stop(paste(paste(pathFacilities, file, sep = "/"), "could not be opened."))
    }
    variables <- c(health_facility_types = "MoSD3", 
                   facility_ownership = "MoSD7", 
                   functionality_status = "HFFUNCT", 
                   facility_status = "MoSD4",
                   accessibility_status = "HFACC"
    )
    for (i in 1:length(variables)){
      backupTib <- newTib
      newTib <- filter_facilities(newTib, variables[i], outFolder)
      if (is.null(newTib)) {
        message("\nInvalid index ! All options were kept.")
        newTib <- backupTib
        next
      }
    }
    write.csv(newTib, file = paste(outFolder, "health_facilities.csv", sep = "/"))
  }
}