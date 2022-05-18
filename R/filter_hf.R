#' Filter health facilities
#'
#' Filters interactively the HeRAMS health facility raw table based on a set of variables and export a table that contains only 
#' the selected facilities. The selection is recorded within a log.txt file stored in the output folder.
#' @param mainPath character; the parent directory of the country/region name folder
#' @param region character; the country name
#' @param mostRecent logical; should the most recent raw health facility table be used? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on date and time.
#' @export
filter_hf <- function (mainPath, region, mostRecent) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  rawHF <- check_exists(pathFacilities, "raw", layer = FALSE, extension = "xlsx")
  if (is.null(rawHF)) {
    stop(paste0("Raw health facility table ('.xlxs') is missing. You might have to run the copy_input function."))
  }
  timeFolder <- choose_input(rawHF, "Excel table copied at", mostRecent)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")
  } else {
    pathFacilities <- paste0(pathFacilities, "/", timeFolder, "/raw/")
    files <- list.files(pathFacilities)[grepl("xlsx", list.files(pathFacilities))]
    if (length(files) > 1) {
      fileInd <- menu(files, "Select the index corresponding to the health facility table to be processed.")
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
                   accesibility_status = "HFACC"
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