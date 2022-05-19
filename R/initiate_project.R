#' Initiate project
#'
#' Select the country, get the ISO 3166-1 alpha-3 country code, store it in a config.txt file and create the directory 
#' main structure for the project. This function also creates a log.txt file that will record and track the main operations 
#' related to the project.
#' @param mainPath A path where the country folder will be created
#' @details The final structure arises when downloading and processing the data with the corresponding functions,
#' and it allows multiple 'raw' inputs and multiple 'processed' outputs for each input. This can be useful when 
#' performing different analyses for the same country (e.g. when we have updated data).
#' @export
initiate_project <- function (mainPath) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!dir.exists(mainPath)) {
    stop(paste(mainPath, "folder does not exist."))
  }
  countryLst <- countrycode::codelist$country.name.en[!is.na(countrycode::codelist$un.name.en)]
  countryInd <- utils::menu(countryLst, title="Select the country", graphics=TRUE)
  if (countryInd == 0) {
    stop_quietly("You exit the function.")
  }
  # Store the original name
  regionOriginalName <- countryLst[countryInd]
  # Store the ISO code
  iso3 <- as.character(countrycode::codelist[countrycode::codelist$country.name.en == regionOriginalName, "iso3c"])
  # Modify the name if necessary for directory name
  region <- gsub(" \\(.*\\)|\\(.*\\)", "", regionOriginalName)
  region <- gsub("[^[[:alnum:]]", " ", region)
  region <- stringr::str_squish(region)
  region <- gsub("[[:space:]]", "_", region)
  region <- stringi::stri_trans_general(str = region, id = "Latin-ASCII")
  # Main standard inputs
  inputNames=c("rDEM", "rPopulation", "rLandcover", "vRoads", "vWaterLines", 
               "vWaterPolygons", "vBorders","vFacilities")
  message(paste("\nThe following input folders will be created:", paste(inputNames,collapse=", ")))
  # Add other data ?
  yn <- utils::menu(c("YES","NO"), title="\nDo you want to add another input folder (type 1 or 2)?")
  if (yn == 0) {
    stop_quietly("You exit the function.")
  }
  while (yn == 1) {
    newName <- readline(prompt = "Enter the folder name: ")
    newName <- gsub(" \\(.*\\)|\\(.*\\)", "", newName)
    newName <- gsub("[^[[:alnum:]]", " ", newName)
    newName <- stringr::str_squish(newName)
    newName <- gsub("[[:space:]]", "_", newName)
    newName <- stringi::stri_trans_general(str = newName, id = "Latin-ASCII")
    types <- c("r", "v")
    type <- utils::menu(c("Raster data", "Vector data"), title="\nWhich type of data will contain this folder?")
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
    newName <- paste0(types[type], stringr::str_to_title(newName))
    inputNames <- c(inputNames, newName)
    yn <- utils::menu(c("YES", "NO"), title="\nDo you want to add another input folder?")
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
  }
  # Create directories
  pathData <- paste0(mainPath, "/", toupper(region), "/data")
  dir.create(pathData, recursive = TRUE, showWarnings = FALSE)
  for (inputName in inputNames) {
    dir.create(paste0(pathData, "/", inputName), showWarnings = FALSE)
  }
  # Create config.txt for ISO code, and then EPSG as well
  pathRegion <- paste0(mainPath, "/", region, "/data")
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  writeLines(c(paste0("COUNTRY:", regionOriginalName), paste0("ISO:", iso3)), fileConn)
  close(fileConn)
  # Create log.txt for operation tracking
  fileConn <- file(paste0(pathRegion, "/log.txt"))
  writeLines(regionOriginalName, fileConn)
  writeLines(paste0(Sys.time(), ": Project initiated"), fileConn)
  close(fileConn)
  # Print directory tree
  fs::dir_tree(paste0(mainPath, "/", region, "/data"))
}
