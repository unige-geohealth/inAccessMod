#' Compile Input Layers
#' 
#' Compiles the available processed layers and copy them to a new folder called zToAccessMod to facilitate the further import into AccessMod
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param mostRecent logical; should the most recent 'processed' input be selected? If FALSE and if there are multiple
#' available 'processed' inputs, the user is interactively asked to select the 'processed' input based on file creation time.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath, country)}
#' 
#' # Replace myCountry with the country name you are working on (workDir subfolder)
#' \dontrun{
#' country <- "myCountry"
#' download_boundaries(mainPath, country, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)
#' set_projection(mainPath, country, mostRecent = TRUE, alwaysSet = TRUE, bestCRS = TRUE)
#' download_landcover(mainPath, country, alwaysDownload = TRUE, mostRecent = TRUE)
#' download_population(mainPath, country, alwaysDownload = TRUE)
#' download_dem(mainPath, country, alwaysDownload = TRUE, mostRecent = TRUE)
#' download_osm(x = "roads", mainPath, country, alwaysDownload = TRUE, countryName = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(x = "waterLines", mainPath, country, alwaysDownload = TRUE, countryName = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(x = "naturalPolygons", mainPath, country, alwaysDownload = TRUE, countryName = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' process_inputs(mainPath, country, selectedInputs = "All", mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = TRUE, newRes = 100, popCorrection = TRUE, gridRes = 3000)
#' compile_processed_data(mainPath, country, mostRecent = TRUE)}
#' @export
compile_processed_data <- function (mainPath, country, mostRecent = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!is.logical(mostRecent)) {
    stop("mostRecent must be 'logical'")
  }
  logTxt <- paste0(mainPath, "/", country, "/data/log.txt")
  folderLst <- list.dirs(paste0(mainPath, "/", country, "/data"))
  inputsAv <- folderLst[grepl("processed", folderLst)]
  inputsAv <- unique(gsub("/[0-9]{14}/processed.*", "", gsub("^.*/data/", "", inputsAv)))
  if (length(inputsAv) == 0) {
    stop("No processed inputs available.")
  }
  outTimeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  outFolder <- file.path(mainPath, country, "data/zToAccessMod", outTimeFolder)
  check_path_length(outFolder)
  dir.create(outFolder, recursive = TRUE)
  for (i in 1:length(inputsAv)) {
    inputFolder <- paste0(mainPath, "/", country, "/data/", inputsAv[i])
    inputFolders <- check_exists(inputFolder, "processed", layer = TRUE)
    timeFolder <- select_input(inputFolders, paste(inputsAv[i], "processed at:"), mostRecent) 
    inputFolder <- folderLst[grepl(paste0("processed/", timeFolder), folderLst)]
    files <- list.files(inputFolder, full.names = TRUE)
    for (file in files) {
      cat(paste("\nCopying", file, "to", outFolder))
      check_path_length(paste0(outFolder, "/", gsub("^.*/", "", file)))
      file.copy(from = file, to = outFolder, copy.date = TRUE, overwrite = TRUE)
    }
  }
  cat("\n")
  write(paste0(Sys.time(), ": ", paste(inputsAv, collapse = ", "), "copied to 'zToAccessMod' - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
  return(TRUE)
}