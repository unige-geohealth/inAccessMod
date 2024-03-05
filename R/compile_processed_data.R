#' Compile Input Layers
#' 
#' Compiles the available processed layers and copy them to a new folder called zToAccessMod to facilitate the further import into AccessMod
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param mostRecent logical; should the most recent 'processed' input be selected? If FALSE and if there are multiple
#' available 'processed' inputs, the user is interactively asked to select the 'processed' input based on file creation time.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myLocation with the location name you are working on (workDir subfolder)
#' \dontrun{
#' location <- "myLocation"
#' download_boundaries(mainPath, location, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)
#' set_projection(mainPath, location, mostRecent = TRUE, alwaysSet = TRUE, bestCRS = TRUE)
#' download_landcover(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)
#' download_population(mainPath, location, alwaysDownload = TRUE)
#' download_dem(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)
#' download_osm(mainPath, location, type = "roads", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(mainPath, location, type = "waterLines", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(mainPath, location, type = "waterPolygons", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' process_inputs(mainPath, location, selectedInputs = "All", mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = FALSE, popCorrection = TRUE, gridRes = 3000)
#' compile_processed_data(mainPath, location, mostRecent = TRUE)}
#' @export
compile_processed_data <- function (mainPath, location, mostRecent = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  if (!is.logical(mostRecent)) {
    stop("mostRecent must be 'logical'")
  }
  logTxt <- paste0(mainPath, "/", location, "/data/log.txt")
  folderLst <- list.dirs(paste0(mainPath, "/", location, "/data"))
  inputsAv <- folderLst[grepl("processed", folderLst)]
  inputsAv <- unique(gsub("/[0-9]{14}/processed.*", "", gsub("^.*/data/", "", inputsAv)))
  if (length(inputsAv) == 0) {
    stop("No processed inputs available.")
  }
  outTimeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  outFolder <- file.path(mainPath, location, "data/zToAccessMod", outTimeFolder)
  check_path_length(outFolder)
  dir.create(outFolder, recursive = TRUE)
  for (i in 1:length(inputsAv)) {
    inputFolder <- paste0(mainPath, "/", location, "/data/", inputsAv[i])
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
