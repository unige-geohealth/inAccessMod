#' Compile Input Layers
#' 
#' Compiles the available processed layers and copy them to a new folder called zToAccessMod to facilitate the further import into AccessMod
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param mostRecent logical; should the most recent 'processed' input be selected? If FALSE and if there are multiple
#' available 'processed' inputs, the user is interactively asked to select the 'processed' input based on file creation time.
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
  folderLst <- list.dirs(paste0(mainPath, "/", country))
  inputsAv <- folderLst[grepl("processed", folderLst)]
  inputsAv <- unique(gsub("/[0-9]{14}/processed.*", "", gsub("^.*/data/", "", inputsAv)))
  if (length(inputsAv) == 0) {
    stop("No processed inputs available.")
  }
  sysTime <- Sys.time()
  outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste(mainPath, country, "data/zToAccessMod", outTimeFolder, sep = "/")
  dir.create(outFolder, recursive = TRUE)
  for (i in 1:length(inputsAv)) {
    inputFolder <- paste0(mainPath, "/", country, "/data/", inputsAv[i])
    inputFolders <- check_exists(inputFolder, "processed", layer = TRUE)
    timeFolder <- select_input(inputFolders, paste(inputsAv[i], "downloaded at:"), mostRecent) 
    inputFolder <- folderLst[grepl(paste0("processed/", timeFolder), folderLst)]
    files <- list.files(inputFolder, full.names = TRUE)
    for (file in files) {
      cat(paste("\nCopying", file, "to", outFolder))
      file.copy(from = file, to = outFolder, copy.date = TRUE, overwrite = TRUE)
    }
  }
  write(paste0(Sys.time(), ": ", paste(inputsAv, collapse = ", "), "copied to 'zToAccessMod' - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
}