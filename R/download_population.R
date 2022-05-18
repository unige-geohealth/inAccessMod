#' Download population raster
#'
#' Download a population raster from the World Pop FTP and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country/region name folder
#' @param region character; the country/region name
#' @param alwaysDownload logical; should the raster always be downloaded, even they have already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @details The function accesses the World Pop FTP \url{ftp://ftp.worldpop.org.uk/GIS/Population/} and uses an internal
#' function (\code{navigate_ftp}) in order to interactively navigate through the folders and select the population raster.
#' @export
download_population <- function (mainPath, region, alwaysDownload = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  # Check directory
  pathPop <- paste0(mainPath, "/", region, "/data/rPopulation")
  folders <- check_exists(pathPop, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  iso <- get_param(mainPath, region, "ISO")
  pathFTP0 <- ftpWorldPop
  pathFTP <- pathFTP0
  downloadProcess <- TRUE
  ftpWorldPop <- "ftp://ftp.worldpop.org.uk/GIS/Population/"
  while (downloadProcess) {
    # To avoid error message
    gc()
    # Get directories
    folderLst <- RCurl::getURL(pathFTP, verbose = FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
    # While we don't exit the function
    out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0)
    folderLst <- out[[1]]
    pathFTP <- out[[2]]
    if (is.null(folderLst)) {
      stop_quietly("You exit the function. No file has been downloaded.")
    }
    nFile <- 1:length(folderLst)
    indFile <- paste(paste0("\n", nFile, ": ", folderLst))
    cat(indFile)
    cat("\n\nSelect file (corresponding number) to be downloaded.\nType zero to get back to the root directory.\nSkip to exit the function.")
    cat("\n ")
    selInd <- readline(prompt = "Selection: ")
    selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
    if (length(selInd) == 0) {
      stop_quietly("You exit the function. No file has been downloaded.")
    }else if (0 %in% selInd) {
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    } else if (length(selInd) > 1) {
      message("Multiple selection is not allowed.")
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    } else if (!all(selInd %in% nFile)) {
      message("Multiple selection is not allowed.")
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    }else{
      logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
      sysTime <- Sys.time()
      timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      dir.create(paste0(pathPop, "/", timeFolder, "/raw"), recursive = TRUE)
      pathPop <- paste0(pathPop, "/", timeFolder, "/raw")
      for (i in selInd) {
        filePath <- paste0(pathFTP, folderLst[i])
        utils::download.file(url = filePath, destfile = paste0(pathPop, "/", folderLst[i]), quiet = FALSE, mode = "wb", method = "libcurl")
        write(paste0(Sys.time(), ": Population raster downloaded from ", filePath, " - Input folder ", timeFolder), file = logTxt, append = TRUE)
        cat(paste0(pathPop, "/", folderLst[i], "\n"))
      }
      downloadProcess <- FALSE
    }
  }
}