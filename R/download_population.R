#' Download Population Raster
#'
#' Download a population raster from the World Pop FTP and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param alwaysDownload logical; should the raster always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param allowInteractivity logical; whether to enable interactivity. \code{TRUE} by default. When is FALSE, alwaysDownload is automatically set to TRUE.
#' When FALSE, UNAdj constrained population raster is downloaded, and when available, maxar_v1 images are prioritized over BSGM images.
#' @param testMode logical; to be ignored. Only used when testing the function with testthat.
#' @details The function accesses the World Pop FTP and uses an internal function (\code{navigate_ftp}) in order to interactively navigate 
#' through the folders and select the population raster to be downloaded. The ISO code retrieved internally by the \code{get_param} function 
#' is used to match the country FTP folder when available. When allowInteractivity = FALSE, predefined parameters are set (see parameter details.)
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myLocation with the location name you are working on (workDir subfolder)
#' \dontrun{
#' location <- "myLocation"
#' download_population(mainPath, location, alwaysDownload = TRUE)}
#' @export
download_population <- function (mainPath, location, alwaysDownload = FALSE, allowInteractivity = TRUE, testMode = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(allowInteractivity)) {
    stop("allowInteractivity must be 'logical'")
  }
  if (!allowInteractivity) {
    if (!alwaysDownload) {
      message("As allowInteractivity = FALSE, alwaysDownload is ignored.")
      alwaysDownload <- TRUE
    }
  }
  
  # Check directory
  pathPop <- file.path(mainPath, location, "data", "rPopulation")
  folders <- check_exists(pathPop, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  iso <- get_param(mainPath, location, "ISO")
  pathFTP0 <- "ftp://ftp.worldpop.org.uk/GIS/Population/"
  pathFTP <- pathFTP0
  downloadProcess <- TRUE
  while (downloadProcess) {
    # To avoid error message
    gc()
    # Get directories
    folderLst <- RCurl::getURL(pathFTP, verbose = FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r|\\n", " ", folderLst), split=" "))
    folderLst <- folderLst[!folderLst == ""]
    # While we don't exit the function
    if (testMode) {
      out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0, allowInteractivity, testMode = TRUE)
    } else {
      out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0, allowInteractivity, testMode = FALSE) 
    }
    folderLst <- out[[1]]
    pathFTP <- out[[2]]
    if (is.null(folderLst)) {
      stop_quietly("You exit the function. No file has been downloaded.")
    }
    nFile <- 1:length(folderLst)
    indFile <- paste(paste0("\n", nFile, ": ", folderLst))
    if (testMode) {
      selInd <- 1
    } else {
      if (allowInteractivity) {
        cat(indFile)
        cat("\n\nSelect file (corresponding number) to be downloaded.\nType zero to get back to the root directory.\nSkip to exit the function.")
        cat("\n ")
        selInd <- readline(prompt = "Selection: ")
        selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
        print(selInd)
      } else {
        selInd <- grep("UNadj_constrained", folderLst)
        if (length(selInd) == 0) {
          selInd <- grep("constrained", folderLst)[1]
        }
      }
    }
    if (length(selInd) == 0) {
      stop_quietly("You exit the function. No file has been downloaded.")
    } else if (0 %in% selInd) {
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    # } else if (length(selInd) > 1) {
    #   message("Multiple selection is not allowed.")
    #   downloadProcess <- TRUE
    #   pathFTP <- pathFTP0
    } else if (!all(selInd %in% nFile)) {
      message("Invalid index.")
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    }else{
      logTxt <- file.path(mainPath, location, "data", "log.txt")
      timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
      pathPop <- file.path(pathPop, timeFolder, "raw")
      check_path_length(pathPop)
      dir.create(pathPop, recursive = TRUE)
      
      for (i in selInd) {
        filePath <- paste0(pathFTP, folderLst[i])
        check_path_length(file.path(pathPop, folderLst[i]))
        utils::download.file(url = filePath, destfile = file.path(pathPop, folderLst[i]), quiet = FALSE, mode = "wb", method = "libcurl")
        write(paste0(Sys.time(), ": Population raster downloaded from ", filePath, " - Input folder ", timeFolder), file = logTxt, append = TRUE)
        cat(paste0("Done: ", pathPop, "/", folderLst[i], "\n"))
      }
      downloadProcess <- FALSE
    }
  }
  return(TRUE)
}
