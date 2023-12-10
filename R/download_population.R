#' Download Population Raster
#'
#' Download a population raster from the World Pop FTP and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param alwaysDownload logical; should the raster always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the raster has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param testMode logical; to be ignored. Only used when testing the function with testthat.
#' @details The function accesses the World Pop FTP and uses an internal function (\code{navigate_ftp}) in order to interactively navigate 
#' through the folders and select the population raster to be downloaded. The ISO code retrieved internally by the \code{get_param} function 
#' is used to match the country FTP folder when available.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myCountry with the country name you are working on (workDir subfolder)
#' \dontrun{
#' country <- "myCountry"
#' download_population(mainPath, country, alwaysDownload = TRUE)}
#' @export
download_population <- function (mainPath, country, alwaysDownload = FALSE, testMode = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  # Check directory
  pathPop <- file.path(mainPath, country, "data", "rPopulation")
  folders <- check_exists(pathPop, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  iso <- get_param(mainPath, country, "ISO")
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
      out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0, testMode = TRUE)
    } else {
      out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0, testMode = FALSE) 
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
      cat(indFile)
      cat("\n\nSelect file (corresponding number) to be downloaded.\nType zero to get back to the root directory.\nSkip to exit the function.")
      cat("\n ")
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
      print(selInd)
    }
    if (length(selInd) == 0) {
      stop_quietly("You exit the function. No file has been downloaded.")
    }else if (0 %in% selInd) {
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
      logTxt <- file.path(mainPath, country, "data", "log.txt")
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
