#' Download Administrative Boundaries
#'
#' Download the administraive boundary shapefile from \emph{geoboundaries} and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param adminLevel integer; administrative level of the boundaries. From 0 to 3.
#' @param type character; data type. Can be 'gbOpen' for geoBoundaries data, 'gbHumanitarian' for UN OCHA CODs data, or
#' 'gbAuthoritative' for UN SALB data (default).
#' @param alwaysDownload logical; should the administrative boundaries always be downloaded, even if they have already been 
#' downloaded? If FALSE and if the administrative boundary shapefile has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @details The ISO code used to download the shapefile is retrieved by the internal \code{get_param} function. Another internal function
#' will query the geoBoundaries database (geoboundaries_query) to check availability and retrieve a json file that contains the download link.
#' @references Runfola, D. et al. (2020) geoBoundaries: A global database of political administrative boundaries. PLoS ONE 15(4): 
#' e0231866. https://doi.org/10.1371/journal.pone.0231866
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath, country)}
#' 
#' # Replace myCountry with the country name you are working on (workDir subfolder)
#' \dontrun{
#' country <- "myCountry"
#' download_boundaries(mainPath, country, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)}
#' @export
download_boundaries <- function (mainPath, country, adminLevel, type = "gbAuthoritative", alwaysDownload = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!adminLevel %in% c(0,1,2,3)) {
    stop("Administrative level must be an integer from 0 to 3")
  }
  if (!is.character(type)) {
    stop("type must be 'character'")
  }
  validTypes <- c("gbOpen", "gbHumanitarian", "gbAuthoritative")
  if (!type %in% validTypes) {
    stop("type can be 'gbOpen', 'gbHumanitarian', or 'gbAuthoritative'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  # Check directory
  pathBorder <- file.path(mainPath, country, "data", "vBorders")
  folders <- check_exists(pathBorder, "raw")
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  # Get country code
  iso <- get_param(mainPath, country, "ISO")
  # # Download the data
  jsonData <- geoboundaries_query(iso, adminLevel, type, validTypes)
  tmpFolder <- tempfile()
  dir.create(tmpFolder)
  url <- jsonData$staticDownloadLink
  utils::download.file(url = url, destfile = paste0(tmpFolder, "/boundaries.zip"))
  utils::unzip(zipfile = paste0(tmpFolder, "/boundaries.zip"), overwrite = TRUE, exdir= tmpFolder)
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  check_path_length(paste0(pathBorder, "/", timeFolder, "/raw"))
  dir.create(paste0(pathBorder, "/", timeFolder, "/raw"), recursive = TRUE)
  pathBorder <- file.path(pathBorder, timeFolder, "raw")
  fileLst <- list.files(tmpFolder)
  check_path_length(file.path(pathBorder, fileLst[which(nchar(fileLst) == max(nchar(fileLst)))]))
  # Save shapefile
  fileLst <- list.files(tmpFolder)
  fileLst <- fileLst[!grepl("\\.zip|simplified", fileLst)]
  for (fi in fileLst) {
    file.copy(from = file.path(tmpFolder, fi), to = file.path(pathBorder, fi))
  }
  shpFile <- list.files(pathBorder, full.names = TRUE, pattern = "\\.shp")
  logTxt <- file.path(mainPath, country, "data", "log.txt")
  write(paste0(Sys.time(), ": Boundaries downloaded from geoboundaries (ADMIN-", adminLevel, "; ", type, ") - Input folder ", timeFolder), file = logTxt, append = TRUE)
  cat(paste0("Done: ", shpFile, "\n"))
  return(TRUE)
}
