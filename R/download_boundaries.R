#' Download Administrative Boundaries
#'
#' Download the administraive boundary shapefile from \emph{geoboundaries} and copy it to its corresponding folder.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param adminLevel integer; administrative level of the boundaries. From 0 to 5
#' @param alwaysDownload logical; should the administrative boundaries always be downloaded, even if they have already been 
#' downloaded? If FALSE and if the administrative boundary shapefile has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @details The ISO code used to download the shapefile is retrieved by the internal \code{get_param} function. If the the administrative level
#' is not available, the function tries a lower one and so on. Metadata file is also downloaded.
#' @references Runfola, D. et al. (2020) geoBoundaries: A global database of political administrative boundaries. PLoS ONE 15(4): 
#' e0231866. https://doi.org/10.1371/journal.pone.0231866
#' @export
download_boundaries <- function (mainPath, country, adminLevel, alwaysDownload = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!adminLevel %in% c(0,1,2,3,4,5)) {
    stop("Administrative level must be an integer from 0 to 5")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  # Check directory
  pathBorder <- paste0(mainPath, "/", country, "/data/vBorders")
  folders <- check_exists(pathBorder, "raw")
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  # Get country code
  iso <- get_param(mainPath, country, "ISO")
  # Download the data
  border <- NULL
  adminLevelTry <- adminLevel
  while (is.null(border) & adminLevelTry >= 0) {
    message(paste("Trying", country, "administrative level", adminLevelTry))
    border <- tryCatch({rgeoboundaries::geoboundaries(iso, adm_lvl = adminLevelTry, quiet = FALSE)}, error = function(e){NULL})
    adminLevelTry <- adminLevelTry - 1
  }
  if (is.null(border)) {
    stop("No available shapefile from geoBoundaries for this country. You might have to download it manually.\n\n")
  }

  adminLevelTry <- adminLevelTry + 1
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathBorder, "/", timeFolder, "/raw"), recursive = TRUE)
  pathBorder <- paste0(pathBorder, "/", timeFolder, "/raw")
  borderMeta <- rgeoboundaries::gb_metadata(iso, adm_lvl = adminLevelTry)
  # Save metadata
  write.table(borderMeta,paste0(pathBorder, "/", borderMeta$boundaryID, ".txt"))
  # Save shapefile
  sf::st_write(border, paste0(pathBorder, "/", borderMeta$boundaryID, ".shp"), append = FALSE)
  logTxt <- paste0(mainPath, "/", country, "/data/log.txt")
  write(paste0(sysTime, ": Boundaries downloaded from OSM (admin level ", adminLevelTry, ") - Input folder ", timeFolder), file = logTxt, append = TRUE)
  cat(paste0(pathBorder, "/", borderMeta$boundaryID, ".shp", "\n"))
}
