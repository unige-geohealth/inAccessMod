#' Get project parameters
#'
#' Internal function that is used to access the config.txt of the project and get a specific parameter (e.g. ISo alpha-3 code).
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param param character; 'COUNTRY' to get the original country name, 'ISO' to get the ISO alpha-3 country code or 'EPSG'
#' to get the EPSG code for layer projection
#' @export
get_param <- function (mainPath, region, param) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.character(param)) {
    stop("param must be 'character'")
  } else {
    if (!param %in% c("COUNTRY", "ISO", "EPSG")) {
      stop("param must be 'COUNTRY', 'ISO', 'EPSG'")
    }
  }
  pathRegion <- paste0(mainPath, "/", region, "/data")
  if (!file.exists(paste0(pathRegion, "/config.txt"))) {
    stop("Project main parameters have not been set yet. Run the initiate_project function.")
  }
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  param <- config[grepl(param, config)]
  param <- gsub("^[A-z]*\\:", "", param)
  return(param)
}