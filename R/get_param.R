#' Get Project Parameters
#'
#' Internal function that is used to access the config.txt of the project and get a specific parameter (e.g. ISO alpha-3 code).
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param param character; 'COUNTRY' to get the original country name, 'ISO' to get the ISO alpha-3 country code or 'EPSG'
#' to get the EPSG code for layer projection
#' @keywords internal
#' @export
get_param <- function (mainPath, location, param) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  if (!is.character(param)) {
    stop("param must be 'character'")
  } else {
    if (!param %in% c("COUNTRY", "ISO", "EPSG", "CITY")) {
      stop("param must be 'COUNTRY', 'CITY', 'ISO', 'EPSG'")
    }
  }
  pathCountry <- file.path(mainPath, location, "data")
  if (!file.exists(file.path(pathCountry, "config.txt"))) {
    stop("Project main parameters have not been set yet. Run the initiate_project function.")
  }
  fileConn <- file(file.path(pathCountry, "config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  param <- config[grepl(param, config)]
  param <- gsub("^[A-z]*\\:", "", param)
  return(param)
}
