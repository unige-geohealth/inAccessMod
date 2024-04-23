#' Query geoBoundaries
#'
#' Internal function that is used to check availability of boundary shapefile and get the json file
#' @param iso character; country ISO code
#' @param adminLevel integer; administrative level of the boundaries. From 0 to 3.
#' @param type character; data type. Can be 'gbOpen' for geoBoundaries data, 'gbHumanitarian' for UN OCHA CODs data, or
#' 'gbAuthoritative' for UN SALB data (default).
#' @param validTypes character; valid data types (see above)
#' @keywords internal
#' @export
geoboundaries_query <- function (iso, adminLevel, type, validTypes) {
  url <- paste0("https://www.geoboundaries.org/api/current/", type, "/", iso, "/ADM", adminLevel, "/")
  jsonData <- tryCatch({jsonlite::fromJSON(url)}, error = function (e) NULL)
  if (is.null(jsonData$staticDownloadLink)) {
    message("Requested data not available. Searching for available data, please wait...")
    avData <- data.frame()
    for (typ in validTypes) {
      for (lev in 0:3) {
        url <- paste0("https://www.geoboundaries.org/api/current/", typ, "/", iso, "/ADM", lev, "/")
        jsonData <- tryCatch({jsonlite::fromJSON(url)}, error = function (e) NULL)
        if (is.null(jsonData$staticDownloadLink)) {
          avData <- rbind(avData, data.frame(Type = typ, Level = lev, Availability = "NO"))
        } else {
          avData <- rbind(avData, data.frame(Type = typ, Level = lev, Availability = "YES"))
        }
      }
    }
    print(avData)
    stop_quietly("Run the download_boundaries function again with valid parameters.")
  } else {
    jsonData <- jsonlite::fromJSON(url)
    return(jsonData)
  }
}
