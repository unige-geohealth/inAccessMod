#' Download Open Street Map shapefiles
#'
#' Download Open Street map shapefiles corresponding to 'roads', 'rivers' or any other natural feature and copy them
#' to their corresponding folders.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param alwaysDownload logical; should the shapefile always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the shapefile has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param countryName logical; should the country name be used to match with the \code{osm.pbf} file? If FALSE, it is the extent
#' of the boundary shapefile that is matched with the \code{osm.pbf} file.
#' @export
download_osm <- function (x, mainPath, region, alwaysDownload = FALSE, countryName = TRUE, mostRecent = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!(x == "roads" | x == "waterLines" | x == "waterPolygons")) {
    stop("x must be 'roads', 'waterLines' or 'waterPolygons'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(countryName)) {
    stop("countryName must be 'logical'")
  }
  if (!countryName) {
    if (is.null(mostRecent)) {
      stop("mostRecent is required when countryName is FALSE")
    }
    if (!is.logical(mostRecent)){
      stop("mostRecent must be 'logical'")
    }
  }
  pathFolder <- paste0(mainPath, "/", region, "/data/v", stringr::str_to_title(x))
  folders <- check_exists(pathFolder, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  if (x == "roads") {
    querySQL <- "SELECT * FROM 'lines' WHERE highway IS NOT NULL"
    colName <- "highway"
  }else if (x == "waterLines") {
    querySQL <- "SELECT * FROM 'lines' WHERE waterway IS NOT NULL"
    colName <- "waterway"
  }else{
    querySQL <- "SELECT * FROM 'multipolygons' WHERE natural IS NOT NULL"
    colName <- "natural"
  }
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathFolder, "/", timeFolder, "/raw"), recursive = TRUE)
  pathFolder <- paste0(pathFolder, "/", timeFolder, "/raw")
  # Download
  if (countryName) {
    countryName <- get_param(mainPath, region, "COUNTRY")
    shp <- osmextract::oe_get(countryName,
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }else{
    message("\nLoading raw boundary shapefile...")
    border <- get_boundaries(mainPath, region, "raw", mostRecent)
    # Download 
    shp <- osmextract::oe_get(st_bbox(border),
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }
  shpCat <- select_categories(shp, colName)
  shp <- shpCat[[1]]
  categ <- shpCat[[2]]
  shapeName <- gsub("\\.gpkg$", "", list.files(pathFolder)[grepl("\\.gpkg$", list.files(pathFolder))])
  sf::st_write(shp, paste0(pathFolder, "/v", stringr::str_to_title(x), "_", shapeName, ".shp"), append=FALSE) # Save the layer
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  write(paste0(Sys.time(), ": ", x, " downloaded from OSM; ", paste(categ, collapse = ", "), "- Input folder ", timeFolder), file = logTxt, append = TRUE)
  file.remove(paste0(pathFolder, "/", list.files(pathFolder)[grepl("\\.gpkg$|\\.pbf$", list.files(pathFolder))]))
  cat(paste0(pathFolder, "/v", stringr::str_to_title(x), "_", shapeName,".shp", "\n"))
}