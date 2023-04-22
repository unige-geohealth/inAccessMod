#' Download Open Street Map Layers
#'
#' Download Open Street Map shapefiles from the Geofabrik's free download server that correspond to 'roads', 'rivers' or any other natural feature and copy them
#' to their corresponding folders.
#' @param x character; target layer. Can be 'roads', 'waterLines' or 'naturalPolygons'
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param alwaysDownload logical; should the shapefile always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the shapefile has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param countryName logical; should the country name be used to match with the \code{osm.pbf} file? If FALSE, it is the extent
#' of the boundary shapefile that is matched with the \code{osm.pbf} file.
#' @param mostRecent logical; should the most recent boundary shapefile be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time. Ignored if countryName is TRUE.
#' @param defaultClasses logical; should only the default classes be kept? If not, the user is interactively asked to select the categories
#' they want to keep. For 'roads', default classes are: trunk, trunk_link, primary, primary_link, 
#' motorway, motorway_link, secondary, secondary_link, tertiary, tertiary_link, road, raceway, residential, 
#' living_street, service, track, pedestrian, path, footway, piste, bridleway, cycleway, steps, unclassified and bridge. For waterLines and naturalPolygons, 
#' default classes are river and water, respectively.    
#' @export
download_osm <- function (x, mainPath, country, alwaysDownload = FALSE, countryName = TRUE, mostRecent = NULL, defaultClasses = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!(x == "roads" | x == "waterLines" | x == "naturalPolygons")) {
    stop("x must be 'roads', 'waterLines' or 'naturalPolygons'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(countryName)) {
    stop("countryName must be 'logical'")
  }
  if (!countryName) {
    if (is.null(mostRecent)) {
      mostRecent <- FALSE
    }
    if (!is.logical(mostRecent)){
      stop("mostRecent must be NULL or 'logical'")
    }
  }
  if(!is.logical(defaultClasses)){
    stop("defaultClasses must 'logical'")
  }
  pathFolder <- file.path(mainPath, country, "data", paste0("v", stringr::str_to_title(x)))
  folders <- check_exists(pathFolder, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  if (x == "roads") {
    querySQL <- "SELECT * FROM 'lines' WHERE highway IS NOT NULL"
    colName <- "highway"
    classes <- c("trunk", 
                    "trunk_link",
                    "primary",
                    "primary_link",
                    "motorway",
                    "motorway_link",
                    "secondary",
                    "secondary_link",
                    "tertiary",
                    "tertiary_link",
                    "road",
                    "raceway",
                    "residential",
                    "living_street",
                    "service",
                    "track",
                    "pedestrian",
                    "path",
                    "footway",
                    "piste",
                    "bridleway",
                    "cycleway",
                    "steps",
                    "unclassified",
                    "bridge")
  } else if (x == "waterLines") {
    querySQL <- "SELECT * FROM 'lines' WHERE waterway IS NOT NULL"
    colName <- "waterway"
    classes <- "river"
  } else {
    querySQL <- "SELECT * FROM 'multipolygons' WHERE natural IS NOT NULL"
    colName <- "natural"
    classes <- "water"
  }
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  pathFolder <- file.path(pathFolder, timeFolder, "raw")
  check_path_length(pathFolder)
  dir.create(pathFolder, recursive = TRUE)
  # Download
  if (countryName) {
    countryName <- get_param(mainPath, country, "COUNTRY")
    shp <- osmextract::oe_get(countryName,
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  } else {
    border <- get_boundaries(mainPath, country, "raw", mostRecent)
    # Is the raw boundary in lon lat ?
    if (terra::linearUnits(as(border, "SpatVector")) != 0) {
      # Projection transformation
      border <- sf::st_transform(border, crs = "+proj=longlat +datum=WGS84")
    }
    # Download 
    shp <- osmextract::oe_get(sf::st_bbox(border),
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }
  shpCat <- select_categories(shp, colName, defaultClasses, classes)
  shp <- shpCat[[1]]
  categ <- shpCat[[2]]
  if (x == "roads") {
    classLab <- data.frame(class = seq(1000,1000+length(categ)-1), highway = categ)
    shp$class <- classLab$class[match(shp$highway, classLab$highway)]
  }
  shapeName <- gsub("\\.gpkg$", "", list.files(pathFolder)[grepl("\\.gpkg$", list.files(pathFolder))])
  check_path_length(file.path(pathFolder, paste0("/v", stringr::str_to_title(x), "_", shapeName, ".shp")))
  suppressWarnings(sf::st_write(shp, file.path(pathFolder, paste0("/v", stringr::str_to_title(x), "_", shapeName, ".shp")), append=FALSE)) # Save the layer
  logTxt <- file.path(mainPath, country, "data", "log.txt")
  write(paste0(Sys.time(), ": ", x, " downloaded from OSM; ", paste(categ, collapse = ", "), "- Input folder ", timeFolder), file = logTxt, append = TRUE)
  file.remove(file.path(pathFolder, list.files(pathFolder)[grepl("\\.gpkg$|\\.pbf$", list.files(pathFolder))]))
  cat(paste0("Done: ", pathFolder, "/v", stringr::str_to_title(x), "_", shapeName,".shp", "\n"))
}
