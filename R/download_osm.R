#' Download Open Street Map Layers
#'
#' Download Open Street Map shapefiles that correspond to 'roads', 'rivers' or any other natural feature and copy them
#' to their corresponding folders.
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param type character; target layer. Can be 'roads', 'waterLines' or 'waterPolygons'
#' @param alwaysDownload logical; should the shapefile always be downloaded, even if it has already been 
#' downloaded? If FALSE and if the shapefile has already been downloaded the user is 
#' interactively asked whether they want to download it again or not.
#' @param mostRecent logical; should the most recent boundary shapefile be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @param defaultClasses logical; should only the default classes be kept? If not, the user is interactively asked to select the categories
#' they want to keep. For 'roads', default classes are: trunk, trunk_link, primary, primary_link, 
#' motorway, motorway_link, secondary, secondary_link, tertiary, tertiary_link, road, raceway, residential, 
#' living_street, service, track, pedestrian, path, footway, piste, bridleway, cycleway, steps, unclassified and bridge. For waterLines and waterPolygons, 
#' default classes are river and water, respectively.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myLocation with the location name you are working on (workDir subfolder)
#' \dontrun{
#' location <- "myLocation"
#' download_osm(mainPath, location, type = "roads", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(mainPath, location, type = "waterLines", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)
#' download_osm(mainPath, location, type = "waterPolygons", alwaysDownload = TRUE, mostRecent = NULL, defaultClasses = TRUE)}
#' @export
download_osm <- function (mainPath, location, type, alwaysDownload = FALSE, mostRecent = TRUE, defaultClasses = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  if (!(type == "roads" | type == "waterLines" | type == "waterPolygons")) {
    stop("type must be 'roads', 'waterLines' or 'waterPolygons'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be'logical'")
  }
  if (!is.logical(defaultClasses)){
    stop("defaultClasses must 'logical'")
  }
  xData <- sub("^.", toupper(substr(type, 1, 1)), type)
  pathFolder <- file.path(mainPath, location, "data", paste0("v", xData))
  folders <- check_exists(pathFolder, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  if (type == "roads") {
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
  } else if (type == "waterLines") {
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
  border <- get_boundaries(mainPath, location, "raw", mostRecent)
  # Is the raw boundary in lon lat ?
  if (terra::linearUnits(as(border, "SpatVector")) != 0) {
    # Projection transformation
    border <- sf::st_transform(border, crs = "+proj=longlat +datum=WGS84")
  }
  # Download 
  message(paste0("Downloading OSM data (", type, ")..."))
  # osmdata or osmextract (osmdata works well for small areas like cities)
  city <- get_param(mainPath, location, "CITY")
  osmData <- NULL
  if (length(city) > 0) {
    osmData <- tryCatch({
      osmdata::opq(bbox = sf::st_bbox(border)) %>%
        osmdata::add_osm_feature(key = colName, value = classes) %>%
        osmdata::osmdata_sf()
    }, error = function(e) {
      message("An error occurred with osmdata: ", e$message, "\nTrying now with osmextract...")
      return(NULL)  # Return NULL or some other default value if there's an error
    })
  }
  if (!is.null(osmData)) {
    if (type == "roads" | type == "waterLines") {
      shp <- osmData$osm_lines
    } else {
      shp <- osmData$osm_multipolygons
    }
  } else {
    # Either we don't have city, or osmdata didn't work
    # Get right country name for osmextract
    iso3 <- get_param(mainPath, location, "ISO")
    iso2 <- inAccessMod::country_list$iso2c[inAccessMod::country_list$iso3c == iso3]
    place <- osmextract::geofabrik_zones$name[which(osmextract::geofabrik_zones$iso3166_1_alpha2 == iso2)]
    shp <- tryCatch({
      osmextract::oe_get(place,
                         quiet = FALSE,
                         query = querySQL,
                         download_directory = pathFolder,
                         force_download = TRUE,
                         max_file_size = 5e+10)
    }, error = function(e) {
      message("An error occurred with name (", place, "): ", e$message, "\nTrying now with bbox...")
      return(NULL)  # Return NULL or some other default value if there's an error
    })
    if (is.null(shp)) {
      # Takes theoretically more time
      shp <- osmextract::oe_get(sf::st_bbox(border),
                                quiet = FALSE,
                                query = querySQL,
                                download_directory = pathFolder,
                                force_download = TRUE,
                                max_file_size = 5e+10)
    }

  }
  shpCat <- select_categories(shp, colName, defaultClasses, classes)
  shp <- shpCat[[1]]
  categ <- shpCat[[2]]
  if (type == "roads") {
    classLab <- data.frame(class = seq(1000, 1000 + length(categ) - 1), highway = categ)
    shp$class <- classLab$class[match(shp$highway, classLab$highway)]
  }
  check_path_length(file.path(pathFolder, paste0("/v", xData, "_OSM.shp")))
  suppressWarnings(sf::st_write(shp, file.path(pathFolder, paste0("/v", xData, "_OSM.shp")), append = FALSE)) # Save the layer
  logTxt <- file.path(mainPath, location, "data", "log.txt")
  write(paste0(Sys.time(), ": ", type, " downloaded from OSM; ", paste(categ, collapse = ", "), "- Input folder ", timeFolder), file = logTxt, append = TRUE)
  cat(paste0("Done: ", pathFolder, "/v", xData, "_OSM.shp", "\n"))
  return(TRUE)
}
