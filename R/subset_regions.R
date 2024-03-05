#' Subset Regions
#'
#' Function that allows to clip specific country regions from the 'raw' boundary shapefile when requiring analysis at the sub-national level
#' @param mainPath character; the parent directory of the location folder
#' @param location character; the location folder name
#' @param mostRecent logical; should the most recent boundary shapefile be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' 
#' # Replace myLocation with the location name you are working on (workDir subfolder)
#' \dontrun{
#' location <- "myLocation"
#' download_boundaries(mainPath, location, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)
#' subset_regions(mainPath, location, mostRecent = TRUE)}
#' @export
subset_regions <- function (mainPath, location, mostRecent = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(location)) {
    stop("location must be 'character'")
  }
  border <- get_boundaries(mainPath, location, "raw", mostRecent)
  cols <- colnames(border)
  print(sf::st_drop_geometry(border))
  colUnit <- utils::menu(cols, title = "\nSelect the column that you would like to use for subsetting the regions.")
  adminUnits <- unique(border[[colUnit]])
  if (length(adminUnits) == 1) {
    stop("Selected column have only one value.")
  }
  nCat <- 1:length(adminUnits)
  indCat <- paste(paste0("\n", nCat, ": ", adminUnits))
  cat(indCat)
  cat(paste("\n\nSelect the regions that you would like to focus on.\nOn the same line separated by a space, or just skip to select all options.\n"))
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
  if (length(selInd) == 0){
    selInd <- nCat
  }
  message(paste(adminUnits[selInd], collapse = ", "))
  border <- border[sf::st_drop_geometry(border)[, colUnit] %in% adminUnits[selInd], ]
  # Save shapefile
  pathBorder <- file.path(mainPath, location, "data", "vBorders")
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  check_path_length(paste0(pathBorder, "/", timeFolder, "/raw"))
  dir.create(paste0(pathBorder, "/", timeFolder, "/raw"), recursive = TRUE)
  pathBorder <- file.path(pathBorder, timeFolder, "raw")
  sf::st_write(border, file.path(pathBorder, "vBorders_sub.shp"), append = FALSE)
  logTxt <- file.path(mainPath, location, "data", "log.txt")
  write(paste0(Sys.time(), ": Boundaries subsetted - Input folder ", timeFolder, " - ", paste(adminUnits[selInd], collapse = ", ")), file = logTxt, append = TRUE)
  cat(paste0(pathBorder, "/vBorders_sub.shp", "\n"))
}
