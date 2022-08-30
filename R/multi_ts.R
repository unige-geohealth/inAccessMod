#' Multiple travel scenarios
#'
#' Function that allows to handle different travel scenarios for different administrative units. It creates an updated 
#' merged landcover and an updated travel scenario table. The required inputs are an administrative shapefile, a merged landcover
#' imported from AccessMod, and an Excel (or CSV) table for each travel scenario. The function requires a folder that only
#' contains the required inputs. When running the function, the user is asked to select the attribute table column of 
#' the shapefile that refers to each administrative unit, and then to select the travel scenario for each administrative unit.
#' @param inputFolder character; the path to the input folder, which must contains an administrative unit shapefile, a merged 
#' landcover imported from AccessMod and an Excel or CSV table for each travel scenario.
#' @param adminLayerName character; the name of the administrative unit layer (without extension)
#' @param landcoverFile character; the file name of the original merged landcover (with extension)
#' @details An output folder called out is created within the input folder, as well as a subfolder whose name 
#' is based on the system time that contains three outputs (e.g. ./out/20220826104842):
#' \itemize{
#' \item The updated landcover raster
#' \item The updated travel scenario table
#' \item A table that relates the different administrative units to the different travel scenarios
#' }
#' The function works are the following:
#' \itemize{
#' \item Check of the tables that are in the input folder: missing data, column names, values available for all landcover classes, values for the "mode" column, only one value per class, and speed in numerical format and positive or equal to zero.
#' \item Console printing of the shapefile attribute table, and selection of the column used to determine the administrative unit. 
#' \item Interactive selection of the scenario (based on the table names) for each administrative unit,  and creation of a table that relates the units and the scenarios.
#' \item Loop over each administrative unit
#' \itemize{
#' \item Copy of the corresponding travel scenario table
#' \item Reclassification (sequentially, taking into account the last value assigned for the scenario of the previous unit)
#' \item Append the administrative unit name (or code) to the classes' labels
#' \item Landcover raster clip and reclassification of the raster values (consistent with the previous reclassification)
#' \item Save the new raster, save the new table (in a list)
#' }
#' \item Merging of the rasters of each unit (in case of overlap, the values get priority in the same order as the arguments), and merging of the tables of each unit.
#' \item Writing the final raster, the final table, and the table that relates the different administrative units and the different travel scenarios. The final number of classes are N-classes x N-units.
#' }
#' @export
multi_ts <- function (inputFolder, adminLayerName, landcoverFile) {
  if (!is.character(inputFolder)) {
    stop("inputFolder must be 'character'")
  }
  if (!dir.exists(inputFolder)) {
    stop(paste(inputFolder, "does not exist"))
  }
  if (!is.character(adminLayerName)) {
    stop("adminLayerName must be 'character'")
  }
  if (!is.character(landcoverFile)) {
    stop("landcoverFile must be 'character'")
  }
  admin <- sf::st_read(inputFolder, adminLayerName)
  landcover <- terra::rast(paste(inputFolder, landcoverFile, sep = "/"))
  vLc <- terra::values(landcover)[,1]
  vLc <- unique(vLc[!is.na(vLc)])
  vLc <- vLc[order(vLc)]
  xls <- list.files(inputFolder, pattern = "\\.xls|\\.csv", full.names = TRUE)
  xls <- xls[!grepl("\\~\\$", xls)]
  if (any(duplicated(gsub("\\.xls|\\.xlsx|\\.csv", "", xls)))) {
    stop("Duplicated names for travel scenario tables.")
  }
  if (length(xls) == 0) {
    stop("No Excel file (travel scenario) in the input folder.")
  }
  xlsLst <- vector("list", length = length(xls))
  for (i in 1:length(xls)) {
    if (grepl("\\.xlsx$", xls[i])) {
      xlsi <- readxl::read_xlsx(xls[i])
    } else if (grepl("\\.xls$", xls[i])){
      xlsi <- readxl::read_xls(xls[i])
    } else {
      xlsi <- tibble::as_tibble(read.csv(xls[i], header = TRUE))
    }
    xlsiFile <- xls[i]
    check_ts_table(xlsi, xlsiFile, vLc)
    xlsLst[[i]] <- xlsi[order(xlsi$class), ]
  }
  xlsNames <- list.files(inputFolder, pattern = "\\.xls|\\.xlsx|\\.csv", full.names = FALSE)
  xlsNames <- gsub("\\.xls|\\.xlsx|\\.csv", "", xlsNames)
  names(xlsLst) <- xlsNames
  cols <- colnames(admin)
  print(as.data.frame(admin))
  colUnit <- utils::menu(cols, title = "\nSelect the column that you would like to use for referring to the different administrative units.")
  adminUnits <- unique(as.data.frame(admin)[, colUnit])
  if (length(adminUnits) == 1) {
    stop("Selected column have only one value.")
  }
  adminUnits <- adminUnits[order(adminUnits)]
  zoneScenario <- data.frame(Zone = adminUnits, scenario = NA)
  colnames(zoneScenario)[1] <- colnames(as.data.frame(admin))[colUnit]
  scenarios <- gsub(paste0(inputFolder, "|/|\\.xls|\\.xlsx|\\.csv"), "", xls)
  for (i in 1:nrow(zoneScenario)) {
    sc <- utils::menu(scenarios, title = paste("\nWhich scenario for", zoneScenario[i, 1], "?"))
    zoneScenario[i, 2] <- scenarios[sc]
  }
  tempDir <- paste0(inputFolder, "/temp")
  if (dir.exists(tempDir)) {
    unlink(tempDir, recursive = TRUE)
  }
  dir.create(tempDir)
  scenarioLst <- vector("list", nrow(zoneScenario))
  classVal <- 0
  for (i in 1:nrow(zoneScenario)) {
    zone <- zoneScenario[i, 1, drop = TRUE]
    cat(paste("\nProcessing zone:", zone))
    scenario <- zoneScenario[i, 2, drop = TRUE]
    subAdmin <- admin[as.data.frame(admin)[, colUnit] %in% zone, ]
    maskedLc <- terra::mask(landcover, as(subAdmin, "SpatVector"), overwrite = TRUE)
    ts <- xlsLst[[scenario]]
    nClass <- nrow(ts)
    oldClass <- ts$class
    newClass <- (classVal + 1):(classVal + nClass)
    ts$class <- newClass
    classVal <- classVal + nClass
    ts$label <- paste0(ts$label, "_", zone)
    scenarioLst[[i]] <- ts
    newRas <- terra::subst(maskedLc, from = oldClass, to = newClass)
    terra::writeRaster(newRas, file = paste0(tempDir, "/zone", i, ".tif"))
  }
  allRast <- paste0(tempDir, "/zone", 1:nrow(zoneScenario), ".tif")
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste0(inputFolder, "/out/", timeFolder)
  dir.create(outFolder, recursive = TRUE)
  message("\nMerging and writing ouptuts...")
  terra::writeRaster(landcover, file = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"), overwrite = TRUE, filetype = "GTiff")
  gdalUtils::mosaic_rasters(gdalfile = allRast, dst_dataset = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"), of="GTiff", verbose = FALSE)
  finalScenario <- do.call(rbind, scenarioLst)
  writexl::write_xlsx(finalScenario, path = paste(outFolder, "multi_ts.xlsx", sep = "/"), col_names = TRUE)  
  writexl::write_xlsx(zoneScenario, path = paste(outFolder, "zones_ts.xlsx", sep = "/"), col_names = TRUE)
  unlink(tempDir, recursive = TRUE)
  message(paste("Output folder:", outFolder, "\n"))
}
