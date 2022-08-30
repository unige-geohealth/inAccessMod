#' Multiple travel scenarios
#'
#' Function that allows to handle different travel scenarios for different administrative units. It creates an updated 
#' merged landcover and an updated travel scenario table.
#' @param inputFolder character; the path to the input folder, which must contains an administrative unit shapefile, a merged 
#' landcover imported from AccessMod and an Excel or CSV table for each travel scenario.
#' @param adminLayerName character; the name of the administrative unit layer (without extension)
#' @param landcoverFile character; the file name of the original merged landcover (with extension)
#' @details The function main steps are the following:
#' * Check of the tables that are in the input folder: missing data, column names, values available for all landcover classes, values for the "mode" column, only one value per class, and speed in numerical format and positive or equal to zero.
#' * Console printing of the shapefile attribute table, and selection of the column used to determine the administrative unit. 
#' * Interactive selection of the scenario (based on the table names) for each administrative unit,  and creation of a table that relates the units and the scenarios.
#' * Loop over each administrative unit
#' + Copy of the corresponding travel scenario table
#' + Reclassification (sequentially, taking into account the last value assigned for the scenario of the previous unit)
#' + Append the administrative unit name (or code) to the classes' labels
#' + Landcover raster clip and reclassification of the raster values (consistent with the previous reclassification)
#' + Save the new raster (in a list), save the new table (in a list)
#' * Merging of the rasters of each unit (in case of overlap, the values get priority in the same order as the arguments), and merging of the tables of each unit.
#' * Writing the final raster, the final table, and the table that relates the different administrative units and the different travel scenarios. The final number of classes are N-classes x N-units.
#' @export
multi_ts <- function (inputFolder, adminLayerName, landcoverFile) {
  requiredPckgs <- c("terra", "sf", "readxl", "writexl", "tibble")
  for (i in 1:length(requiredPckgs)) {
    if (!requiredPckgs[i] %in% rownames(installed.packages())) {
      stop(paste(requiredPckgs[i], "package is missing."))
    }
  }
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
  rastLst <- vector("list", nrow(zoneScenario))
  scenarioLst <- vector("list", nrow(zoneScenario))
  classVal <- 0
  for (i in 1:nrow(zoneScenario)) {
    zone <- zoneScenario[i, 1, drop = TRUE]
    cat(paste("\nProcessing zone:", zone))
    scenario <- zoneScenario[i, 2, drop = TRUE]
    subAdmin <- admin[as.data.frame(admin)[, colUnit] %in% zone, ]
    maskedLc <- terra::mask(landcover, as(subAdmin, "SpatVector"))
    ts <- xlsLst[[scenario]]
    nClass <- nrow(ts)
    oldClass <- ts$class
    newClass <- (classVal + 1):(classVal + nClass)
    ts$class <- newClass
    classVal <- classVal + nClass
    ts$label <- paste0(ts$label, "_", zone)
    scenarioLst[[i]] <- ts
    newRas <- terra::subst(maskedLc, from = oldClass, to = newClass)
    rastLst[[i]] <- newRas
  }
  message("\nMerging...")
  finalLandcover <- do.call(terra::merge, rastLst)
  finalScenario <- do.call(rbind, scenarioLst)
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste0(inputFolder, "/out/", timeFolder)
  dir.create(outFolder, recursive = TRUE)
  message("Writing new merged landcover raster...")
  terra::writeRaster(finalLandcover, filename = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"))
  message("Writing new travel scenario table...")
  writexl::write_xlsx(finalScenario, path = paste(outFolder, "multi_ts.xlsx", sep = "/"), col_names = TRUE)  
  message("Writing travel scenarios/zones relationship table...")
  writexl::write_xlsx(zoneScenario, path = paste(outFolder, "zones_ts.xlsx", sep = "/"), col_names = TRUE)  
  cat(paste("Output folder:", outFolder))
}