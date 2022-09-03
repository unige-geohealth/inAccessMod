#' Multiple travel scenarios (alternative to multi_ts)
#'
#' Function that allows to handle different travel scenarios for different administrative units. It is faster than multi_ts and
#' recommended when the outputs of this latter can not be well processed in AccessMod. It creates an updated 
#' merged landcover and an updated travel scenario table (potentially much smaller that the one created by multi_ts). 
#' As for multi_ts, the required inputs are an administrative shapefile, a merged landcover
#' imported from AccessMod, and an Excel (or CSV) table for each travel scenario. The function requires a folder that only
#' contains the required inputs. When running the function, the user is asked to select the attribute table column of 
#' the shapefile that refers to each administrative unit (with no special character), and then to select the travel scenario for each administrative unit.
#' When modifying a scenario, the user must re-run the function and should not modify the outputs directly.
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
#' \item Loop over each scenario
#' \itemize{
#' \item Landcover raster clip based on the administrative units that are related to the scenario i.
#' \item Check if the scenario i has entries that are identical to entries from already processed scenarios
#' \item Creation of new classes if necessary
#' \item Reclassification of the raster depending on the two previous point
#' \item Update of the final travel scenario table
#' }
#' \item Merging of the rasters
#' \item Writing the final raster, the final table, and the table that relates the different administrative units and the different travel scenarios..
#' }
#' @export
multi_ts_fast <- function (inputFolder, adminLayerName, landcoverFile) {
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
  # admin$shapeName <- paste0("Admin", 1:nrow(admin))
  vLc <- terra::values(landcover)[, 1]
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
    xlsLst[[i]] <- as.data.frame(xlsi[order(xlsi$class), ])
  }
  xlsNames <- list.files(inputFolder, pattern = "\\.xls|\\.xlsx|\\.csv", full.names = FALSE)
  xlsNames <- gsub("\\.xls|\\.xlsx|\\.csv", "", xlsNames)
  names(xlsLst) <- xlsNames
  cols <- colnames(admin)
  print(sf::st_drop_geometry(admin))
  colUnit <- utils::menu(cols, title = "\nSelect the column (WITH NO SPECIAL CHARACTER LIKE APOSTROPHES, ACCENTS, etc.) that you would like to use for referring to the different administrative units.")
  adminUnits <- unique(admin[[colUnit]])
  if (length(adminUnits) == 1) {
    stop("Selected column have only one value.")
  }
  adminUnits <- adminUnits[order(adminUnits)]
  zoneScenario <- data.frame(Zone = adminUnits, scenario = NA)
  colnames(zoneScenario)[1] <- colnames(sf::st_drop_geometry(admin))[colUnit]
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
  finalScenario <- xlsLst[[1]]
  finalScenario$newlabel <- character(nrow(finalScenario))
  finalScenario$newclass <- character(nrow(finalScenario))
  for (i in 1:length(xlsLst)) {
    message(names(xlsLst)[i])
    # Units for this scenario
    zones <- zoneScenario[, 1][zoneScenario$scenario == names(xlsLst)[i]]
    newShp <- admin[as.data.frame(admin)[, colUnit] %in% zones, ]
    newRas <- terra::mask(landcover, as(newShp, "SpatVector"))
    # Get this scenario
    newSc <- xlsLst[[i]]
    # If first 
    if (i == 1) {
      newVal <- 1:nrow(newSc)
      newRas <- terra::subst(newRas, from = newSc$class, to = newVal)
      terra::writeRaster(newRas, file = paste0(tempDir, "/scenario", i, ".tif"))
      finalScenario$newclass <- newVal
      finalScenario$newlabel <- paste0(finalScenario$label, "_", i)
    } else {
      matchClass <- plyr::match_df(newSc, finalScenario, on = c("label", "speed", "mode"))[, 1]
      noMatch <- newSc[!newSc$class %in% matchClass, ]
      recode <- plyr::match_df(finalScenario, newSc, on = c("label", "speed", "mode"))[, c("class", "newclass")]
      if (nrow(noMatch) > 0) {
        lastVal <- finalScenario$newclass[length(finalScenario$newclass)]
        newVal <- (lastVal + 1):(lastVal + nrow(noMatch))
        noMatchRecode <- data.frame(class = noMatch$class, newclass = newVal)
        recode <- rbind(recode, noMatchRecode)
        noMatch$newclass <- newVal
        noMatch$newlabel <- paste0(noMatch$label, "_", i)
        finalScenario <- rbind(finalScenario, noMatch)
      }
      newRas <- terra::subst(newRas, from = recode$class, to = recode$newclass)
      terra::writeRaster(newRas, file = paste0(tempDir, "/scenario", i, ".tif"))
    }
  }
  allRast <- paste0(tempDir, "/scenario", 1:length(xlsLst), ".tif")
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste0(inputFolder, "/out/", timeFolder)
  dir.create(outFolder, recursive = TRUE)
  message("\nMerging and writing ouptuts...")
  terra::writeRaster(landcover, file = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"), overwrite = TRUE, filetype = "GTiff")
  gdalUtils::mosaic_rasters(gdalfile = allRast, dst_dataset = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"), of="GTiff", verbose = FALSE)
  
  finalScenario <- finalScenario[, c(6, 5, 3, 4)]
  colnames(finalScenario) <- c("class", "label", "speed", "mode")
  finalScenario$speed <- as.integer(finalScenario$speed)
  finalScenario$class <- as.integer(finalScenario$class)
  
  writexl::write_xlsx(finalScenario, path = paste(outFolder, "multi_ts.xlsx", sep = "/"), col_names = TRUE)  
  writexl::write_xlsx(zoneScenario, path = paste(outFolder, "zones_ts.xlsx", sep = "/"), col_names = TRUE)
  unlink(tempDir, recursive = TRUE)
  message(paste("Output folder:", outFolder, "\n"))
}
