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
#' @param zones_ts character; Excel or CSV table that relates the administrative units to the different scenarios. Two columns:
#' the first one has the same name than the field name of the administrative layer attribute table that refers to each administrative
#' unit, and the second one called 'scenario'. In the first all the different units (as indicated in the attribute table), in the second,
#' the names (without the extension) of the different scenario tables. If set to NULL, the user is interactively asked to assign
#' a scenario to each administrative unit, and a zones_ts table is created.
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
#' \item Interactive selection of the scenario (based on the table names) for each administrative unit, and creation of a table that relates the units and the scenarios, or loading this table if indicated.
#' \item Loop over each administrative unit
#' \itemize{
#' \item Copy of the corresponding travel scenario table
#' \item Reclassification (sequentially, taking into account the last value assigned for the scenario of the previous unit)
#' \item Append the administrative unit name (or code) to the classes' labels
#' \item Landcover raster clip and reclassification of the raster values (consistent with the previous reclassification)
#' \item Save the new raster (in a temporary directory), save the new table (in a list)
#' }
#' \item Merging of the rasters of each unit (in case of overlap, the values get priority in the same order as the arguments), and merging of the tables of each unit.
#' \item Writing the final raster, the final table, and the table that relates the different administrative units and the different travel scenarios. The final number of classes are N-classes x N-units.
#' }
#' @export
multi_ts <- function (inputFolder, adminLayerName, landcoverFile, zones_ts = NULL) {
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
  if (!is.null(zones_ts)) {
    if (!is.character(zones_ts)) {
      stop("zones_ts must be 'character'")
    }
    if (!file.exists(zones_ts)) {
      stop(paste(zones_ts, "does not exists. Set zones_ts to NULL to create it or modify the file path."))
    }
    if (!grepl("\\.csv|\\.xls", zones_ts)) {
      stop("zones_ts must refer to an Excel or CSV file.")
    }
  }
  admin <- sf::st_read(inputFolder, adminLayerName)
  landcover <- terra::rast(paste(inputFolder, landcoverFile, sep = "/"))
  vLc <- terra::values(landcover)[, 1]
  vLc <- unique(vLc[!is.na(vLc)])
  vLc <- vLc[order(vLc)]
  xls <- list.files(inputFolder, pattern = "\\.xls|\\.csv", full.names = TRUE)
  xlsNames <- list.files(inputFolder, pattern = "\\.xls|\\.xlsx|\\.csv", full.names = FALSE)
  xlsNames <- gsub("\\.xls|\\.xlsx|\\.csv", "", xlsNames)
  # xls <- xls[!grepl("\\~\\$", xls)]
  
  if (any(duplicated(gsub("\\.xls|\\.xlsx|\\.csv", "", xls)))) {
    stop("Duplicated names for travel scenario tables.")
  }
  if (length(xls) == 0) {
    stop("No Excel file (travel scenario) in the input folder.")
  }
  xlsLst <- list()
  for (i in 1:length(xls)) {
    if (grepl("\\.xlsx$", xls[i])) {
      xlsi <- tryCatch({readxl::read_xlsx(xls[i])}, error = function (e) NULL)
    } else if (grepl("\\.xls$", xls[i])){
      xlsi <- tryCatch({readxl::read_xls(xls[i])}, error = function (e) NULL)
    } else {
      # Warning = NULL; with read.csv difficult to get an error, but it gives warning if table is not well formatted or if it is a temporary
      # file (macOS)
      xlsi <- tryCatch({tibble::as_tibble(read.csv(xls[i], header = TRUE))}, error = function (e) NULL, warning = function (e) NULL)
    }
    if (is.null(xlsi)) {
      next
    }
    xlsiFile <- xls[i]
    check_ts_table(xlsi, xlsiFile, vLc)
    xlsLst[[xlsNames[i]]] <- as.data.frame(xlsi[order(xlsi$class), ])
  }
  if (is.null(zones_ts)) {
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
    scenarios <- names(xlsLst)
    for (i in 1:nrow(zoneScenario)) {
      sc <- utils::menu(scenarios, title = paste("\nWhich scenario for", zoneScenario[i, 1], "?"))
      zoneScenario[i, 2] <- scenarios[sc]
    }
  } else {
    if (grepl("\\.csv", zones_ts)) {
      zoneScenario <- read.csv(zones_ts, header = TRUE)
    } else if (grepl("\\.xlsx", zones_ts)) {
      zoneScenario <- as.data.frame(readxl::read_xlsx(zones_ts))
    } else {
      zoneScenario <- as.data.frame(readxl::read_xls(zones_ts))
    }
    if (!colnames(zoneScenario)[1] %in% colnames(admin)) {
      stop(paste(colnames(zoneScenario)[1], "is not a field of the administrative unit layer attribute table."))
    }
    colUnit <- colnames(zoneScenario)[1]
    adminUnits <- unique(admin[[colUnit]])
    if (!colnames(zoneScenario)[2] == "scenario") {
      stop("Column 2 of zones_ts must be 'scenario'")
    }
    if (!all(adminUnits %in% zoneScenario[, 1])) {
      noInfo <- adminUnits[!adminUnits %in% zoneScenario[, 1]]
      if (length(noInfo) > 1) {
        x <- "are"
      } else {
        x <- "is"
      }
      stop(paste(paste(noInfo, collapse = ", "), x, "missing the zones_ts table."))
    }
    if (!all(zoneScenario[, 2] %in% xlsNames)) {
      noInfo <- zoneScenario[, 2][!zoneScenario[, 2] %in% xlsNames]
      if (length(noInfo) > 1) {
        x <- "are"
        y <- "tables"
      } else {
        x <- "is"
        y <- "table"
      }
      stop(paste(paste(noInfo, collapse = ", "), "scenario", y, x, "missing. Please check the scenario names in the zones_ts table."))
    }
  }
  tempDir <- paste0(inputFolder, "/temp")
  if (dir.exists(tempDir)) {
    unlink(tempDir, recursive = TRUE)
  }
  dir.create(tempDir)
  scenarioLst <- vector("list", nrow(zoneScenario))
  classVal <- 0
  rasLst <- list()
  for (i in 1:nrow(zoneScenario)) {
    zone <- zoneScenario[i, 1, drop = TRUE]
    cat(paste("\nProcessing zone:", zone))
    scenario <- zoneScenario[i, 2, drop = TRUE]
    subAdmin <- admin[admin[[colUnit]] %in% zone, ]
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
    # List and write (list ready if mosaic_gdal doesn't work)
    rasLst[[i]] <- newRas
    terra::writeRaster(newRas, file = paste0(tempDir, "/zone", i, ".tif"))
  }
  allRast <- paste0(tempDir, "/zone", 1:nrow(zoneScenario), ".tif")
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste0(inputFolder, "/out/", timeFolder)
  dir.create(outFolder, recursive = TRUE)
  message("\nMerging and writing ouptuts...")
  mosaicGDAL <- tryCatch({gdalUtils::mosaic_rasters(gdalfile = allRast, dst_dataset = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"), of = "GTiff", verbose = FALSE)}, error = function (e) 0, warning = function (e) 0)
  if (!is.null(mosaicGDAL) && mosaicGDAL == 0) {
    message("GDAL library not found/issues -> mosaicking the tiles using the terra::merge function (slower)...")
    newRas <- tryCatch({do.call(terra::merge, rasLst)}, error = function (e) NULL)
    if (is.null(newRas)) {
      message("Memory issues: Too large ? Trying to mosaicking the tiles incrementally...")
      newRas <- do.call(terra::merge, rasLst[1:2])
      rasLst <- rasLst[-c(1:2)]
      while(length(rasLst) > 0) {
        rasLst[[length(rasLst) + 1]] <- newRas
        newRas <- do.call(terra::merge, rasLst[c(1, length(rasLst))])
        rasLst <- rasLst[-c(1, length(rasLst))]
      }
    }
    terra::writeRaster(newRas, paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"))
  }
  finalScenario <- do.call(rbind, scenarioLst)
  # finalScenario2 <- finalScenario
  # finalScenario2$label <- paste0("label_", 1:nrow(finalScenario2))
  # writexl::write_xlsx(finalScenario2, path = paste(outFolder, "multi_ts2.xlsx", sep = "/"), col_names = TRUE) 
  writexl::write_xlsx(finalScenario, path = paste(outFolder, "multi_ts.xlsx", sep = "/"), col_names = TRUE)  
  writexl::write_xlsx(zoneScenario, path = paste(outFolder, "zones_ts.xlsx", sep = "/"), col_names = TRUE)
  unlink(tempDir, recursive = TRUE)
  message(paste("Output folder:", outFolder, "\n"))
  # message("If the outputs can not be well processed in AccessMod, try to use multi_ts_fast().")
}
