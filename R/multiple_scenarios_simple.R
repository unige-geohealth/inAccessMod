library(terra)
library(sf)
library(readxl)
library(writexl)
library(tibble)

inputFolder <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/SWITZERLAND/multi_ts"
adminLayerName <- "vBorders"
landcoverFile <- "mergedLandcover.img"

check_ts_table <- function (xlsi, xlsiFile, vLc) {
  xlsi <- xlsi[complete.cases(xlsi), ]
  if (!all(colnames(xlsi) == c("class", "label", "speed", "mode"))) {
    stop(paste0(xlsiFile, ": column names must be 'class', 'label', 'speed' and 'mode'"))
  }
  vLci <- xlsi[, "class", drop = TRUE]
  if (!all(vLc %in% vLci)) {
    missVLc <- vLc[!vLc %in% vLci]
    missVLc <- paste(missVLc, collapse = ", ")
    stop(paste0(xlsiFile, ": Missing information for landcover value(s) ", missVLc))
  }
  vLcDupl <- vLci[duplicated(vLci)]
  if (length(vLcDupl) > 0) {
    stop(paste0(xlsiFile, ": Duplicated landcover class: ", vLcDupl))
  }
  vSpeed <- xlsi[, "speed", drop = TRUE]
  if (!is.numeric(vSpeed)) {
    stop(paste0(xlsiFile, ": Speed column has to be numeric."))
  }
  if (any(vSpeed < 0)) {
    stop(paste0(xlsiFile, ": Speed can only be positive or equal to zero."))
  }
  vMode <- xlsi[, "mode", drop = TRUE]
  if (!all(vMode %in% c("WALKING", "MOTORIZED", "BICYCLING"))) {
    stop(paste0(xlsiFile, ": mode can only be 'WALKING' 'MOTORIZED' or 'BICYCLING'."))
  }
  return(0)
}

prep_zones_ts_table <- function (inputFolder, adminLayerName, landcoverFile) {
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
  adminUnits <- adminUnits[order(adminUnits)]
  if (length(adminUnits) == 1) {
    stop("Selected column have only one value.")
  }
  zoneScenario <- data.frame(Zone = adminUnits, scenario = NA)
  colnames(zoneScenario)[1] <- colnames(as.data.frame(admin))[colUnit]
  scenarios <- gsub(paste0(inputFolder, "|/|\\.xls|\\.xlsx|\\.csv"), "", xls)
  for (i in 1:nrow(zoneScenario)) {
    sc <- utils::menu(scenarios, title = paste("\nWhich scenario for", zoneScenario[i, 1]))
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
multi_ts(inputFolder, adminLayerName, landcoverFile)

