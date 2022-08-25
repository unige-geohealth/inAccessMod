library(terra)
library(sf)
library(inAccessMod)
library(readxl)
library(xfun)
library(writexl)


mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
country <- "LIECHTENSTEIN"
scenarioFolder <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import"
mergedLandcover <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import/raster_land_cover_merged_landcoverMerged.img"

adminDSN <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import"
adminLayer <- "vector_zone_for_stat_zones"
tsRootFolder = "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import/NEW"

prep_multi_ts <- function (mergedLandcover, scenarioFolder, standardFolders = TRUE, mainPath = NULL, country = NULL, adminDSN = NULL, adminLayer = NULL, tsRootFolder = NULL) {
  if (!is.character(mergedLandcover)) {
    stop("mergedLandcover must be 'character'")
  }
  if (!dir.exists(as.character(scenarioFolder))) {
    stop("scenarioFolder does no exist.")
  }
  if (standardFolders) {
    if (is.null(mainPath)) {
      stop("As standardFolders is TRUE, mainPath is required.")
    }
    if (!is.character(mainPath)) {
      stop("mainPath must be 'character'")
    }
    if (is.null(country)) {
      stop("As standardFolders is TRUE, country is required.")
    }
    if (!is.character(country)) {
      stop("country must be 'character'")
    }
    if (!dir.exists(paste(mainPath, country, sep = "/"))) {
      stop(paste("No project has been found at", paste(mainPath, country, sep = "/")))
    }
    if (!is.null(adminDSN)) {
      cat("\nAs standardFolder is TRUE, adminDSN is ignored")
      adminDSN <- NULL
    }
    if (!is.null(adminLayer)) {
      cat("\nAs standardFolder is TRUE, adminLayer is ignored")
      adminLayer <- NULL
    }
    if (!is.null(tsRootFolder)) {
      cat("\nAs standardFolder is TRUE, tsRootFolder is ignored")
      tsRootFolder <- NULL
    }
  } else {
    if (is.null(adminDSN)) {
      stop("As standardFolders is FALSE, adminDSN is required.")
    }
    if (!dir.exists(as.character(adminDSN))) {
      stop("adminDSN does not exist")
    }
    if (is.null(adminLayer)) {
      stop("As standardFolders is FALSE, adminLayer is required.")
    }
    if (!is.character(adminLayer)) {
      stop("adminLayer must be 'character'")
    }
    if (is.null(tsRootFolder)) {
      stop("As standardFolders is FALSE, tsRootFolder is required.")
    }
    if (!is.character(tsRootFolder)) {
      stop("tsRootFolder must be 'character'")
    }
    if (!dir.exists(tsRootFolder)) {
      stop(paste(tsRootFolder, "does not exist."))
    } 
    if (!is.null(mainPath)) {
      cat("\nAs standardFolder is FALSE, mainPath is ignored")
      mainPath <- NULL
    }
    if (!is.null(country)) {
      cat("\nAs standardFolder is FALSE, country is ignored")
      country <- NULL
    }
  }
  message("\n\nAdministrative zones...")
  if (is.null(adminDSN)) {
    borders <- get_boundaries(mainPath, country, "processed", mostRecent = FALSE)
  } else {
    borders <- tryCatch({sf::st_read(adminDSN, adminLayer)}, error = function(e) NULL)
    if (is.null(borders)) {
      stop(paste0(adminDSN, "/", adminLayer, " shapefile could not be opened."))
    }
  }
  cols <- colnames(borders)
  print(as.data.frame(borders))
  colUniqueAdmin <- utils::menu(cols, title = "\nSelect the column that you would like to use for referring to the different administrative units.")
  landcover <- terra::rast(mergedLandcover)
  vLc <- values(landcover)[,1]
  vLc <- unique(vLc[!is.na(vLc)])
  vLc <- vLc[order(vLc)]
  files <- list.files(scenarioFolder, full.names = TRUE)
  xls <- files[grepl(".xls$|.xlsx$", files)]
  cat(paste("\n", stringr::str_to_title(xfun::numbers_to_words(length(xls))), "Excel files found in the travel scenario folder.\n"))
  subsDf <- data.frame(from = vLc, to = 1:length(vLc))
  xlsLst <- vector("list", length = length(xls))
  names(xlsLst) <- gsub("(^.*/)|(\\.xls$|\\.xlsx$)", "", xls)
  for (i in 1:length(xls)) {
    if (grepl(".xlsx$", xls[i])) {
      xlsi <- readxl::read_xlsx(xls[i])
    } else {
      xlsi <- readxl::read_xls(xls[i])
    }
    xlsi <- xlsi[complete.cases(xlsi), ]
    if (!all(colnames(xlsi) == c("class", "label", "speed", "mode"))) {
      stop(paste0(xls[i], ": column names must be 'class', 'label', 'speed' and 'mode'"))
    }
    vLci <- xlsi[, "class", drop = TRUE]
    if (!all(vLc %in% vLci)) {
      missVLc <- vLc[!vLc %in% vLci]
      missVLc <- paste(missVLc, collapse = ", ")
      stop(paste0(xls[i], ": Missing information for landcover value(s) ", missVLc))
    }
    vLcDupl <- vLci[duplicated(vLci)]
    if (length(vLcDupl) > 0) {
      stop(paste0(xls[i], ": Duplicated landcover class: ", vLcDupl))
    }
    vSpeed <- xlsi[, "speed", drop = TRUE]
    if (!is.numeric(vSpeed)) {
      stop(paste0(xls[i], ": Speed column has to be numeric."))
    }
    vMode <- xlsi[, "mode", drop = TRUE]
    if (!all(vMode %in% c("WALKING", "MOTORIZED", "BICYCLING"))) {
      stop(paste0(xls[i], ": mode can only be 'WALKING' 'MOTORIZED' or 'BICYCLING'."))
    }
    # # Reclass
    # xlsi$class <- subsDf$to[match(xlsi$class, subsDf$from)]
    # xlsi <- xlsi[order(xlsi$class), ]
    # xlsi$class
    xlsLst[[i]] <- xlsi
  }
  # Reclass landcover
  # newLandcover <- terra::subst(landcover, from = subsDf$from, to = subsDf$to)
  newLandcover <- landcover
  if (is.null(tsRootFolder)) {
    dirLst <- list.dirs(paste(mainPath, country, "data", sep = "/"), recursive = FALSE)
    if (any(grepl("zToAccessMod", dirLst))) {
      zToAccess <- dirLst[grepl("zToAccessMod", dirLst)]
      folderLst <- list.dirs(zToAccess)
      folderLst <- folderLst[grepl("zToAccessMod/[0-9]{14}$", folderLst)]
      if (length(folderLst) > 1) {
        folderLst <- unique(gsub("[^0123456789]","", folderLst))
        folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
        message("\nMultiple travel scenario folder...")
        folder <- select_folder(folders, "Select the 'zToAccessMod' subfolder where you would like to store the set of travel scenarios.")
        folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
        sysTime <- Sys.time()
        timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
        inputFolder <- paste(mainPath, country, "data/zToAccessMod", folder, "multiple_ts", timeFolder, "input", sep = "/")
        dir.create(inputFolder, recursive = TRUE)
      } else if (length(folderLst) == 1){
        sysTime <- Sys.time()
        timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
        inputFolder <- paste(folderLst, "multiple_ts", timeFolder, "input", sep = "/")
        dir.create(inputFolder, recursive = TRUE)
      } else {
        stop("No compiled data folder has been found. Run the 'compile_processed_data' function first or set the argument standardFolders to FALSE.")
      }
    } else {
      stop("No 'zToAccessMod' folder has been found. Run the 'compile_processed_data' function first or set the argument standardFolders to FALSE.")
    }
  } else {
    sysTime <- Sys.time()
    timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
    inputFolder <- paste(tsRootFolder, "multiple_ts", timeFolder, "input", sep = "/")
    dir.create(inputFolder, recursive = TRUE)
  }
  zoneScenario <- data.frame(Zone = unique(as.data.frame(borders)[, colUniqueAdmin]), Scenario = NA)
  colnames(zoneScenario)[1] <- colnames(as.data.frame(borders))[colUniqueAdmin]
  for (i in 1:length(xlsLst)) {
    writexl::write_xlsx(xlsLst[[i]], path = paste0(inputFolder, "/", names(xlsLst)[i], ".xlsx"), col_names = TRUE)
  }
  writexl::write_xlsx(zoneScenario, paste0(inputFolder, "/zones_ts.xlsx"), col_names = TRUE)
  terra::writeRaster(newLandcover, filename = paste(inputFolder, "original_merged_landcover.tif", sep = "/"))
  sf::st_write(obj = borders, dsn = inputFolder, layer = "vBorders", driver = "Esri Shapefile")
  message("\n=====================================================\n")
  message("These are the Excel tables associated to each travel scenario:\n")
  for (i in 1:length(xlsLst)) {
    cat(paste0(inputFolder, "/", names(xlsLst)[i], ".xlsx\n"))
  }
  message("\nThis is the Excel table that relates the different administrative zones with the different travel scenarios:\n")
  cat(paste0(inputFolder, "/zones_ts.xlsx\n"))
  message("\n=====================================================\n")
  message("Open these tables and modify their values accordingly, and then run the 'multi_ts' function.\n")
}

multi_ts <- function (standardFolders = TRUE, mainPath = NULL, country = NULL, tsRootFolder = NULL) {
  if (standardFolders) {
    if (!is.null(tsRootFolder)) {
      cat("\nAs standardFolders is TRUE, tsRootFolder is ignored")
      tsRootFolder <- NULL
    }
    if (is.null(mainPath)) {
      stop("As standardFolders is TRUE, mainPath is required.")
    }
    if (!is.character(mainPath)) {
      stop("mainPath must be 'character'")
    }
    if (is.null(country)) {
      stop("As standardFolders is TRUE, country is required.")
    }
    if (!is.character(country)) {
      stop("country must be 'character'")
    }
    dirLst <- list.dirs(paste(mainPath, country, "data/zToAccessMod", sep = "/"), recursive = TRUE)
    if (any(grepl("multiple_ts/[0-9]{14}/input", dirLst))) {
      inputFolder <- dirLst[grepl("multiple_ts/[0-9]{14}/input", dirLst)]
      if (length(inputFolder) > 1) {
        folderLst <- gsub("^.*data/", "", inputFolder)
        ind <- utils::menu(folderLst, title = "Select the multiple travel scenario input folder (index).")
        inputFolder <- inputFolder[ind]
      }
    } else {
      stop("Inputs are missing. Run the 'prep_multi_ts' function or specify the tsRootFolder.")
    }
  } else {
    if (is.null(tsRootFolder)) {
      stop("As standardFolders is FALSE, tsRootFolder is required.")
    }
    if (!is.character(tsRootFolder)) {
      stop("tsRootFolder must be 'character'")
    }
    if (!dir.exists(tsRootFolder)) {
      stop(paste(tsRootFolder, "does not exist."))
    }
    dirLst <- list.dirs(tsRootFolder, recursive = TRUE)
    if (any(grepl("multiple_ts/[0-9]{14}/input", dirLst))) {
      inputFolder <- dirLst[grepl("multiple_ts/[0-9]{14}/input", dirLst)]
      if (length(inputFolder) > 1) {
        folderLst <- gsub("^.*data/", "", inputFolder)
        ind <- utils::menu(folderLst, title = "Select the multiple travel scenario input folder (index).")
        inputFolder <- inputFolder[ind]
      }
    } else {
      stop("Inputs are missing. Run the 'prep_multi_ts' function or specify the tsRootFolder.")
    }
    if (!is.null(mainPath)) {
      cat("\nAs standardFolders is FALSE, mainPath is ignored")
      mainPath <- NULL
    }
    if (!is.null(country)) {
      cat("\nAs standardFolders is FALSE, country is ignored")
      country <- NULL
    }
  } 
  zoneScenario <- paste(inputFolder, "zones_ts.xlsx", sep = "/")
  if (!file.exists(zoneScenario)) {
    stop(paste(zoneScenario, "does not exist. Run the 'prep_multi_ts' function first."))
  }
  zoneScenario <- readxl::read_excel(paste(inputFolder, "zones_ts.xlsx", sep = "/"))
  xls <- paste0(inputFolder, "/", zoneScenario$Scenario, ".xlsx")
  xlsFound <- NULL
  for (i in 1:length(xls)) {
    xlsFound <- c(xlsFound, file.exists(xls[i]))
  }
  if (any(!xlsFound)) {
    stop(paste0("Scenarios indicated in the zones_ts_xlsx table are missing or do not match with the available travel scenario tables."))
  }
  colN <- colnames(zoneScenario)[1]
  borders <- tryCatch({sf::st_read(inputFolder, "vBorders")}, error = function(e) NULL)
  if (is.null(borders)) {
    stop(paste0(inputFolder, "/vBorders", " does not exist or could not be opened. Run the 'prep_multi_ts' function first."))
  }
  newLandcover <- tryCatch({terra::rast(paste0(inputFolder, "/original_merged_landcover.tif"))}, error = function(e) NULL)
  if (is.null(newLandcover)) {
    stop(paste0(inputFolder, "/original_merged_landcover.tif does not exist or could not be opened. Run the 'prep_multi_ts' function first."))
  }
  rastLst <- vector("list", nrow(zoneScenario))
  scenarioLst <- vector("list", nrow(zoneScenario))
  for (i in 1:nrow(zoneScenario)) {
    zone <- zoneScenario[i, 1, drop = TRUE]
    scenario <- zoneScenario[i, 2, drop = TRUE]
    newShp <- borders[as.data.frame(borders)[, colN] %in% zone, ]
    newRas <- terra::mask(newLandcover, as(newShp, "SpatVector"))
    # plot(newRas)
    # plot(st_geometry(newShp), add =TRUE)
    scenarioTable <- readxl::read_excel(paste0(inputFolder, "/", scenario,".xlsx"))
    scenarioTable$label <- paste0(scenarioTable$label, "_", scenario)
    scenarioLst[[i]] <- scenario
    
    matchClass <- plyr::match_df(newSc, finalScenario, on = c("label", "speed", "mode"))[, 1]
    noMatch <- newSc[!newSc$class %in% matchClass, ]
    if (nrow(noMatch) == 0) {
      rastLst[[i]] <- newRas
      next
    }
    lastVal <- finalScenario$class[length(finalScenario$class)]
    newVal <- (lastVal + 1):(lastVal + nrow(noMatch))
    newRas <- terra::subst(newRas, from = noMatch$class, to = newVal)
    rastLst[[i]] <- newRas
    noMatch$class <- newVal
    noMatch$label <- paste0(noMatch$label, "_", i)
    finalScenario <- rbind(finalScenario, noMatch)
  }
  finalLandcover <- do.call(terra::merge, rastLst)
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste0(folder, "/../output/", timeFolder)
  dir.create(outFolder, recursive = TRUE)
  message("Writing new merged landcover raster...")
  terra::writeRaster(finalLandcover, filename = paste(outFolder, "multi_ts_merged_landcover.tif", sep = "/"))
  message("Writing new travel scenario table...")
  write.csv(finalScenario, file = paste(outFolder, "multi_ts.csv", sep = "/"), row.names = FALSE)   
}  

prep_multi_ts(mergedLandcover = mergedLandcover, scenarioFolder = scenarioFolder, standardFolders = FALSE, adminDSN = adminDSN, 
              adminLayer = adminLayer, mainPath = mainPath,
              country = country, tsRootFolder = tsRootFolder)

multi_ts(mainPath, country)

# version longue: rajouter ID sur label; output => Excel; tableau scenarios; le plus souvent déjà existants
# Nom ficher excel comme nom du scenario pour zones_ts.csv
# Laisser si tous les mêmes; checker les noms de fichier
# Vérifier pas de missing speed; checker; Capital letter; Entête...
# Pas de classe dans le landcover sans speed; (l'inverse n'est pas grave);
# Voir avec Loïc pour intégrer sa fonction R.
# Données

# Envoyer 
# Workshop; but atelier, quel public, comment ils travaillent; pour capacity building
# Capacity building; interpretatio/utlisation pour renforcer le système; utilisation accessmod, modélisation

#       folderLst <- list.dirs(zToAccess)
#       folderLst <- folderLst[grepl("[0-9]{14}", folderLst)]
#       if (length(folderLst) > 0) {
#         folderLst <- unique(gsub("[^0123456789]","", folderLst))
#         folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
#         message("\nMultiple travel scenario folder...")
#         folder <- select_folder(folders, "Select the 'zToAccessMod' subfolder where you would like to store the set of travel scenarios.")
#   }
#       
#       folderLst <- unique(gsub("[^0123456789]","", folderLst))
#       folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
#       message("\nMultiple travel scenario folder...")
#       folder <- select_folder(folders, "Select the 'zToAccessMod' subfolder where you would like to store the set of travel scenarios.")
#       folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
#     
#   #   fileLst <- list.files(tsRootFolder, recursive = FALSE, full.names = TRUE)
#   #   if (any(grepl("final_merged_landcover.tif|final_travel_scenario.csv", fileLst))) {
#   #     message("\nFinal outputs already exist and they will be removed.\n")
#   #     yn <- utils::menu(c("YES", "NO"), title = "Do you want to continue (if NO, set the the tsRootFolder to NULL or provide another folder path).")
#   #     if (yn == 2) {
#   #       stop_quietly("You stop the function.")
#   #     } else {
#   #       fileLst
#   #       toRm <- fileLst[grepl("zones_ts.csv|ts[0-9]*.csv", fileLst)]
#   #       for (i in 1:length(toRm)) {
#   #         unlink(toRm[i])
#   #       }
#   #     }
#   #   }
#   # }
#   
#   
#   
#   
#   dirLst <- list.dirs(paste(mainPath, country, "data", sep = "/"), recursive = FALSE)
#   if (!any(grepl("zToAccessMod", dirLst))) {
#     stop(stop("No compiled data folder has been found. Run the 'compile_processed_data' function first or provide a folder path for the multiple travel scenario files (tsRootFolder argument)."))
#   }
#     zToAccess <- dirLst[grepl("zToAccessMod", dirLst)]
#     folderLst <- list.dirs(zToAccess)
#     folderLst <- folderLst[grepl("[0-9]{14}", folderLst)]
#     if (length(folderLst) > 0) {
#       folderLst <- unique(gsub("[^0123456789]","", folderLst))
#       folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
#       message("\nMultiple travel scenario folder...")
#       folder <- select_folder(folders, "Select the 'zToAccessMod' subfolder where you would like to store the set of travel scenarios.")
#       folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
#     }
#   }
# } 
# 
# multi <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/data/zToAccessMod/20220816152629/multiple_ts/20220818113501"
# prep_multi_ts(mainPath, country, mergedLandcover, scenario)
# 

# admin <- st_read(pathAdmin, layerAdmin)
# plot(st_geometry(admin))
# newLandcover <- terra::subst(landcover, from = scenario$class, to = 1:length(scenario$class))
# plot(newLandcover)
# scenario$class <- 1:length(scenario$class)
# 
# # dirLst <- list.dirs(paste(mainPath, country, "data", sep = "/"), recursive = FALSE)
# # 
# # if (any(grepl("zToAccessMod", dirLst))) {
# #   zToAccess <- dirLst[grepl("zToAccessMod", dirLst)]
# #   folderLst <- list.dirs(zToAccess)
# #   folderLst <- folderLst[grepl("[0-9]{14}", folderLst)]
# #   if (length(folderLst) > 0) {
# #     folderLst<- gsub("[^0123456789]","", folderLst)
# #   }
# #   folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
# # }
# # 
# # folder <- select_folder(folders, "Select the zToAccessMod folder to store the set of travel scenarios")
# # folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
# # if (!dir.exists(paste(mainPath, country, "data/zToAccessMod", folder, "travelScenarios", sep = "/"))) {
# #   dir.create(paste(mainPath, country, "data/zToAccessMod", folder, "travelScenarios", sep = "/"))
# # }
# 
# 
# 
# 
