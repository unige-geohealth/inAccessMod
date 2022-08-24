library(terra)
library(sf)
library(inAccessMod)
library(readxl)
library(xfun)

mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS"
country <- "LIECHTENSTEIN"
scenarioFolder <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import"
mergedLandcover <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import/raster_land_cover_merged_landcoverMerged.img"

pathAdmin <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/LIECHTENSTEIN/import/vector_zone_for_stat_zones"
layerAdmin <- "vector_zone_for_stat_zones"
multiTsFolder = NULL

prep_multi_ts <- function (mergedLandcover, scenarioFolder, standardFolders = TRUE, mainPath = NULL, country = NULL, adminDSN = NULL, adminLayer = NULL, multiTsFolder = NULL) {
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
    if (!is.null(multiTsFolder)) {
      cat("\nAs standardFolder is TRUE, multiTsFolder is ignored")
      multiTsFolder <- NULL
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
    if (is.null(multiTsFolder)) {
      stop("As standardFolders is FALSE, multiTsFolder is required.")
    }
    if (!is.character(multiTsFolder)) {
      stop("multiTsFolder must be 'character'")
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
  message("\nAdministrative zones...")
  if (is.null(adminDSN)) {
    borders <- get_boundaries(mainPath, country, "processed", mostRecent = FALSE)
  } else {
    borders <- tryCatch({sf::st_read(adminDSN, adminLayer)}, error = function(e) NULL)
    if (is.null(borders)) {
      stop(paste0(adminDSN, "/", adminLayer, " shapefile could not be opened."))
    }
  }
  cols <- colnames(borders)
  nZones <- nrow(borders)
  uniqueCols <- which(apply(as.data.frame(borders), 2, function(x) length(unique(x))) == nZones)
  uniqueCols <- uniqueCols[!names(uniqueCols) == "geometry"]
  if (length(uniqueCols) == 0) {
    stop("No column has a only unique values.")
  } 
  message("Only the columns with only unique values are displayed.")
  print(as.data.frame(borders)[, uniqueCols])
  colUniqueAdmin <- utils::menu(names(uniqueCols), title = "\nSelect the column that you would like to use for referring to the different administrative units.")
  landcover <- terra::rast(mergedLandcover)
  vLc <- values(landcover)[,1]
  vLc <- unique(vLc[!is.na(vLc)])
  vLc <- vLc[order(vLc)]
  files <- list.files(scenarioFolder, full.names = TRUE)
  xls <- files[grepl(".xls$|.xlsx$", files)]
  cat(paste("\n", stringr::str_to_title(xfun::numbers_to_words(length(xls))), "Excel files found in the travel scenario folder.\n"))
  subsDf <- data.frame(from = vLc, to = 1:length(vLc))
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
    vLcDupl <- vLCi[duplicated(vLci)]
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
    match(xlsi$class, subsDf$from)
  }
  newLandcover <- terra::subst(landcover, from = subsDf$from, to = subsDf$to)
  scenario$class <- 1:length(scenario$class)
  if (is.null(multiTsFolder)) {
    dirLst <- list.dirs(paste(mainPath, country, "data", sep = "/"), recursive = FALSE)
    if (any(grepl("zToAccessMod", dirLst))) {
      zToAccess <- dirLst[grepl("zToAccessMod", dirLst)]
      folderLst <- list.dirs(zToAccess)
      folderLst <- folderLst[grepl("[0-9]{14}", folderLst)]
      if (length(folderLst) > 1) {
        folderLst <- unique(gsub("[^0123456789]","", folderLst))
        folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
        message("\nMultiple travel scenario folder...")
        folder <- select_folder(folders, "Select the 'zToAccessMod' subfolder where you would like to store the set of travel scenarios.")
        folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
        sysTime <- Sys.time()
        timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
        multiTsFolder <- paste(mainPath, country, "data/zToAccessMod", folder, "multiple_ts", timeFolder, "input", sep = "/")
        dir.create(multiTsFolder, recursive = TRUE)
      } else if (length(folderLst) == 1){
        sysTime <- Sys.time()
        timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
        multiTsFolder <- paste(folderLst, "multiple_ts", timeFolder, "input", sep = "/")
        dir.create(multiTsFolder, recursive = TRUE)
      } else {
        stop("No compiled data folder has been found. Run the 'compile_processed_data' function first or provide a folder path for the multiple travel scenario files (multiTsFolder argument).")
      }
    } else {
      stop("No 'zToAccessMod' folder has been found. Run the 'compile_processed_data' function first or provide a folder path for the multiple travel scenario files (multiTsFolder argument).")
    }
  } else {
    sysTime <- Sys.time()
    timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
    multiTsFolder <- paste(multiTsFolder, "multiple_ts", timeFolder, "input", sep = "/")
    dir.create(multiTsFolder, recursive = TRUE)
  }
  nScenarios <- readline(prompt = "How many travel scenarios you would like to have: ")
  if (is.na(round(as.numeric(nScenarios))) | round(as.numeric(nScenarios)) <= 0 ) {
    stop("Invalid input !")
  }
  for (i in 1:nScenarios) {
    write.csv(scenario, paste0(multiTsFolder, "/ts", i, ".csv"), row.names = FALSE)
  }
  zoneScenario <- data.frame(Zone = unique(as.data.frame(borders)[, colUniqueAdmin]), Scenario = 1)
  colnames(zoneScenario)[1] <- colnames(as.data.frame(borders))[colUniqueAdmin]
  write.csv(zoneScenario, paste0(multiTsFolder, "/zones_ts.csv"), row.names = FALSE)
  terra::writeRaster(newLandcover, filename = paste(multiTsFolder, "original_merged_landcover.tif", sep = "/"))
  sf::st_write(obj = borders, dsn = multiTsFolder, layer = "vBorders", driver = "Esri Shapefile")
  message("\n=====================================================\n")
  message("These are the CSV tables associated to each travel scenario:\n")
  for (i in 1:nScenarios) {
    cat(paste0(multiTsFolder, "/ts", i, ".csv\n"))
  }
  message("\nThis is the CSV table that relates the different administrative zones with the different travel scenarios:\n")
  cat(paste0(multiTsFolder, "/zones_ts.csv\n"))
  message("\n=====================================================\n")
  message("Open these tables and modify their values accordingly, and then run the 'multi_ts' function.\n")
}

multi_ts <- function (mainPath, country, multiTsFolder = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!is.null(multiTsFolder)) {
    if (!is.character(multiTsFolder)) {
      stop("multiTsFolder must be 'character'.")
    }
    if (!dir.exists(multiTsFolder)) {
      stop(paste(multiTsFolder, "does not exist."))
    } 
    multiTsFolder <- paste(multiTsFolder, "multiple_ts", sep = "/")
    folderLst <- list.dirs(multiTsFolder)
    folderLst <- folderLst[grepl("[0-9]{14}/input", folderLst)]
    if (length(folderLst) == 0) {
      stop("Inputs are missing. Run the 'prep_multi_ts' function or modify the multiTsFolder argument.")
    }
    if (!length(folderLst) == 1) {
      folderLst <- unique(gsub("[^0123456789]","", folderLst))
      folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
      message("\nMultiple travel scenario input folder...")
      folder <- select_folder(folders, "Select the multiple travel scenario input folder.")
      folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
      folder <- paste(multiTsFolder, folder, "input", sep = "/")
    } else {
      folder <- folderLst
    }
  } else {
    dirLst <- list.dirs(paste(mainPath, country, "data/zToAccessMod", sep = "/"), recursive = TRUE)
    if (any(grepl("multiple_ts/[0-9]{14}/input", dirLst))) {
      folder <- dirLst[grepl("multiple_ts/[0-9]{14}/input", dirLst)]
      if (length(folder) > 1) {
        folderLst <- gsub("^.*data/", "", folder)
        ind <- utils::menu(folderLst, title = "Select the multiple travel scenario input folder (index).")
        folder <- folder[ind]
      }
    } else {
      stop("Inputs are missing. Run the 'prep_multi_ts' function or specify a value for the multiTsFolder argument.")
    }
  }
  zoneScenario <- paste(folder, "zones_ts.csv", sep = "/")
  if (!file.exists(zoneScenario)) {
    stop(paste(zoneScenario, "does not exist. Run the 'prep_multi_ts' function first."))
  }
  zoneScenario <- read.csv(paste(folder, "zones_ts.csv", sep = "/"))
  scenarios <- unique(zoneScenario$Scenario)
  if (length(scenarios) == 1) {
    stop("Only one travel scenario taken into account. Modify the zones_ts.csv table.")
  }
  ts <- list.files(folder, pattern = "ts[0-9].csv", full.names = TRUE)
  comb <- utils::combn(1:length(ts), 2)
  matchTs <- NULL
  for (i in 1:ncol(comb)) {
    ts1 <- read.csv(ts[comb[1, i]])
    ts2 <- read.csv(ts[comb[2, i]])
    nMatch <- plyr::match_df(ts1, ts2, on = c("label", "speed", "mode"))[, 1]
    if (length(nMatch) == nrow(ts1)) {
      matchTs <- c(matchTs, TRUE)
    } else {
      matchTs <- c(matchTs, FALSE)
    }
  }
  if (all(matchTs)) {
    stop("All travel scenarios are identical. Modify the ts#.csv tables.")
  }
  tsNum <- as.numeric(gsub(".csv", "", stringr::str_extract(ts, "[0-9]*.csv")))
  if (!all(scenarios %in% tsNum)) {
    stop("Inconsistency between the indicated travel scenario in the zones_ts.csv table and the available travel scenario tables.")
  }
  
  finalScenario <- paste0(folder, "/ts", scenarios[1], ".csv")
  if (!file.exists(finalScenario)) {
    stop(paste(finalScenario, "does not exist. Run the 'prep_multi_ts' function first."))
  }
  finalScenario <- read.csv(finalScenario)
  colN <- colnames(zoneScenario)[1]
  borders <- tryCatch({sf::st_read(folder, "vBorders")}, error = function(e) NULL)
  if (is.null(borders)) {
    stop(paste0(folder, "/vBorders", " does not exist or could not be opened. Run the 'prep_multi_ts' function first."))
  }
  newLandcover <- tryCatch({terra::rast(paste0(folder, "/original_merged_landcover.tif"))}, error = function(e) NULL)
  if (is.null(newLandcover)) {
    stop(paste0(folder, "/original_merged_landcover.tif does not exist or could not be opened. Run the 'prep_multi_ts' function first."))
  }
  rastLst <- vector("list", length(scenarios))
  for (i in 1:length(scenarios)) {
    zones <- zoneScenario[, colN][zoneScenario$Scenario == scenarios[i]]
    newShp <- borders[as.data.frame(borders)[, colN] %in% zones, ]
    newRas <- terra::mask(newLandcover, as(newShp, "SpatVector"))
    newSc <- read.csv(paste0(folder, "/ts", scenarios[i],".csv"))
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

prep_multi_ts(mainPath, country, mergedLandcover, scenario)
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
#   #   fileLst <- list.files(multiTsFolder, recursive = FALSE, full.names = TRUE)
#   #   if (any(grepl("final_merged_landcover.tif|final_travel_scenario.csv", fileLst))) {
#   #     message("\nFinal outputs already exist and they will be removed.\n")
#   #     yn <- utils::menu(c("YES", "NO"), title = "Do you want to continue (if NO, set the the multiTsFolder to NULL or provide another folder path).")
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
#     stop(stop("No compiled data folder has been found. Run the 'compile_processed_data' function first or provide a folder path for the multiple travel scenario files (multiTsFolder argument)."))
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
