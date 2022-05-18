##' Title: Main functions for pre-processing the input data for AccessMod
##' Author: Fleur Hierink and Pablo Timoner
##' Organisation: University of Geneva
##' Date: 2022

# Libraries --------------------------------------------------------------------------
# Efficient for shapefiles 
if (!require("sf")) install.packages("sf"); library("sf")
# New library raster
if (!require("terra")) install.packages("terra"); library("terra")
# Still useful for some functions
if (!require("raster")) install.packages("raster"); library("raster")
# Downloading openstreetmap data on roads and rivers
if (!require("osmextract")) install.packages("osmextract"); library("osmextract")
# Underlying support for spatial analysis
if (!require("rgdal")) install.packages("rgdal"); library("rgdal")
# Underlying support for spatial analysis
if (!require("gdalUtils")) install.packages("gdalUtilsg"); library("gdalUtils")
# Underlying support for spatial analysis
if (!require("rgeos")) install.packages("rgeos"); library("rgeos")
# Zonal statistics
if (!require("exactextractr")) install.packages("exactextractr"); library("exactextractr")
# Rasterize fast
if (!require("fasterize")) install.packages("fasterize"); library("fasterize")
# Reclassifying roads
if (!require("dplyr")) install.packages("dplyr"); library("dplyr")
# Print contents of directories in a tree-like format 
if (!require("fs")) install.packages("fs"); library("fs")
# Menu interface and prompt
if (!require("utils")) install.packages("utils"); library("utils")
# Common string manipulations
if (!require("stringr")) install.packages("stringr"); library("stringr")
# Access FTP servers
if (!require("RCurl")) install.packages("RCurl"); library("RCurl")
# Access remotes to install packages from github
if (!require("remotes")) install.packages("remotes"); library("remotes")
# Access geoboundaries
if (!require("rgeoboundaries")) remotes::install_github("wmgeolab/rgeoboundaries"); library("rgeoboundaries")
# Access Zenodo
if (!require("zen4R")) install_github("eblondel/zen4R"); library("zen4R")
# Get suggestion of projection based on a shapefile
if (!require("crsuggest")) install.packages("crsuggest"); library("crsuggest")
# Get ISO code
if (!require("countrycode")) install.packages("countrycode"); library("countrycode")
# Remove accent for country names
if (!require("stringi")) install.packages("stringi"); library("stringi")
# Read Excel file
if (!require("readxl")) install.packages("readxl"); library("readxl")
# Large data table
if (!require("data.table")) install.packages("data.table"); library("data.table")

# Main parameters --------------------------------------------------------------------------
# DEM
urlSRTM <- "https://github.com/sikli/srtm_country/archive/master.zip"
ftpWorldPop <- "ftp://ftp.worldpop.org.uk/GIS/Population/"
# Land cover
# First alternative: Download the global LC and then crop it. Slow, and bugs with Zenodo download
# doiLC <- "10.5281/zenodo.3939050"
# zenodoFileLC <- "PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"
# Second alternative: Based on the boundary extent, download the corresponding tiles from AWS and mosaic
awsLCFolder <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
awsLCSuffix <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"

stop_quietly <- function(msg) {
  message(msg)
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


# Main functions --------------------------------------------------------------------------
# Initiate the project. Create directories, get the ISO code
# Warnings are displayed if the folders already exist, and these are not erased
initiate_project <- function (mainPath) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!dir.exists(mainPath)) {
    stop(paste(mainPath, "folder does not exist."))
  }
  # Select the country from the codelist dataframe from the countrycode package
  if (!exists("codelist")) {
    stop("'codelist' table from 'countrycode' is missing. Load the 'countrycode' package.")
  }
  countryLst <- codelist$country.name.en[!is.na(codelist$un.name.en)]
  countryInd <- menu(countryLst, title="Select the country", graphics=TRUE)
  if (countryInd == 0) {
    stop_quietly("You exit the function.")
  }
  # Store the original name
  regionOriginalName <- countryLst[countryInd]
  # Store the ISO code
  iso3 <- as.character(codelist[codelist$country.name.en == regionOriginalName, "iso3c"])
  # Modify the name if necessary for directory name
  region <- gsub(" \\(.*\\)|\\(.*\\)", "", regionOriginalName)
  region <- gsub("[^[[:alnum:]]", " ", region)
  region <- str_squish(region)
  region <- gsub("[[:space:]]", "_", region)
  region <- stri_trans_general(str = region, id = "Latin-ASCII")
  # Main standard inputs
  inputNames=c("rDEM", "rPopulation", "rLandcover", "vRoads", "vWaterLines", 
               "vWaterPolygons", "vBorders","vFacilities")
  message(paste("\nThe following input folders will be created:", paste(inputNames,collapse=", ")))
  # Add other data ?
  yn <- menu(c("YES","NO"), title="\nDo you want to add another input folder (type 1 or 2)?")
  if (yn == 0) {
    stop_quietly("You exit the function.")
  }
  while (yn == 1) {
    newName <- readline(prompt = "Enter the folder name: ")
    newName <- gsub(" \\(.*\\)|\\(.*\\)", "", newName)
    newName <- gsub("[^[[:alnum:]]", " ", newName)
    newName <- str_squish(newName)
    newName <- gsub("[[:space:]]", "_", newName)
    newName <- stri_trans_general(str = newName, id = "Latin-ASCII")
    types <- c("r", "v")
    type <- menu(c("Raster data", "Vector data"), title="\nWhich type of data will contain this folder?")
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
    newName <- paste0(types[type], str_to_title(newName))
    inputNames <- c(inputNames, newName)
    yn <- menu(c("YES", "NO"), title="\nDo you want to add another input folder?")
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
  }
  # Create directories
  pathData <- paste0(mainPath, "/", toupper(region), "/data")
  dir.create(pathData, recursive = TRUE, showWarnings = FALSE)
  for (inputName in inputNames) {
    dir.create(paste0(pathData, "/", inputName), showWarnings = FALSE)
  }
  # Create config.txt for ISO code, and then EPSG as well
  pathRegion <- paste0(mainPath, "/", region, "/data")
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  writeLines(c(paste0("COUNTRY:", regionOriginalName), paste0("ISO:", iso3)), fileConn)
  close(fileConn)
  # Create log.txt for operation tracking
  fileConn <- file(paste0(pathRegion, "/log.txt"))
  writeLines(regionOriginalName, fileConn)
  writeLines(paste0(Sys.time(), ": Project initiated"), fileConn)
  close(fileConn)
  # Print directory tree
  dir_tree(paste0(mainPath, "/", region, "/data"))
}


# Load files (e.g. health facilities)
copy_input <- function (mainPath, region, file) {
  if (!is.character(file)) {
    stop("file must be 'character'")
  }
  if (!file.exists(file)) {
    stop(paste(file, "does not exist."))
  }
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  path <- paste0(mainPath, "/", region, "/data")
  if (!dir.exists(paste0(mainPath, "/", region, "/data"))) {
    stop(paste(path, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  folders <- gsub("^.*/data/", "", list.dirs(path, recursive = FALSE))
  folders <- folders[!grepl("zToAccessMod", folders)]
  fold <- menu(folders, title = "Which data would you like to load?")
  folder <- list.dirs(path, recursive = FALSE)[grepl(folders[fold], list.dirs(path, recursive = FALSE))]
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(folder, "/", timeFolder, "/raw"), recursive = TRUE)
  folder <- paste0(folder, "/", timeFolder, "/raw")
  file.copy(file, folder, overwrite = TRUE, copy.date = TRUE)
  return(cat(paste(file, "copied to:", folder)))
}

filter_facilities <- function (tib, var, outFolder) {
  categories <- pull(tib, var) %>% unique()
  # If no NA
  if (!any(is.na(categories))) {
    # Only one category
    if (length(categories) == 1){
      cat(paste0("\nAll entries has '", categories, "' value for ", gsub("_", " ", names(var))," column.\n"))
      write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", categories), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
      return(tib)
    } else {
      cat("\n")
      gsub("_", " ", names(var)) %>% str_to_sentence() %>%  message()
      nCat <- 1:length(categories)
      indCat <- paste(paste0("\n", nCat, ": ", categories))
      cat(indCat)
      cat(paste("\n\nEnter all the indices that correspond to", gsub("_", " ", names(var)), "you want to keep.\nOn the same line separated by a space, or just skip to select all options.\n"))
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
      # All categories are selected
      if (length(selInd) == 0){
        write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(tib)
        # If invalid index, all categories are selected
      } else if (!all(selInd %in% nCat)) {
        write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(NULL)
      } else {
        # Only selected categories
        tib <- tib[tib[, var, drop = TRUE] %in% categories[selInd], ]
        write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories[selInd], collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(tib)
      }
    }
  } else {
    # If only NA
    if (all(is.na(categories))) {
      cat("\n")
      message(paste("\n\nThere are ONLY missing values for", gsub("_", " ", names(var)),":"))
      yn <- menu(c("YES", "NO"), title = paste("\nDo you want to keep all the health facilities (if not, the script will stop and no output will be produced)?"))
      # We keep all the data and keep running the script
      if (yn == 1) {
        write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(tib)
      }else{
        stop_quietly("You exited the script as a variable had only missing values. No output has been produced.")
      }
      # Some NA
    } else {
      categories <- categories[!is.na(categories)]
      # Besides NA, only one category
      if (length(categories) == 1){
        message(paste("\n\nThere are missing values for", gsub("_", " ", names(var)), "for the following facilities:\n"))
        print(tib[is.na(pull(tib, var)), c("external_id", "workspace_id", "date", "MoSD3", "HFNAME", var)])
        yn <- menu(c("YES", "NO"), title = paste("\nDo you want to keep these health facilities?"))
        # We keep the category and the NA
        if (yn == 1) {
          cat(paste0("Besides missing values, all entries has '", categories, "' value for ", gsub("_", " ", names(var))," column.\n"))
          write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, ", NA")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        } else {
          # We only keep the category, discarding the NA
          cat(paste0("Besides missing values, all entries has '", categories, "' value for ", gsub("_", " ", names(var))," column.\n"))
          write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", categories), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          tib <- tib[!is.na(tib[, var, drop = TRUE]), ]
          return(tib)
        }
      }
      # Besides NA, several categories
      cat("\n")
      gsub("_", " ", names(var)) %>% str_to_sentence() %>%  message()
      nCat <- 1:length(categories)
      indCat <- paste(paste0("\n", nCat, ": ", categories))
      cat(indCat)
      cat(paste("\n\nEnter all the indices that correspond to", gsub("_", " ", names(var)), "you want to keep.\nOn the same line separated by a space, or just skip to select all options.\n"))
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
      # All are selected
      if (length(selInd == 0)) {
        message(paste("\nThere are missing values for", gsub("_", " ", names(var)), "for the following facilities:\n"))
        print(tib[is.na(pull(tib, var)), c("external_id", "workspace_id", "date", "MoSD3", "HFNAME", var)])
        yn <- menu(c("YES", "NO"), title = paste("\nDo you want to keep these health facilities?"))
        # All categories and NA are kept
        if (yn == 1) {
          write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", "), ", NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        } else {
          # All categories but no NA
          tib <- tib[!is.na(tib[, var, drop = TRUE]), ]
          write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        }
        # Invalid index, all categories and NA are kept
      } else if (!all(selInd %in% nCat)) {
        write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", "), ", NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(NULL)
      } else {
        message(paste("\nThere are missing values for", gsub("_", " ", names(var)), "for the following facilities:\n"))
        print(tib[is.na(pull(tib, var)), c("external_id", "workspace_id", "date", "MoSD3", "HFNAME", var)])
        yn <- menu(c("YES", "NO"), title = paste("\nDo you want to keep these health facilities?"))
        # Selected categories and NA are kept
        if (yn == 1) {
          tib <- tib[tib[, var, drop = TRUE] %in% categories[selInd] | is.na(tib[, var, drop = TRUE]), ]
          write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories[selInd], collapse = ", "), ", NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        } else {
          # Only selected categories
          tib <- tib[tib[, var, drop = TRUE] %in% categories[selInd], ]
          write(paste0(str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories[selInd], collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        }
      }
    }
  }
}


filter_hf <- function (mainPath, region, extension, mostRecent) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  rawHF <- check_exists(pathFacilities, "raw", layer = FALSE, extension = extension)
  if (is.null(rawHF)) {
    stop(paste0("Raw health facility table ('.", extension, "') is missing. You might have to run the copy_input function."))
  }
  timeFolder <- choose_input(rawHF, "Excel table copied at", mostRecent)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")
  } else {
    pathFacilities <- paste0(pathFacilities, "/", timeFolder, "/raw/")
    files <- list.files(pathFacilities)[grepl(extension, list.files(pathFacilities))]
    if (length(files) > 1) {
      fileInd <- menu(files, "Select the index corresponding to the health facility table to be processed.")
      file <- files[fileInd]
    }else{
      file <- files
    }
    sysTime <- Sys.time()
    t0 <- gsub("-|[[:space:]]|\\:","", sysTime)
    outFolder <- paste(gsub("raw", "processed", pathFacilities), t0, sep = "/")
    dir.create(outFolder, recursive = TRUE)
    fileConn <- file(paste(outFolder, "log.txt", sep = "/"))
    writeLines(sysTime %>% as.character(), fileConn)
    close(fileConn)
    newTib <- read_excel(paste(pathFacilities, file, sep = "/"), skip = 1)
    variables <- c(health_facility_types = "MoSD3", 
                   facility_ownership = "MoSD7", 
                   functionality_status = "HFFUNCT", 
                   facility_status = "MoSD4",
                   accesibility_status = "HFACC"
    )
    for (i in 1:length(variables)){
      backupTib <- newTib
      newTib <- filter_facilities(newTib, variables[i], outFolder)
      if (is.null(newTib)) {
        message("\nInvalid index ! All options were kept.")
        newTib <- backupTib
        next
      }
    }
    write.csv(newTib, file = paste(outFolder, "health_facilities.csv", sep = "/"))
  }
}

create_hf_shapefile <- function (mainPath, region, mostRecentBoundaries, mostRecentTable, WGS84 = TRUE, epsg = NULL, alwaysProcess = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(WGS84)) {
    stop("WGS84 must be 'logical'")
  }
  if (WGS84) {
    epsg <- "EPSG:4326"
  } else {
    if (is.null(epsg)) {
      stop("If WGS84 = FALSE, epsg is required.")
    } else {
      validEPSG <- crs_sf$crs_code[!is.na(crs_sf$crs_units)]
      if (!epsg %in% validEPSG) {
        stop("EPSG not valid.")
      }
      else {
        epsg <- paste0("EPSG:", epsg)
      }
    }
  }
  pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  message("\nLoading processed boundary shapefile...")
  border <- get_boundaries(mainPath, region, type = "processed", mostRecent = mostRecentBoundaries)
  if (is.null(border)) {
    
  }
  hf <- check_exists(path = pathFacilities, type = "processed", layer = FALSE, extension = "csv")
  if (is.null(hf)) {
    stop("No processed health facility table available. Run the filter_hf function.")
  }
  timeFolder <- choose_input(hf, "Health facility table filtered at:", mostRecentTable)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")  
  } else {
    folderLst <- list.dirs(pathFacilities)
    hfFolder <-   folderLst[grepl(paste0("processed/", timeFolder), folderLst)]
    toProcess <- already_processed(hfFolder, alwaysProcess)
    if (!toProcess) {
      stop_quietly("You exit the function.")  
    }
    filesCsv <- list.files(hfFolder)[grepl("\\.csv$", list.files(hfFolder))]
    multiMsg <- "Select the CSV table that you would like to process."
    if (length(filesCsv) > 1) {
      fileInd <- menu(filesCsv, multiMsg)
      file <- filesShp[fileInd]
    }else{
      file <- filesCsv
    }
    df <- read.csv(paste(hfFolder, file, sep = "/"))
    xy <- data.frame(Lat = df[, "MoSDGPS_SQ002", drop = TRUE], Lon = df[, "MoSDGPS_SQ001", drop = TRUE])
    if (nrow(xy[complete.cases(xy), ]) == 0) {
      stop_quietly(paste("Coordinates are not available! Add them manually in the CSV file:\n", paste(hfFolder, file, sep = "/")))
    }
    if (!all(complete.cases(xy))) {
      message(paste("Coordinates are missing for the following facilities:"))
      cat("\n")
      dfNA <- df[!complete.cases(xy), ]
      print(dfNA[, c("external_id", "workspace_id", "date", "MoSD3", "HFNAME")])
      yn <- menu(c("Exit the script and add the coordinates manually in the CSV file", "Remove these HFs"), title = paste("\nWhat would you like to do?"))
      if (yn == 1) {
        stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", paste(hfFolder, file, sep = "/")))
      } else {
        write.table(dfNA, paste(hfFolder, "coordinates_NA.txt", sep = "/"))
        message(paste("\nYou can access the removed HFs at:\n", paste(hfFolder, "coordinates_NA.txt", sep = "/")))
      }
    }
    pts <- SpatialPointsDataFrame(coords = xy[complete.cases(xy), ], data = df[complete.cases(xy), ], proj4string = crs(epsg))
    border <- gUnaryUnion(as(st_transform(border, crs(pts)), "Spatial"))
    inter <- gIntersects(border, pts, byid = TRUE)
    interOutside <- FALSE
    if (!all(inter[, 1])) {
      interOutside <- TRUE
      message("The follwing HFs are outside the region/country boundaries:")
      print(df[!inter, c("external_id", "workspace_id", "date", "MoSD3", "HFNAME")])
      yn <- menu(c("Exit the script and correct the coordinates manually in the CSV file", "Remove these HFs and create a HFs' shapefile"), title = paste("\nWhat would you like to do?"))
      if (yn == 1) {
        stop_quietly(paste("You exited the script! Correct the coordinates manually in the CSV file:\n", paste(hfFolder, file, sep = "/")))
      }
    }
    shp <- st_as_sf(pts[inter[, 1], c("external_id", "workspace_id", "date", "MoSD3", "HFNAME")])
    cat("\nSaving the HFs' shapefile...")
    st_write(shp, paste(hfFolder, "health_facilities.shp", sep = "/"), append = FALSE)
    if (interOutside) {
      write.table(df[!inter[, 1]], paste(hfFolder, "coordinates_outside.txt", sep = "/"))
      message(paste("\nYou can access the removed HFs at:\n", paste(hfFolder, "coordinates_outside.txt", sep = "/")))
    }
  }
}



# Access config.txt (created with initiate_project)
# Used in other functions
get_param <- function (mainPath, region, param) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.character(param)) {
    stop("param must be 'character'")
  }
  pathRegion <- paste0(mainPath, "/", region, "/data")
  if (!file.exists(paste0(pathRegion, "/config.txt"))) {
    stop("Project main parameters have not been set yet. Run the initiate_project function.")
  }
  fileConn <- file(paste0(pathRegion, "/config.txt"))
  config <- readLines(fileConn)
  close(fileConn)
  param <- config[grepl(param, config)]
  param <- gsub("^[A-z]*\\:", "", param)
  return(param)
}

check_exists <- function (path, type, layer = TRUE, extension = NULL) {
  if (!is.character(path)) {
    stop("path must be 'character'")
  }
  if (!dir.exists(path)) {
    stop(paste(path,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  if (!type %in% c("raw", "processed")) {
    stop("type must be 'raw' or 'processed")
  }
  if (!is.logical(layer)) {
    stop("layer must be 'logical'")
  }
  if (!layer) {
    if (is.null(extension)) {
      stop("extension is required when 'layer' = FALSE")
    }
    if (!is.character(extension)) {
      stop("extension must be 'character'")
    }
  }
  fileLst <- list.files(path, full.names = FALSE, recursive = TRUE)
  if (type == "raw"){
    if (layer) {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.tif|/", type, "/.*\\.shp"), fileLst)], 1, 14)
    } else {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.", extension), fileLst)], 1, 14)
    }
  } else {
    if (layer) {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.tif|/", type, "/.*\\.shp"), fileLst)], 1 + 25, 14 + 25)
    } else {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.", extension), fileLst)], 1 + 25, 14 + 25)
    }
  }
  if (length(folderLst) != 0) {
    # Only keep sys.time folders
    folderLst <- folderLst[grepl("^[0-9]{14}$", folderLst)]
    folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
    return(folders)
  } else {
    return(NULL)
  }
}

check_downloaded <- function (folders) {
  indFolder <- paste(paste0("\n", folders))    
  cat("\nInput was already downloaded at the following date and time:\n")
  cat(indFolder)
  yn <- menu(c("YES","NO"), title="\n\nWould you like to download it again?")
  if (yn == 0) {
    stop_quietly("You exit the function.")
  }
  if (yn == 2) {
    stop_quietly("Download canceled")
  }
}

choose_input <- function (folders, msg, mostRecent = FALSE) {
  if (length(folders) > 1) {
    if (!mostRecent) {
      selInd <- menu(paste(msg, folders), title = "\nSelect the data you would like to use.")
    } else {
      selInd <- length(folders)
    }
    if (selInd == 0) {
      return(NULL)
    } else {
      folder <- folders[selInd]
      folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folder)
      return(folder)
    }
  } else {
    folder <- gsub("-|[[:space:]]|\\:|[A-z]", "", folders)
    return(folder)
  }
}

# Download administrative boundaries from geoBoundaries
download_boundaries <- function (mainPath, region, adminLevel, alwaysDownload = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!adminLevel %in% c(0,1,2,3,4,5)) {
    stop("Administrative level must be an integer from 0 to 5")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  # Check directory
  pathBorder <- paste0(mainPath, "/", region, "/data/vBorders")
  folders <- check_exists(pathBorder, "raw")
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  # Get country code
  iso <- get_param(mainPath, region, "ISO")
  # Download the data
  border <- NULL
  adminLevelTry <- adminLevel
  while (is.null(border) & adminLevelTry >= 0) {
    message(paste("Trying", region, "administrative level", adminLevelTry))
    border <- tryCatch({geoboundaries(iso, adm_lvl = adminLevelTry, quiet = FALSE)}, error = function(e){NULL})
    adminLevelTry <- adminLevelTry - 1
  }
  if (is.null(border)) {
    stop("No available shapefile from geoBoundaries for this country/region. You might have to download it manually.\n\n")
  }
  if (adminLevelTry < 0) {
    adminLevelTry <- 0
  }
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathBorder, "/", timeFolder, "/raw"), recursive = TRUE)
  pathBorder <- paste0(pathBorder, "/", timeFolder, "/raw")
  borderMeta <- gb_metadata(iso, adm_lvl = adminLevelTry)
  # Save metadata
  write.table(borderMeta,paste0(pathBorder, "/", borderMeta$boundaryID, ".txt"))
  # Save shapefile
  st_write(border, paste0(pathBorder, "/", borderMeta$boundaryID, ".shp"), append = FALSE)
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  write(paste0(sysTime, ": Boundaries downloaded from OSM (admin level ", adminLevelTry, ") - Input folder ", timeFolder), file = logTxt, append = TRUE)
  cat(paste0(pathBorder, "/", borderMeta$boundaryID, ".shp", "\n"))
}

get_boundaries <- function (mainPath, region, type, mostRecent) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!type %in% c("raw", "processed")) {
    stop("type must be 'raw' or 'processed")
  }
  # Check directory
  pathBorder <- paste0(mainPath, "/", region, "/data/vBorders")
  if (!dir.exists(pathBorder)) {
    stop(paste(pathBorder,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  folders <- check_exists(pathBorder, type, layer = TRUE)
  if (is.null(folders)) {
    stop(paste(str_to_title(type), "boundary shapefile is missing."))
  } else {
    if (type == "raw") {
      timeFolder <- choose_input(folders, "Shapefile downloaded at", mostRecent)
      if (is.null(timeFolder)) {
        stop_quietly("You exit the function.")
      } else {
        boundFolder <- paste0(pathBorder, "/", timeFolder, "/raw/")
        multipleFilesMsg <- "Select the boundary shapefile that you would like to use."
        message(paste("Loading", type, "boundaries..."))
        border <- load_layer(boundFolder, multipleFilesMsg)[[2]]
        return(border)
      }
    } else {
      timeFolder <- choose_input(folders, "Shapefile processed at", mostRecent)
      if (is.null(timeFolder)) {
        stop_quietly("You exit the function.")
      } else {
        folderLst <- list.dirs(pathBorder)
        boundFolder <-   folderLst[grepl(paste0("processed/", timeFolder), folderLst)]
        multipleFilesMsg <- "Select the boundary shapefile that you would like to use."
        message("\nLoading boundaries...")
        border <- load_layer(boundFolder, multipleFilesMsg)[[2]]
        return(border)
      }
    }
  } 
}

# Set the projection that will be used for the entire project
set_projection <- function (mainPath, region, mostRecent = FALSE, alwaysSet = FALSE, bestCRS = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  message("\nSuitable coordinate reference systems based on boundaries")
  # Write the EPSG in the config.txt file
  fileConn=file(paste0(mainPath, "/", region, "/data/config.txt"), open = "r")
  configTxt <- readLines(fileConn)
  close(fileConn)
  if (any(grepl(paste0("EPSG:"), configTxt))) {
    if (!alwaysSet) {
      yn <- menu(c("YES", "NO"), title = paste("\nThe projected coordinate system has already been set. Would you like to modify it?"))
      if (yn == 0) {
        stop_quietly("You exit the function.")
      }
      if (yn == 2) {
        epsg <- get_param(mainPath, region, "EPSG")
        stop_quietly(paste("EPSG previously set:", epsg))
      }
    } else {
      epsg <- get_param(mainPath, region, "EPSG")
      message(paste("EPSG previously set:", epsg))
    }
  }
  # Get the admin boundaries
  message("\nLoading raw boundary shapefile...")
  border <- get_boundaries(mainPath, region, "raw", mostRecent)
  if (!exists("crs_sf")) {
    stop("'crs_sf' table from 'crsuggest' is missing. Load the 'crsuggest' package.")
  }
  validEPSG <- crs_sf$crs_code[!is.na(crs_sf$crs_units) & crs_sf$crs_units=="m" & crs_sf$crs_type == "projected"]
  # Select projection
  cat(paste("\nEPSG:", suggest_top_crs(border), "seems to be the best projected coordinate reference for this region/country.\n"))
  if (bestCRS) {
    epsg <- suggest_top_crs(border)
  } else {
    suggestedCRS <- tryCatch({suggest_crs(input = border, type = "projected", limit = 100, units = "m")}, error=function(e){NULL})
    if (is.null(suggestedCRS)) {
      cat("\nNo reference system can be suggested. Enter the EPSG code that you want to use for this project.")
      cat("\n ")
      selInd <- readline(prompt = "Selection: ")
      epsg <- as.numeric(selInd)
      if (!epsg %in% validEPSG) {
        stop("EPSG not valid !")
      }
    } else {
      suggestedCRS <- paste(paste("EPSG:", suggestedCRS$crs_code), gsub(" .*$", "", suggestedCRS$crs_proj4))
      suggestedCRS <- c(suggestedCRS, "Other")
      valid <- FALSE
      while (!valid) {
        selectedProj <- menu(suggestedCRS, title = "Select projection for this project", graphics=TRUE)
        if (selectedProj == 0 | !exists("selectedProj")) {
          stop_quietly("You exit the function.")
        }
        if (selectedProj == length(suggestedCRS)) {
          cat("\n\nEnter the EPSG code that you want to use for this project.")
          cat("\n ")
          selInd <- readline(prompt = "Selection: ")
          epsg <- selInd
          if (!epsg %in% validEPSG) {
            message("EPSG not valid !")
            valid <- FALSE
          }else{
            valid <- TRUE
          }
        }else{
          epsg <- unlist(str_split(string = suggestedCRS[selectedProj], pattern = " "))[2]
          valid <- TRUE
        }
      }
    }
  }
  # Write the EPSG in the config.txt file
  fileConn=file(paste0(mainPath, "/", region, "/data/config.txt"), open = "r")
  configTxt <- readLines(fileConn)
  close(fileConn)
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  if(any(grepl(paste0("EPSG:"), configTxt))){
    epsgOld <- get_param(mainPath, region, "EPSG")
    if (epsgOld == epsg) {
      stop_quietly("\nNew projection equal to the one previously set. No change has been made.")
    }
    newValues <- gsub("EPSG:.*", paste0("EPSG:", epsg), configTxt)
    fileConn <- file(paste0(mainPath, "/", region, "/data/config.txt"), open = "w")
    writeLines(newValues, fileConn)
    close(fileConn)
    write(paste0(Sys.time(), ": Projection parameter changed (", epsg, ")"), file = logTxt, append = TRUE)
    warning("\nProjection parameter had already been set and has been changed. Inputs might have to be processed again.")
  }else{
    write(paste0("EPSG:", epsg), file = paste0(mainPath, "/", region, "/data/config.txt"), append = TRUE)
    write(paste0(Sys.time(), ": Projection parameter set (", epsg, ")"), file = logTxt, append = TRUE)
    message("\n\nProjection parameter has been set.")
  }
}

# Download DEM from SRTM (FABDEM only can be accessed through their website)
download_dem <- function (mainPath, region, alwaysDownload = FALSE, mostRecent = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  # Check directory
  pathDEM <- paste0(mainPath, "/", region, "/data/rDEM")
  folders <- check_exists(pathDEM, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  message("\nLoading raw boundary shapefile...")
  border <- get_boundaries(mainPath, region, "raw", mostRecent)
  border <- as(border, "Spatial")
  border <- gUnaryUnion(border)
  # Download SRTM tiles shapefile in a temporary folder
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathDEM, "/", timeFolder, "/raw"), recursive = TRUE)
  pathDEM <- paste0(pathDEM, "/", timeFolder, "/raw")
  tmpFolder <- paste0(pathDEM, "/temp")
  dir.create(tmpFolder)
  download.file(url = urlSRTM, destfile = paste0(tmpFolder, "/srtm.zip"))
  unzip(zipfile = paste0(tmpFolder, "/srtm.zip"), overwrite = TRUE, exdir= tmpFolder)
  shp <- shapefile(paste0(tmpFolder, "/srtm_country-master/srtm/tiles.shp"))
  intersects <- gIntersects(border, shp, byid=TRUE)
  tiles <- shp[intersects[,1],]
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  #Download tiles
  if (length(tiles) > 1) {
    srtmList  <- list()
    for (i in 1:length(tiles)) {
      cat(paste0("Downloading tile ", i, "/", length(tiles), "...\n"))
      lon <- extent(tiles[i,])[1]  + (extent(tiles[i,])[2] - extent(tiles[i,])[1]) / 2
      lat <- extent(tiles[i,])[3]  + (extent(tiles[i,])[4] - extent(tiles[i,])[3]) / 2
      tile <- getData('SRTM', lon = lon, lat = lat, path = paste0(tmpFolder,"/"))
      srtmList[[i]] <- tile
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    # Gdal mosaic (faster)
    files <- list.files(tmpFolder, pattern = "tif", full.names = TRUE)
    mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathDEM, "/srtm.tif") ,of = "GTiff")
    write(paste0(Sys.time(), ": Multiple DEM tiles downloaded and mosaicked"), file = logTxt, append = TRUE)
  }else{
    lon <- extent(tiles[1,])[1]  + (extent(tiles[1,])[2] - extent(tiles[1,])[1]) / 2
    lat <- extent(tiles[1,])[3]  + (extent(tiles[1,])[4] - extent(tiles[1,])[3]) / 2
    tile <- getData('SRTM', lon = lon, lat = lat, path = pathDEM)
    write(paste0(Sys.time(), ": Single DEM tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }
  unlink(tmpFolder, recursive = TRUE)
  files <- list.files(pathDEM)
  filesTif <- files[grepl("^.*\\.tif$", files)]
  mtime <- file.info(list.files(path = pathDEM, pattern="*.tif", full.names = TRUE))[,"mtime"]
  mostRecent <- which(order(as.POSIXct(mtime)) == 1)
  cat(paste0("\n", pathDEM, "/", filesTif[mostRecent], "\n"))
}

# Search in FTP folders
# Used in other functions (download_population)
navigate_ftp <- function (folderLst, iso, pathFTP, pathFTP0) {
  while (!(any(grepl("\\.tif$", folderLst)))) {
    # If there is a folder with our region code, select it
    isoProp <- sum(grepl("^[A-Z]{3}$", folderLst)) / length(folderLst)
    if (any(grepl(iso, folderLst))) {
      pathFTP <- paste0(pathFTP, iso,"/")
      # If not, let the user choose
    }else{
      if (isoProp == 1 & !any(grepl(iso, folderLst))) {
        pathFTP <- paste0(pathFTP,"../")
        message(paste(iso, "is not available in this dataset."))
      }else{
        if (!pathFTP == pathFTP0) {
          folderLst <- c(folderLst, "PREVIOUS DIRECTORY", "EXIT FUNCTION")
        }else{
          folderLst <- c(folderLst, "EXIT FUNCTION")
        }
        folderNb <- menu(c(folderLst), title="\nSelect folder (type the corresponding number or zero to get back to the root directory)?")
        if (folderNb == length(folderLst)) {
          return(NULL)
        }else if (folderNb == (length(folderLst)-1)) {
          pathFTP <- paste0(pathFTP, "../")
        }else if (folderNb == 0) {
          pathFTP <- pathFTP0
        }else{
          selectedFolder <- folderLst[folderNb]
          pathFTP <- paste0(pathFTP, selectedFolder, "/")
        }
      }
    }
    gc()
    folderLst <- getURL(pathFTP, verbose=FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
  }
  return(list(folderLst, pathFTP))
}

# Download population raster
download_population <- function (mainPath, region, alwaysDownload = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  # Check directory
  pathPop <- paste0(mainPath, "/", region, "/data/rPopulation")
  folders <- check_exists(pathPop, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  iso <- get_param(mainPath, region, "ISO")
  pathFTP0 <- ftpWorldPop
  pathFTP <- pathFTP0
  downloadProcess <- TRUE
  while (downloadProcess) {
    # To avoid error message
    gc()
    # Get directories
    folderLst <- getURL(pathFTP, verbose = FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
    # While we don't exit the function
    out <- navigate_ftp(folderLst, iso, pathFTP, pathFTP0)
    folderLst <- out[[1]]
    pathFTP <- out[[2]]
    if (is.null(folderLst)) {
      stop_quietly("You exit the function. No file has been downloaded.")
    }
    nFile <- 1:length(folderLst)
    indFile <- paste(paste0("\n", nFile, ": ", folderLst))
    cat(indFile)
    cat("\n\nSelect file (corresponding number) to be downloaded.\nType zero to get back to the root directory.\nSkip to exit the function.")
    cat("\n ")
    selInd <- readline(prompt = "Selection: ")
    selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
    if (length(selInd) == 0) {
      stop_quietly("You exit the function. No file has been downloaded.")
    }else if (0 %in% selInd) {
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    } else if (length(selInd) > 1) {
      message("Multiple selection is not allowed.")
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    } else if (!all(selInd %in% nFile)) {
      message("Multiple selection is not allowed.")
      downloadProcess <- TRUE
      pathFTP <- pathFTP0
    }else{
      logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
      sysTime <- Sys.time()
      timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      dir.create(paste0(pathPop, "/", timeFolder, "/raw"), recursive = TRUE)
      pathPop <- paste0(pathPop, "/", timeFolder, "/raw")
      for (i in selInd) {
        filePath <- paste0(pathFTP, folderLst[i])
        download.file(url = filePath, destfile = paste0(pathPop, "/", folderLst[i]), quiet = FALSE, mode = "wb", method = "libcurl")
        write(paste0(Sys.time(), ": Population raster downloaded from ", filePath, " - Input folder ", timeFolder), file = logTxt, append = TRUE)
        cat(paste0(pathPop, "/", folderLst[i], "\n"))
      }
      downloadProcess <- FALSE
    }
  }
}

# Download land cover
download_landcover <- function (mainPath, region, alwaysDownload = FALSE, mostRecent = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  # Check directory
  pathLandcover <- paste0(mainPath, "/", region, "/data/rLandcover")
  folders <- check_exists(pathLandcover, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  message("\nLoading raw boundary shapefile...")
  border <- get_boundaries(mainPath, region, "raw", mostRecent)
  # Based on https://lcviewer.vito.be/download and the names of available files for downloading
  # Coordinate intervals
  seqCoord <- list(X = seq(from = -180, to = 180, by = 20), Y = seq(from = -40, to = 80, by = 20))
  # Get extent of the boundaries
  minMax <- list(X = c(extent(border)[1], extent(border)[2]), Y = c(extent(border)[3], extent(border)[4]))
  # The file name: lower X limit, upper Y limit (see tiles at https://lcviewer.vito.be/download)
  # findInterval function: upper limit
  adjustTile <- c(X = -1, y = 0)
  prefixes <- list(c("E", "N") ,c("W", "S"))
  partialTileDeg <- vector(mode = "list", length = 2)
  tileName <- NULL
  for (i in 1:length(seqCoord)) {
    seqDeg <- seqCoord[[i]]
    coords <- minMax[[i]]
    for (j in 1:length(coords)) {
      coord <- coords[j]
      if (coord %in% seqDeg) {
        if (j==1) {
          coord <- coord+1
        }else{
          coord <- coord-1
        }
      }
      if (coord > max(seqDeg) | coord < min(seqDeg)) {
        stop("Region outside the limits of the Land Cover availability.")
      }
      getPosition <- findInterval(seqDeg, vec=coord)
      partialTileDeg[[i]][j] <- seqDeg[min(which(getPosition == 1)) + adjustTile[i]]
    }
  }
  seqTilesLst <- vector(mode = "list", length = 2)
  for (i in 1:length(partialTileDeg)) {
    seqTilesLst[[i]] <- seq(partialTileDeg[[i]][1], partialTileDeg[[i]][2], by = 20)
  }
  urls <- NULL
  codeFiles <- NULL
  for (i in seqTilesLst[[1]]) {
    if (i < 0) {
      pref <- prefixes[[2]][1]
    }else{
      pref <- prefixes[[1]][1]
    }
    missing0 <- (nchar(as.character(max(seqCoord[[1]]))) - nchar(as.character(abs(i))))
    if (missing0 == 2) {
      charX <- paste0(pref, "00", abs(i))
    }else if (missing0==1) {
      charX <- paste0(pref, "0", abs(i))
    }else{
      charX <- paste0(pref, abs(i))
    }
    for (j in seqTilesLst[[2]]) {
      if (j < 0) {
        pref <- prefixes[[2]][2]
      }else{
        pref <- prefixes[[1]][2]
      }
      missing0 <- (nchar(as.character(max(seqCoord[[2]]))) - nchar(as.character(abs(j))))
      if (missing0 == 1) {
        charY <- paste0(pref, "0", abs(j))
      }else{
        charY <- paste0(pref, abs(j))
      }
      codeFiles <- c(codeFiles, paste0(charX, charY))
      urls <- c(urls, paste0(awsLCFolder, charX, charY, "/", charX, charY, awsLCSuffix))
    }
  }
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathLandcover, "/", timeFolder, "/raw"), recursive = TRUE)
  pathLandcover <- paste0(pathLandcover, "/", timeFolder, "/raw")
  if (length(urls) == 1) {
    download.file(urls, destfile = paste0(pathLandcover, "/", region, awsLCSuffix, ".tif"), mode = "wb")
    write(paste0(Sys.time(), ": Single landcover tile downloaded - Input folder ", timeFolder), file = logTxt, append = TRUE)
  }else{
    # Download SRTM tiles shapefile in a temporary folder
    tmpFolder <- paste0(pathLandcover, "/temp")
    dir.create(tmpFolder)
    for (i in 1:length(urls)) {
      cat(paste0("Downloading tile ", i, "/", length(urls), "...\n"))
      download.file(urls[i], destfile = paste0(tmpFolder, "/", codeFiles[i], ".tif"), mode = "wb")
    }
    cat(paste0("Creating a mosaic with the downloaded rasters...\n"))
    files <- list.files(tmpFolder, pattern = "*.tif", full.names=TRUE)
    # Gdal mosaic
    mosaic_rasters(gdalfile = files, dst_dataset = paste0(pathLandcover, "/", region, awsLCSuffix, ".tif"), of="GTiff")
    write(paste0(Sys.time(), ": Multiple landcover tiles downloaded and mosaicked - Input folder ", timeFolder), file = logTxt, append = TRUE)
    unlink(tmpFolder, recursive = TRUE)
  }
  cat(paste0(pathLandcover, "/", region, awsLCSuffix, "\n"))
}

# Subset based on categories for automatically downloaded shapefiles
# Used in download_osm
select_categories <- function (sfObject, columnName) {
  sfDataFrame <- sfObject
  st_geometry(sfDataFrame) <- NULL
  categories <- unique(sfDataFrame[, columnName])
  nCat <- 1:length(categories)
  indCat <- paste(paste0("\n", nCat, ": ", categories))
  cat(indCat)
  cat("\n\nEnter all the indices that correspond to categories you want to keep.\nOn the same line separated by a space, or just skip to select all categories.\n")
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
  if (length(selInd) != 0) {
    categ <- categories[selInd]
    sfObject <- subset(sfObject,eval(parse(text=columnName)) %in% categ)
  } else {
    categ <- categories
  }
  return(list(sfObject, categ))
}

# Download shapefile from Open Street Map
download_osm <- function (x, mainPath, region, alwaysDownload = FALSE, countryName = TRUE, mostRecent = NULL) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!(x == "roads" | x == "waterLines" | x == "waterPolygons")) {
    stop("x must be 'roads', 'waterLines' or 'waterPolygons'")
  }
  if (!is.logical(alwaysDownload)) {
    stop("alwaysDownload must be 'logical'")
  }
  if (!is.logical(countryName)) {
    stop("countryName must be 'logical'")
  }
  if (!countryName) {
    if (is.null(mostRecent)) {
      stop("mostRecent is required when countryName is FALSE")
    }
    if (!is.logical(mostRecent)){
      stop("mostRecent must be 'logical'")
    }
  }
  pathFolder <- paste0(mainPath, "/", region, "/data/v", str_to_title(x))
  folders <- check_exists(pathFolder, "raw", layer = TRUE)
  if (!is.null(folders)) {
    if (!alwaysDownload) {
      check_downloaded(folders)
    }
  }
  if (x == "roads") {
    querySQL <- "SELECT * FROM 'lines' WHERE highway IS NOT NULL"
    colName <- "highway"
  }else if (x == "waterLines") {
    querySQL <- "SELECT * FROM 'lines' WHERE waterway IS NOT NULL"
    colName <- "waterway"
  }else{
    querySQL <- "SELECT * FROM 'multipolygons' WHERE natural IS NOT NULL"
    colName <- "natural"
  }
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(pathFolder, "/", timeFolder, "/raw"), recursive = TRUE)
  pathFolder <- paste0(pathFolder, "/", timeFolder, "/raw")
  # Download
  if (countryName) {
    countryName <- get_param(mainPath, region, "COUNTRY")
    shp <- oe_get(countryName,
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }else{
    message("\nLoading raw boundary shapefile...")
    border <- get_boundaries(mainPath, region, "raw", mostRecent)
    # Download 
    shp <- oe_get(st_bbox(border),
                  quiet = FALSE,
                  query = querySQL,
                  download_directory = pathFolder,
                  force_download = TRUE)
  }
  shpCat <- select_categories(shp, colName)
  shp <- shpCat[[1]]
  categ <- shpCat[[2]]
  shapeName <- gsub("\\.gpkg$", "", list.files(pathFolder)[grepl("\\.gpkg$", list.files(pathFolder))])
  st_write(shp, paste0(pathFolder, "/v", str_to_title(x), "_", shapeName, ".shp"), append=FALSE) # Save the layer
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  write(paste0(Sys.time(), ": ", x, " dowloaded from OSM; ", paste(categ, collapse = ", "), "- Input folder ", timeFolder), file = logTxt, append = TRUE)
  file.remove(paste0(pathFolder, "/", list.files(pathFolder)[grepl("\\.gpkg$|\\.pbf$", list.files(pathFolder))]))
  cat(paste0(pathFolder, "/v", str_to_title(x), "_", shapeName,".shp", "\n"))
}

# Select directories to be processed
select_DirFile <- function (x, msg) {
  n <- 1:length(x)
  indInput <- paste(paste0("\n", n, ": ", x))
  cat(indInput)
  cat(paste0("\n\n", msg, "\n"))
  selInd <- readline(prompt = "Selection: ")
  selInd <- as.numeric(unlist(strsplit(x = selInd, split = " ")))
  if (length(selInd) != 0) {
    if (!(all(is.numeric(selInd)) & all(selInd > 0))){
      stop("Input user must be positive integers.")
    }
    selectedX <- x[selInd]
  }else{
    selectedX <- x
  }
  return(selectedX)
}

# Process raster: crop, mask and project
process_raster <- function (ras, border, epsg, projMeth) {
  border <- st_transform(as(border, "sf"), crs(ras))
  cat(paste("Cropping:\n", ras %>% sources()))
  rasCrop <- terra::crop(ras, border)
  cat(paste("\n\nMasking:\n", ras %>% sources()))
  rasMask <- terra::mask(rasCrop, as(border, "SpatVector"))
  if (is.null(projMeth)) {
    projectionMethod <- c("near", "bilinear","cubic", "cubicspline")
    pm <- menu(projectionMethod, title = cat(paste0("\n\nSelect projection method for:\n", ras %>% sources(),"\nSee terra::project function help for more details.")))
    if (pm == 0) {
      return(NULL)
    } else {
      projMeth <- projectionMethod[pm]
    }
  } 
  cat(paste("\nProjecting:\n", ras %>% sources(), "\n"))
  rasProject <- terra::project(rasMask, epsg, method = projMeth)
  return(list(rasProject, projMeth))
}

# Resample raster
resample_raster <- function (ras1, ras0, rasInit, resampMeth) {
  if (is.null(resampMeth)) {
    resamplingMethod <- c("near", "bilinear", "cubic", "cubicspline", "lanczos", "sum", "min", "q1", "q3", "max", "average", "mode", "rms")
    resm <- menu(resamplingMethod, title = cat(paste("\n\nSelect resampling method for:\n", rasInit %>% sources(),"\nSee terra::resample function help for more details.")))
    if (resm == 0) {
      return(NULL)
    } else {
      resampMeth <- resamplingMethod[resm]
    }
  }
  cat(paste("\nResampling:\n", rasInit %>% sources(),"\n"))
  rasResamp <- terra::resample(ras1, ras0, method = resampMeth)
  return(list(rasResamp, resampMeth))
}

# Process shapefile: clip and project
process_shapefile <- function (shp, borderInit, epsg, inputName) {
  cat(paste("\nProjecting:", inputName, "shapefile\n"))
  shp <- st_transform(shp, st_crs(epsg))
  border <- st_transform(as(border, "sf"), crs(shp))
  cat(paste("\nClipping:", inputName, "shapefile\n\n"))
  shpInter <- st_intersects(shp, as(border, "sf")) 
  shpInter <- lengths(shpInter) > 0
  shpClip <- shp[shpInter, ]
  return(shpClip)
}

# Procees again ?
already_processed <- function (path, alwaysProcess) {
  prFiles <- list.files(gsub("raw", "processed", path), recursive = TRUE, pattern = "*.tif|*.shp")
  if (length(prFiles) > 0) {
    if (!alwaysProcess) {
      inputName <- gsub("/raw$", "", gsub("^.*/data/", "", path))
      yn <- menu(c("YES", "NO"), title = cat(paste("\n", inputName, "was already processed. Would you like to reprocess it?")))
      if (yn==1) {
        process <- TRUE
      }else{
        process <- FALSE
      }
    } else {
      process <- TRUE
    }
  }else{
    process <- TRUE
  }
  return(process)
}

# Check whether the input is a shapefile or a raster
load_layer <- function (folder, multiMsg, load = TRUE) {
  rasterLayer <- FALSE
  vectorLayer <- FALSE
  files <- list.files(folder)
  if (length(files) > 0) {
    filesTif <- files[grepl("\\.tif$", files)]
    if (length(filesTif) > 0) {
      rasterLayer <- TRUE
    }
    filesShp <- files[grepl("\\.shp$", files)]
    if (length(filesShp) > 0) {
      vectorLayer <- TRUE
    }
  } else {
    # Usually not necessary this function is used on folders with available data
    # Check is performed previously
    stop("Input is missing.")
  }
  if (rasterLayer) {
    if (length(filesTif) > 1) {
      fileInd <- menu(filesTif, multiMsg)
      file <- filesTif[fileInd]
    }else{
      file <- filesTif
    }
    if (load) {
      ras <- rast(paste0(folder, "/", file))
    } else {
      ras <- paste0(folder, "/", file)
    }
  }else{
    ras <- NULL
  }
  if (vectorLayer) {
    if (length(filesShp) > 1) {
      fileInd <- menu(filesShp, multiMsg)
      file <- filesShp[fileInd]
    }else{
      file <- filesShp
    }
    if (load) {
      shp <- st_read(paste0(folder, "/", file), quiet=TRUE)
    } else {
      shp <- paste0(folder, "/", file)
    }
  }else{
    shp <- NULL
  }
  return(list(ras, shp))
}

check_input <- function (mainPath, region, type, print = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!type %in% c("raw","processed")) {
      stop("type must be 'raw' or 'processed'")
  }
  if (!is.logical(print)) {
    stop("print must be 'logical'")
  }
  fileLst <- list.files(paste0(mainPath, "/", region, "/data"), recursive = TRUE)
  fileLst <- fileLst[!grepl("zToAccessMod", fileLst)]
  fileAv <- fileLst[grepl(paste0(type, "/.*(\\.tif|\\.shp)"), fileLst)]
  folderAv <- unique(gsub("/[0-9].*$", "", fileAv))
  folderLst <- list.dirs(paste0(mainPath, "/", region, "/data"), recursive = TRUE)[-1]
  folderLst <- folderLst[!grepl("zToAccessMod", folderLst)]
  folderLst <- unique(gsub("/[0-9].*$", "", folderLst))
  folderLst <- gsub("^.*/data/", "", folderLst)
  folderNAv <- folderLst[!folderLst %in% folderAv]
  if (length(folderAv) > 0) {
    if (length(folderAv) == 1) {
      message(paste("\nThe following", type, "input is AVAILABLE:"))
      cat(folderAv)
      cat("\n")
    } else {
      message(paste("\nThe following", type, "inputs are AVAILABLE:"))
      cat(paste(folderAv, collapse = ", "))
      cat("\n")
    }
  }
  if (length(folderNAv) > 0) {
    if (length(folderNAv) == 1) {
      message(paste("\nThe following", type, "input is UNAVAILABLE:"))
      cat(folderNAv)
      cat("\n")
    } else {
      message(paste("\nThe following", type, "inputs are UNAVAILABLE:"))
      cat(paste(folderNAv, collapse = ", "))
      cat("\n")
    }
  }
  if (!print) {
    return(folderAv)
  }
}

process_pop <- function (mainPath, region, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes) {
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  message("\nProcessing population raster...")
  popFolder <- paste0(mainPath, "/", region, "/data/rPopulation")
  popFolders <- check_exists(popFolder, "raw", layer = TRUE)
  if (is.null(popFolders)) {
    stop("No input population raster available.")
  }
  timeFolder <- choose_input(popFolders, "Raster downloaded at", mostRecent)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")
  }
  popFolder <- paste0(popFolder, "/", timeFolder, "/raw")
  multipleFilesMsg <- "Select the population raster that you would like to process."
  popRas <- load_layer(popFolder, multipleFilesMsg)[[1]]
  if (defaultMethods) {
    projMeth <- "bilinear"
  } else {
    projMeth <- NULL
  }
  popReprojMeth <- process_raster(popRas, border, epsg, projMeth = projMeth)
  popReproj <- popReprojMeth[[1]]
  projMeth <- popReprojMeth[[2]]
  if (is.null(popReproj)) {
    stop_quietly("You exit the function.")
  }
  write(paste0(Sys.time(), ": Population raster cropped, masked and projected (", epsg, ") using the '", projMeth, "' method - Input folder: ", timeFolder), file = logTxt, append = TRUE)
  # Initial resolution
  if (is.null(changeRes)) {
    resInit <- terra::res(popReproj)
    yn <- menu(c("YES", "NO"), title = paste("\nThe resolution of the population raster is", round(resInit[1], 2), "m. Would you like to modify it?"))
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
  } else if (changeRes) {
    resInit <- terra::res(popReproj)
    yn <- 1
  } else {
    yn <- 2
  }
  if (yn == 1) {
    if (is.null(newRes)) {
      cat("\nEnter the new resolution (m)\n")
      newRes <- readline(prompt = "Selection: ")
      newRes <- as.numeric(newRes)
      k <- 0
      while ((is.na(newRes) | newRes < 0) & k < 3) {
        message("Resolution must be a real positive number.")
        k <- k + 1
        newRes <- readline(prompt = "Selection: ")
        newRes <- as.numeric(newRes)
      }
      if ((is.na(newRes) | newRes < 0) & k == 3) {
        stop("Invalid resolution!")
      }
    }
    popReprojNew <- popReproj
    res(popReproj) <- newRes
    if (defaultMethods) {
      if (newRes[1] >= resInit[1]) {
        resampMeth <- "sum"
      } else {
        resampMeth <- "bilinear"
      }
    } else {
      resampMeth <- NULL
    }
    popFinalMeth <- resample_raster(popReprojNew, popReproj, popRas, resampMeth)
    popFinal <- popFinalMeth[[1]]
    resampMeth <- popFinalMeth[[2]]
    write(paste0(Sys.time(), ": Population raster resampled using the '", resampMeth, "' method - Input folder: ", timeFolder), file = logTxt, append = TRUE)
  }else{
    popFinal <- popReproj
  }
  if (is.null(popCorrection)) {
    ynCorr <- menu(c("YES", "NO"), title = "\nReprojecting a raster always causes some (small) distortion in the grid of a raster.\nWould you like to correct it (see 'help' for more details)?")
    if (ynCorr == 0) {
      stop_quietly("You exit the function.")
    }
  } else if (popCorrection) {
    ynCorr <- 1
  } else {
    ynCorr <- 2
  }
  if (ynCorr == 1) {
    if (is.null(gridRes)) {
      cat("\nEnter the resolution of the grid for the zonal statistic used for correcting the population raster (m)\n")
      gridRes <- readline(prompt = "Selection: ")
      gridRes <- as.numeric(gridRes)
      k <- 0
      while ((is.na(gridRes) | gridRes < 0) & k < 3) {
        message("Resultion must be a real positive number.")
        k <- k + 1
        gridRes <- readline(prompt = "Selection: ")
        gridRes <- as.numeric(gridRes)
      }
      if ((is.na(gridRes) | gridRes < 0) & k == 3) {
        stop("Invalid resolution!")
      }
    }
    # Transform border for following process
    border <- st_transform(as(border, "sf"), crs(popFinal))
    grd <- st_as_sf(st_make_grid(border, cellsize = gridRes))
    # We don't do that, because then, partial cells are not pixelized with fasterize
    # So border areas may become NA, leading to a loss of population when multiplied by the zonalStat raster
    # grdInter <- gIntersection(gUnaryUnion(as(border, "Spatial")), as(grd, "Spatial"), byid = TRUE)
    # grdInterPoly <- st_cast(as(grdInter, "sf"), "MULTIPOLYGON")
    cat("\nSumming values of the original population raster per grid cell\n")
    popSum <- exact_extract(popRas, st_transform(as(grd, "sf"), crs(popRas)), "sum")
    cat("\nSumming values of the processed population raster per grid cell before correction\n")
    popFinalSum <- exact_extract(popFinal, grd, "sum")
    # Ratio per grid cell
    grd$pop_diff <- popSum / popFinalSum
    # The only zones that are not going to be corrected are the ones that
    # initially had some population but that lost them with projection.
    # Ratio is infinite, which became NA in R.
    zonalStat  <- fasterize(grd, as(popFinal, "Raster"), "pop_diff")
    popOut <- popFinal * as(zonalStat, "SpatRaster")
    sysTime <- Sys.time()
    outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
    popOutFolder <- paste0(gsub("raw", "processed", popFolder), "/", outTimeFolder)
    dir.create(popOutFolder, recursive = TRUE)
    writeRaster(popOut, paste0(popOutFolder, "/rPopulation.tif"), overwrite=TRUE)
    write(paste0(Sys.time(), ": Population raster corrected using a grid of ", gridRes, " x ", gridRes, " m cells"), file = logTxt, append = TRUE)
    cat("\nSumming values of the processed population raster per grid cell after correction\n")          
    popOutSum <- exact_extract(popOut, grd, "sum")
    message(paste0("Mean difference per grid cell (", gridRes, " x ", gridRes, ") ", "before correction: ", round(mean(popFinalSum - popSum), 2)))
    message(paste0("Mean difference per grid cell (", gridRes, " x ", gridRes, ") ", "after correction: ", round(mean(popOutSum - popSum), 2)))
    write(paste0(Sys.time(), ": Mean difference per grid cell before correction: ", round(mean(popFinalSum - popSum), 2), " - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    write(paste0(Sys.time(), ": Mean difference per grid cell after correction: ", round(mean(popOutSum - popSum), 2), " - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    write(paste0(Sys.time(), ": Processed population raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
  } else {
    popOut <- popFinal
    sysTime <- Sys.time()
    outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
    popOutFolder <- paste0(gsub("raw", "processed", popFolder), "/", outTimeFolder)
    dir.create(popOutFolder, recursive = TRUE)
    writeRaster(popOut, paste0(popOutFolder, "/rPopulation.tif"), overwrite=TRUE)
    write(paste0(Sys.time(), ": Processed population raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
  }
}
  
 
# Main function to process the inputs
process_inputs <- function (mainPath, region, mostRecent = FALSE, alwaysProcess = FALSE, defaultMethods = FALSE, changeRes = NULL, newRes = NULL, popCorrection = NULL, gridRes = NULL, all = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(mostRecent)) {
    stop("mostRecent must be 'logical'")
  }
  if (!is.logical(alwaysProcess)) {
    stop("alwaysProcess must be 'logical'")
  }
  if (!is.null(changeRes)) {
    if (!is.logical(changeRes)) {
      stop("mostRecent must be NULL or 'logical'")
    }
  }
  if (!is.null(newRes)) {
    if (!is.numeric(newRes) & newRes > 0) {
      stop("newRes must be NULL or a real positive number'")
    }
  }
  if (!is.null(popCorrection)) {
    if (!is.logical(popCorrection)) {
      stop("popCorrection must be NULL or 'logical'")
    }
  }
  if (!is.null(gridRes)) {
    if (!is.numeric(gridRes) & newRes > 0) {
      stop("gridRes must be NULL or a real positive number'")
    }
  }
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  epsg <- get_param(mainPath = mainPath, region = region, "EPSG")
  epsg <- paste0("EPSG:", epsg)
  if (length(epsg) == 0) {
    stop("EPSG for projection is not set. Run the set_projection function.")
  }
  rawFolders <- check_input(mainPath, region, "raw")
  if (length(rawFolders) == 0) {
    stop("No input data available.")
  }
  if (!all) {
    selectedFolders <- select_DirFile(rawFolders, "Enter all the indices that correspond to the inputs you want to process.\nOn the same line separated by a space, or just skip to select all inputs.")
  } else {
    selectedFolders <- rawFolders
  }
  # Border is required for any input processing; if wanted, first process this layer
  borderPath <- paste0(mainPath, "/", region, "/data/vBorders")
  borderPr <- check_exists(borderPath, "processed", layer = TRUE)
  # If we want to process border or if no processed shapefile exists (required for any other processing)
  if (("vBorders" %in% selectedFolders) | is.null(borderPr)) {
    borderFolders <- check_exists(borderPath, "raw", layer = TRUE)
    if (is.null(borderFolders)) {
      stop("\nBoundary shapefile is required for input processing. Run the download_boundaries function.")
    }
    message("\nLoading raw shapefile of boundaries...")
    timeFolder <- choose_input(borderFolders, "Shapefile downloaded at", mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    }
    borderFolder <- paste0(borderPath, "/", timeFolder, "/raw")
    toProcess <- already_processed(borderFolder, alwaysProcess)
    if (toProcess) {
      border <- load_layer(borderFolder)[[2]]
      border <- st_transform(border, crs(epsg))
      write(paste0(Sys.time(), ": vBorders shapefile projected (", epsg, ") - Input folder: ", timeFolder), file = logTxt, append = TRUE)
      sysTime <- Sys.time()
      outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      borderOutFolder <- paste0(gsub("raw", "processed", borderFolder), "/", outTimeFolder)
      dir.create(borderOutFolder, recursive = TRUE)
      st_write(border, paste0(borderOutFolder, "/vBorders.shp"), append=FALSE)
      write(paste0(Sys.time(), ": Processed vBorders shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
      selectedFolders <- selectedFolders[!grepl("vBorders", selectedFolders)]
    }
  }
  if (length(selectedFolders) < 1) {
    stop_quietly("No more input to be processed!")
  }
  message("\nLoading processed boundary shapefile...")
  border <- get_boundaries(mainPath, region, "processed", mostRecent)
  if ("rPopulation" %in% selectedFolders) {
    process_pop(mainPath, region, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes)
    selectedFolders <- selectedFolders[!grepl("rPopulation", selectedFolders)]
  }
  # Check if other rasters to be processed
  filesRasTrue <- NULL
  for (i in 1:length(selectedFolders)) {
    files <- list.files(paste0(mainPath, "/", region, "/data/", selectedFolders[i]), recursive = TRUE)
    filesRasTrue <- c(filesRasTrue, any(grepl("raw/.*\\.tif",files)))
  }
  if (any(filesRasTrue)) {
    popFolder <- paste0(mainPath, "/", region, "/data/rPopulation")
    popFolders <- check_exists(popFolder, "processed", layer = TRUE)
    if (is.null(popFolders)) {
      message("\nNo processed population raster is available.\nProcessing raw population raster...")
      process_pop(mainPath, region, border, epsg, mostRecent, defaultMethods, changeRes, newRes, popCorrection, gridRes)
      popFolders <- check_exists(popFolder, "processed", layer = TRUE)
    }
    message("\nLoading processed population raster...")
    timeFolder <- choose_input(popFolders, "Population raster processed at:", mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    } else {
      popFolderLst <- list.dirs(popFolder)
      popFolder <- popFolderLst[grepl(paste0("processed/", timeFolder), popFolderLst)]
      multipleFilesMsg <- "Select the population raster that you would like to process."
      popOut <- load_layer(popFolder, multipleFilesMsg)[[1]]
    }
  }
  for (i in 1:length(selectedFolders)) {
    cat("\n")
    message(selectedFolders[i])
    inputFolder <- paste0(mainPath, "/", region, "/data/", selectedFolders[i])
    inputFolders <- check_exists(inputFolder, "raw", layer = TRUE)
    timeFolder <- choose_input(inputFolders, paste(selectedFolders[i], "downloaded at:"), mostRecent)
    if (is.null(timeFolder)) {
      stop_quietly("You exit the function.")
    }
    inputFolder <- paste0(inputFolder, "/", timeFolder, "/raw")
    processLayer <- already_processed(inputFolder, alwaysProcess)
    if (!processLayer) {
      if (i == length(selectedFolders)) {
        stop_quietly("No more input to be processed!")
      }else{
        next
      }
    }
    multipleFilesMsg <- "Select the input that you want to process."
    inputLayers <- load_layer(inputFolder, multipleFilesMsg)
    if (!is.null(inputLayers[[1]])) {
      if (defaultMethods) {
        if (grepl("Land|land", selectedFolders[i])) {
          projMeth <- "near"
          resampMeth <- "near"
        } else {
          projMeth <- "bilinear"
          resampMeth <- "bilinear"
        }
      } else {
        projMeth <- NULL
        resampMeth <- NULL
      }
      rasReprojMeth <- process_raster(inputLayers[[1]], border, epsg, projMeth)
      rasReproj <- rasReprojMeth[[1]]
      projMeth <- rasReprojMeth[[2]]
      write(paste0(Sys.time(), ": ", selectedFolders[i], " raster cropped, masked and projected using the '", projMeth, "' method - Input folder: ",timeFolder), file = logTxt, append = TRUE)
      rasResampledMeth <- resample_raster(rasReproj, popOut, inputLayers[[1]], resampMeth)
      rasResampled <- rasResampledMeth[[1]]
      resampMeth <- rasResampledMeth[[2]]
      write(paste0(Sys.time(), ": ", selectedFolders[i], " raster resampled using the '", resampMeth, "' method - Input folder: ",timeFolder), file = logTxt, append = TRUE)
      sysTime <- Sys.time()
      outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      outFolder <- paste0(gsub("raw", "processed", inputFolder), "/", outTimeFolder)
      dir.create(outFolder, recursive = TRUE)
      writeRaster(rasResampled, paste0(outFolder, "/", selectedFolders[i], ".tif"), overwrite=TRUE)
      write(paste0(Sys.time(), ": Processed ", selectedFolders[i], " raster saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    }
    if (!is.null(inputLayers[[2]])) {
      shpProcessed <- process_shapefile(inputLayers[[2]], border, epsg, selectedFolders[i])
      write(paste0(Sys.time(), ": ", selectedFolders[i], " shapefile projected and clipped - Input folder: ", timeFolder), file = logTxt, append = TRUE)
      sysTime <- Sys.time()
      outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
      outFolder <- paste0(gsub("raw", "processed", inputFolder), "/", outTimeFolder)
      dir.create(outFolder, recursive = TRUE)
      st_write(shpProcessed, paste0(outFolder, "/", selectedFolders[i], ".shp"), append=FALSE)
      write(paste0(Sys.time(), ": Processed ", selectedFolders[i], " shapefile saved - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
    }
  }
  cat("\nDone!\n")
}

compile_processed_data <- function (mainPath, region, mostRecent = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.logical(mostRecent)) {
    stop("mostRecent must be 'logical'")
  }
  folderLst <- list.dirs(paste0(mainPath, "/", region))
  inputsAv <- folderLst[grepl("processed", folderLst)]
  inputsAv <- unique(gsub("/[0-9]{14}/processed.*", "", gsub("^.*/data/", "", inputsAv)))
  if (length(inputsAv) == 0) {
    stop("No processed inputs available.")
  }
  sysTime <- Sys.time()
  outTimeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  outFolder <- paste(mainPath, region, "data/zToAccessMod", outTimeFolder, sep = "/")
  dir.create(outFolder, recursive = TRUE)
  for (i in 1:length(inputsAv)) {
    inputFolder <- paste0(mainPath, "/", region, "/data/", inputsAv[i])
    inputFolders <- check_exists(inputFolder, "processed", layer = TRUE)
    timeFolder <- choose_input(inputFolders, paste(inputsAv[i], "downloaded at:"), mostRecent) 
    inputFolder <- folderLst[grepl(paste0("processed/", timeFolder), folderLst)]
    files <- list.files(inputFolder, full.names = TRUE)
    for (file in files) {
      cat(paste("\nCopying", file, "to", outFolder))
      file.copy(from = file, to = outFolder, copy.date = TRUE, overwrite = TRUE)
    }
  }
  write(paste0(Sys.time(), ": ", paste(inputsAv, collapse = ", "), "copied to 'zToAccessMod' - Output folder: ", outTimeFolder), file = logTxt, append = TRUE)
}

