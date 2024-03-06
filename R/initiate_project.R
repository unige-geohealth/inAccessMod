#' Initiate Project
#'
#' Select the country or city, get the ISO 3166-1 alpha-3 country code, store it in a config.txt file and create the directory 
#' main structure for the project. This function also creates a log.txt file that will record and track the main operations 
#' related to the project.
#' @param mainPath character; a path where the country/city folder will be created
#' @param allowInteractivity logical; whether to enable interactivity. \code{TRUE} by default.
#' @param city logical; whether to focus on cities instead of countries. \code{FALSE} by default.
#' @param name character; country or city name when \code{allowInteractivity} is set to \code{FALSE}. Must match perfectly either one of 
#' the names included in inAccessMod::country_list (country) or inAccessMod::city_list (city). For cities, can also be the name 
#' of the city combined with the country ISO2 code when > 1 city have the same name (ex. 'Vancouver'). In this case the format 
#' is the following: city name, white space, hyphen, space, code (ex. 'Zurich - CH'). This parameter is ignored when \code{allowInteractivity} 
#' is set to \code{TRUE}.
#' @param iso character; optional and only when city is TRUE, the ISO 3166-1 alpha-3 country code.
#' The ESRI World Urban Areas dataset may have inaccurate country information for cities situated near borders, and this parameter allows the user
#' to set up the country in which is located the city. Using an incorrect code for downloading the population raster 
#' can result in either an issue or obtaining the wrong population dataset.
#' @param testMode logical; \code{FALSE} by default. Can be ignored. used for testing the function in the testthat context.
#' @details The final structure arises when downloading and processing the data with the corresponding functions,
#' and it allows multiple 'raw' inputs and multiple 'processed' outputs for each input. This can be useful when 
#' performing different analyses for the same country (e.g. when we have updated data).
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath)}
#' @export
initiate_project <- function (mainPath, allowInteractivity = TRUE, city = FALSE, name = NULL, iso = NULL, testMode = FALSE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!dir.exists(mainPath)) {
    stop(paste(mainPath, "folder does not exist."))
  }
  if (testMode) {
    allowInteractivity <- FALSE
    city <- FALSE
    name <- "Switzerland"
    # Get the ISO code
    iso3 <- as.character(country_list[country_list$country.name.en == name, "iso3c"])
    countryOriginalName <- name
  } else {
    if (!is.logical(allowInteractivity)) {
      stop("allowInteractivity must be 'logical'")
    }
    if (!allowInteractivity) {
      if (is.null(name)) {
        stop("When allowInteractive is FALSE, a name must be provided.")
      }
      if (!city) {
        if (!name %in% inAccessMod::country_list$country.name.en) {
          stop(paste(name, "is no a valid country name."))
        } else {
          iso3 <- as.character(country_list[country_list$country.name.en == name, "iso3c"])
          countryOriginalName <- name
        }
      } else {
        cityLst <- paste0(inAccessMod::city_list$Name, " - ", inAccessMod::city_list$ISO_CC)
        cityLstN <- inAccessMod::city_list$Name
        if (!name %in% c(cityLst, cityLstN)) {
          print(cityLst)
          stop(paste(name, "is no a valid city name. Please use one of the names above. If the country code is incorrect, indicate the name of the city without the code, and set the 'iso' parameter correctly."))
        } else {
          if (grepl(" - [A-Z]{2}$", name)) {
            cityInd <- which(cityLst == name)
            if (length(cityInd) > 1) {
              stop("> 1 city with this name/ISO2 code combination; unexpected")
            }
          } else {
            cityInd <- which(cityLstN == name)
            if (length(cityInd) > 1) {
              stop("> 1 city with this name; please add the ISO2 country code as following: 'name - XX' (ex. 'Zurich - CH'")
            }
          }
        }
      }
    } else {
      if (!city) {
        countryInd <- utils::menu(inAccessMod::country_list$country.name.en, title = "Select the country", graphics = FALSE)
        if (countryInd == 0) {
          stop_quietly("You exit the function.")
        }
        name <- inAccessMod::country_list$country.name.en[countryInd]
        iso3 <- as.character(country_list[country_list$country.name.en == name, "iso3c"])
        countryOriginalName <- name
      } else {
        # Some city names exist in multiple country (ex. Vancouver)
        cityLst <- paste0(inAccessMod::city_list$Name, " - ", inAccessMod::city_list$ISO_CC)
        cityInd <- utils::menu(cityLst, title = "Select the city", graphics = FALSE)
        if (cityInd == 0) {
          stop_quietly("You exit the function.")
        }
      }
    }
  }

  if (city) {
    if (!is.null(iso)) {
      if (!grepl("^[A-Z]{3}$", iso)) {
        stop("Invalid 'iso' parameter: must be an ISO 3166-1 alpha-3 country code.")
      }
      if (!iso %in% country_list$iso3c) {
        stop("Invalid 'iso' parameter: must be a valid ISO 3166-1 alpha-3 country code.")
      }
      iso3 <- iso
      countryOriginalName <- as.character(country_list[which(country_list$iso3c == iso), "country.name.en"])
    } else {
      iso2 <- inAccessMod::city_list$ISO_CC[cityInd]
      iso3 <- as.character(country_list[which(country_list$iso2c == iso2), "iso3c"])
      countryOriginalName <- as.character(country_list[which(country_list$iso2c == iso2), "country.name.en"])
    }
    name <- inAccessMod::city_list$Name[cityInd]
    cityOriginalName <- name
  }
  # Modify the name if necessary for directory name
  folderName <- gsub(" \\(.*\\)|\\(.*\\)", "", name)
  folderName <- gsub("[^[[:alnum:]]", " ", folderName)
  folderName <- stringr::str_squish(folderName)
  folderName <- gsub("[[:space:]]", "_", folderName)
  folderName <- stringi::stri_trans_general(str = folderName, id = "Latin-ASCII")
  
  # Main standard inputs
  inputNames <- c("rDEM", "rPopulation", "rLandcover", "vRoads", "vWaterLines", 
               "vWaterPolygons", "vBorders","vFacilities")
  message(paste("\nThe following input folders will be created:", paste(inputNames, collapse=", ")))
  # Add other data ?
  if (testMode | !allowInteractivity) {
    yn <- 2
  } else {
    yn <- utils::menu(c("YES","NO"), title="\nDo you want to add another input folder (type 1 or 2)?")
  }
  if (yn == 0) {
    stop_quietly("You exit the function.")
  }
  while (yn == 1) {
    newName <- readline(prompt = "Enter the folder name: ")
    newName <- gsub(" \\(.*\\)|\\(.*\\)", "", newName)
    newName <- gsub("[^[[:alnum:]]", " ", newName)
    newName <- stringr::str_squish(newName)
    newName <- gsub("[[:space:]]", "_", newName)
    newName <- stringi::stri_trans_general(str = newName, id = "Latin-ASCII")
    types <- c("r", "v")
    type <- utils::menu(c("Raster data", "Vector data"), title="\nWhich type of data will contain this folder?")
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
    newName <- paste0(types[type], stringr::str_to_title(newName))
    inputNames <- c(inputNames, newName)
    yn <- utils::menu(c("YES", "NO"), title="\nDo you want to add another input folder?")
    if (yn == 0) {
      stop_quietly("You exit the function.")
    }
  }
  # Create directories
  pathData <- file.path(mainPath, folderName, "data")
  check_path_length(pathData)
  dir.create(pathData, recursive = TRUE, showWarnings = FALSE)
  for (inputName in inputNames) {
    pathInput <- paste0(pathData, "/", inputName)
    check_path_length(pathInput)
    dir.create(pathInput, showWarnings = FALSE)
  }
  # Create config.txt for ISO code, and then EPSG as well
  fileConn <- file(file.path(pathData, "config.txt"))
  if (city) {
    writeLines(c(paste0("CITY:", cityOriginalName), paste0("COUNTRY:", countryOriginalName), paste0("ISO:", iso3)), fileConn)
  } else {
    writeLines(c(paste0("COUNTRY:", countryOriginalName), paste0("ISO:", iso3)), fileConn)
  }
  close(fileConn)
  # Create log.txt for operation tracking
  fileConn <- file(file.path(pathData, "log.txt"), open = "a")
  if (city) {
    writeLines(cityOriginalName, fileConn)
  } else {
    writeLines(countryOriginalName, fileConn)
  }
  writeLines(paste0(Sys.time(), ": Project initiated"), fileConn)
  close(fileConn)
  
  if (city) {
    indShp <- which(paste0(inAccessMod::world_urban_areas$Name, " - ", inAccessMod::world_urban_areas$ISO_CC) == cityLst[cityInd])
    shp <- inAccessMod::world_urban_areas[indShp, ]
    pathBorder <- file.path(mainPath, folderName, "data", "vBorders")
    timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
    check_path_length(paste0(pathBorder, "/", timeFolder, "/raw"))
    dir.create(paste0(pathBorder, "/", timeFolder, "/raw"), recursive = TRUE)
    pathBorder <- file.path(pathBorder, timeFolder, "raw")
    sf::st_write(shp, file.path(pathBorder, paste0(folderName, ".shp")))
    fileConn <- file(file.path(pathData, "log.txt"), open = "a")
    writeLines(paste0(Sys.time(), ": Urban area shapefile extracted"), fileConn)
    close(fileConn)
  }
  # Print directory tree
  fs::dir_tree(pathData)
  if (city) {
    if (!is.null(iso)) {
      if (!grepl("^[A-Z]{3}$", iso)) {
        stop("Invalid 'iso' parameter: must be an ISO 3166-1 alpha-3 country code.")
      }
      if (!iso %in% country_list$iso3c) {
        stop("Invalid 'iso' parameter: must be a valid ISO 3166-1 alpha-3 country code.")
      }
    }
    message(paste0("The city of ", name, " is considered to be part of ", countryOriginalName, " (", iso3, ")."))
    message("If this is incorrect, please run the initiate_project again, setting up the 'iso' parameter with the correct ISO 3166-1 alpha-3 country code.")
  }
  return(TRUE)
}
