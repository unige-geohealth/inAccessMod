#' Label landcover layer
#' 
#' Create a CSV table with landcover codes and labels that can be imported into AccessMod.
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param mostRecent logical; should the most recent input be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on file creation time.
#' @param overwrite logical; in case it exists, should the output be overwritten?
#' @param defaultLabels logical; should the Copernicus default labels be associated with the the landcover raster values.
#' If FALSE, the user is interactively asked to enter the label names for each value.
#' @details The function writes a CSV table with two columns (classes and labels) into the same folder as the processed landcover layer.
#' @export
label_landcover <- function(mainPath, country, mostRecent, overwrite = FALSE, defaultLabels = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!is.logical(mostRecent)){
    stop("mostRecent must be 'logical'")
  }
  
  # Check directory
  pathLandcover <- paste0(mainPath, "/", country, "/data/rLandcover")
  folders <- check_exists(pathLandcover, "processed", layer = TRUE)
  if (is.null(folders)) {
    stop("\nProcessed land cover raster is missing!")
  }
  timeFolder <- select_input(folders, "Landcover processed at", mostRecent)
  if (is.null(timeFolder)) {
    stop_quietly("You exit the function.")
  }
  folderLst <- list.dirs(pathLandcover)
  landcoverFolder <-   folderLst[grepl(paste0("processed/", timeFolder), folderLst)]
  landcover <- load_layer(landcoverFolder, "")[[1]]
  landcoverTable <- paste0(landcoverFolder, "/labelLandcover.csv")
  if (file.exists(landcoverTable)) {
    if (!overwrite) {
      stop("\nLandcover label table already exists. Set overwrite = TRUE to create it again.")
    }
  }
  landcoverLabels <- data.frame(class = c(0, 111, 113, 112, 114, 115, 116, 121, 123, 122, 124, 125, 126, 20, 30, 90, 100, 60, 40, 50, 70, 80, 200), 
                                label = c("NA", 
                                           "Closed forest, evergreen needle leaf", 
                                           "Closed forest, deciduous needle leaf", 
                                           "Closed forest, evergreen, broad leaf",
                                           "Closed forest, deciduous broad leaf",
                                           "Closed forest, mixed",
                                           "Closed forest, unknown",
                                           "Open forest, evergreen needle leaf",
                                           "Open forest, deciduous needle leaf",
                                           "Open forest, evergreen broad leaf",
                                           "Open forest, deciduous broad leaf",
                                           "Open forest, mixed",
                                           "Open forest, unknown",
                                           "Shrubs",
                                           "Herbaceous vegetation",
                                           "Herbaceous wetland",
                                           "Moss and lichen",
                                           "Bare / sparse vegetation",
                                           "Cultivated and managed vegetation/agriculture",
                                           "Urban / built up",
                                           "Snow and Ice",
                                           "Permanent water bodies",
                                           "Open sea"))
  
  val <- terra::values(landcover)[,1]
  val <- unique(val[!is.na(val)])
  val <- val[order(val)]
  
  if (defaultLabels) {
    labelDf <- landcoverLabels[landcoverLabels$class %in% val, ]
    labelDf <- labelDf[order(labelDf$class), ]  
  } else {
    lcLabels <- vector(mode = "character", length = length(val))
    for (i in 1:length(val)) {
      validLab <- FALSE
      k <- 0
      while (!validLab & k < 3) {
        lab <- readline(prompt = paste("Enter the label corresponding to", val[i], ": "))
        lab <- gsub(" \\(.*\\)|\\(.*\\)", "", lab)
        lab <- gsub("[^[[:alnum:]]", " ", lab)
        lab <- stringr::str_squish(lab)
        lab <- gsub("[[:space:]]", "_", lab)
        lab <- stringi::stri_trans_general(str = lab, id = "Latin-ASCII")
        if (nchar(lab) == 0) {
          message("\nInvalid value!")
        } else if (lab %in% lcLabels) {
          message("\nDuplicated values are not allowed.")
        } else {
          validLab <- TRUE
        }
      }
      if (k > 3) {
        stop("\nInvalid label (too many attempts)!")
      }
      lcLabels[i] <- lab
    }
    labelDf <- data.frame(class = val, label = lcLabels)
  }
  write.csv(labelDf, file = landcoverTable, row.names =  FALSE)
}
