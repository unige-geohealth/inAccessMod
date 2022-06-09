label_landcover <- function(mainPath, country, overwrite = FALSE) {
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
  
  landcoverTable <- paste0(landcoverFolder, "/labelLandcover.csv")
  if (file.exists(landcoverTable)) {
    if (!overwrite) {
      stop("\nLandcover label table already exists. Set overwrite = TRUE to create it again.")
    }
  }
  
  val <- terra::values(landcover)[,1]
  val <- unique(val[!is.na(val)])
  val <- val[order(val)]
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
  write.csv(labelDf, file = landcoverTable, row.names =  FALSE)
}

label_landcover(mainPath, country, overwrite = T)
