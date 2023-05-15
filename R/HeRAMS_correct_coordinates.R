#' Correct Coordinates (HeRAMS filtered table)
#'
#' Correct or complete coordinates in the herams_facilities.csv based on a second CSV table that contains correct coordinates
#' @param heramsCSV character; path to the 'herams_facilities.csv' file (to be modified)
#' @param corrCSV character; path to the table that contains correct coordinates
#' @param gpsX character; vector of length 2 corresponding to the GPS column names in heramsCSV
#' @param gpsY character; vector of length 2 corresponding to the GPS column names in corrCSV
#' @param byX character; column name in heramsCSV used for matching rows
#' @param byY character; column name in CorrCSV used for matching rows
#' @export
HeRAMS_correct_coordinates <- function (heramsCSV, corrCSV, gpsX, gpsY, byX, byY) {
  if (!is.character(heramsCSV)) {
    stop("heramsCSV must be 'character'")
  }
  if (!is.character(corrCSV)) {
    stop("corrCSV must be 'character'")
  }
  if (!is.character(gpsX)) {
    stop("gpsX must be 'character'")
  } else {
    if (!length(gpsX) == 2) {
      stop("gpsX length must be 2")
    }
  }
  if (!is.character(gpsY)) {
    stop("gpsY must be 'character'")
  } else {
    if (!length(gpsY) == 2) {
      stop("gpsY length must be 2")
    }
  }
  if (!is.character(byX)) {
    stop("byX must be 'character'")
  }
  if (!is.character(byY)) {
    stop("byY must be 'character'")
  }
  
  herams <- read.csv(heramsCSV, header = TRUE)
  if (!all(gpsX %in% colnames(herams))) {
    stop(paste("Not valid GPS columns in", heramsCSV))
  }
  if (!byX %in% colnames(herams)) {
    stop(paste(byX, "is not a valid column in", heramsCSV))
  }

  
  corr <- read.csv(corrCSV, header = TRUE)
  if (!all(gpsY %in% colnames(corr))) {
    stop(paste("Not valid GPS columns in", corrCSV))
  }
  if (!byY %in% colnames(corr)) {
    stop(paste(byY, "is not a valid column in", corrCSV))
  }
  if (length(unique(corr[, byY])) != nrow(corr)) {
    dupl <- corr[, byY][duplicated(corr[, byY])]
    dupls <- NULL
    # Ignore duplicated, no correction
    # stop(paste0("Duplicated values (", byY, ") in ", corrCSV, "\n", paste(dupl, collapse = ", ")))
  } else {
    dupl <- NULL
  }
  noMatches <- 0
  for (i in 1:nrow(corr)) {
    by <- corr[i, byY]
    coords <- corr[i, gpsY]
    indX <- which(herams[, byX] == by)
    if (length(indX) == 0) {
      noMatches <- noMatches + 1
      next
    } else {
      if (by %in% dupl) {
        dupls <- c(dupls, by)
        next
      }
      # print(herams[indX, gpsX])
      # print(coords)
      # stop()
      herams[indX, gpsX] <- coords
    }
  }
  cat(paste0("\nMatches = ", nrow(corr) - noMatches), "/", nrow(corr))
  if (!is.null(dupl)) {
    cat(paste("\nDuplicated entries (were ignored): ", paste(unique(dupls), collapse = ", ")))
  }
  pathOut <- file.path(dirname(heramsCSV), "health_facilities_corr.csv")
  write.csv(herams, pathOut, row.names = FALSE)
  cat(paste0("\nDone: ", file.path(dirname(heramsCSV), "health_facilities_corr.csv"), "\n"))
}
