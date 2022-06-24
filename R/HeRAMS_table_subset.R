#' HeRAMS table subset
#' 
#' Internal function used for subsetting the HeRAMS table and track record.
#' @param tibT \code{tibble} object; HeRAMS table with labels.
#' @param tibC \code{tibble} object; HeRAMS table with codes.
#' @param varCol character; column name for filtering.
#' @param impairValues character; values that indicate that there is an impairment (e.g., service partially available). 
#' Has to be compatible with regular expression (regex).
#' @param stopQuest logical; is it possible that the questionnaire is stopped at the specified column ?
#' @param codeName character; name of the column as it its indicated in the column code and stop lists.
#' @param stopLst list; list with the values for different attributes that stops the questionnaire. 
#' @param tempDir character; temporary folder.
#' @return a list of two \code{tibble} objects; The updated tibbles, the one with labels and the other one with codes.
#' @export
HeRAMS_table_subset <- function (tibT, tibC, varCol, stopQuest = TRUE, codeName = NULL, stopLst = NULL, tempDir, barriers, suffix, impairmentValues) {
  categories <- unique(tibT[, varCol, drop = TRUE])
  categories[!complete.cases(categories)] <- "Empty response"
  selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
  if (is.null(selInd)) {
    categories <- categories
  } else {
    categories <- categories[selInd]
  }
  # If selInd is equal to the length of categories + 1
  if (!all(complete.cases(categories))) {
    unlink(tempDir, recursive = TRUE)
    stop_quietly("You canceled the filtering process.")
  }
  categories[categories == "Empty response"] <- NA
  tibC <- tibC[tibT[, varCol, drop = TRUE] %in% categories, ]
  tibT <- tibT[tibT[, varCol, drop = TRUE] %in% categories, ]
  write(paste0(varCol, " -> ", paste(categories, collapse = " + ")), file = paste(tempDir, "selected_hf.txt", sep = "/"), append = TRUE)
  # Check if stop
  if (stopQuest) {
    if (codeName %in% names(stopLst)) {
      if (all(tibC[tibT[, varCol, drop = TRUE] %in% categories, varCol, drop = TRUE] == stopLst[[codeName]])) {
        cat("\nThe filtering process has be stopped as the questionnaire was stopped at this stage for all the selected facilities.\n")
        return(list(tibT, tibC, TRUE))
      }
    }
  }
  # Check if they are possible barriers
  if (barriers) {
    colBarriers <- colnames(tibT)[grep(paste0(varCol, suffix), colnames(tibT))]
    if (length(colBarriers) == 0) {
      return(list(tibT, tibC))
    } else {
      impair <- grepl(impairmentValues, tibC[, varCol, drop = TRUE])
      if (any(impair) & barriers){
        cat("\nCauses/barriers for impairment:\n")
        # Get possible responses
        resps <- NULL
        for (j in 1:length(colBarriers)) {
          resp <- tibT[, colBarriers[j], drop = TRUE]
          resp <- resp[complete.cases(resp)]
          resps <- c(resps, resp)
        }
        categories <- unique(resps)
        selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
        if (is.null(selInd)) {
          categories <- categories
        } else {
          categories <- categories[selInd]
        }
        # As there are different columns that can contain the value
        condMat1 <- matrix(NA, nrow = nrow(tibT), ncol = length(colBarriers))
        for (j in 1:length(colBarriers)) {
          condMat2 <- matrix(NA, nrow = nrow(tibT), ncol = length(categories))
          for (k in 1:length(categories)) {
            condMat2[, k] <- categories[k] == tibT[, colBarriers[j], drop = TRUE]
          }
          condMat1[, j] <- apply(condMat2, 1, any)
        }
        tibC <- tibC[apply(condMat1, 1, any, na.rm = TRUE), ]
        tibT <- tibT[apply(condMat1, 1, any, na.rm = TRUE), ]
        write(paste0(varCol, substr(suffix, 1, 1), " -> ", paste(categories, collapse = " + ")), file = paste(tempDir, "selected_hf.txt", sep = "/"), append = TRUE)
      }
    }
  }
  return(list(tibT, tibC))
}
