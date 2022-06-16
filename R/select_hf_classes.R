#' Health facility attribute class selection (HeRAMS data)
#' 
#' Internal function used for an interactive class selection for a specific attribute
#' @param categories character; available values
#' @param instructions character; message to be displayed for the selection process
#' @export
select_hf_classes <- function (categories, instructions) {
  nCat <- 1:length(categories)
  indCat <- paste(paste0("\n", nCat, ": ", categories))
  cat(indCat)
  cat(paste0("\n\n", instructions, "\nOn the same line separated by a space, or just skip to select all options.\n"))
  k <- 0
  while (k < 3) {
    selInd <- readline(prompt = "Selection: ")
    selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
    if (length(selInd) == 0) {
      return(NULL)
    } else if (all(selInd %in% nCat)) {
      return(selInd)
    } else {
      message("\nInvalid selection!")
      valid <- FALSE
      k <- k + 1
    }
  }
  stop("Invalid selection and too many attempts!")
}

