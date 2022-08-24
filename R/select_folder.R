#' Select Folders
#'
#' Internal function used to subset elements from a character vector that correspond to folder names
#' @param x character vector; folder names
#' @param msg character; message to be print for selection
#' @return character vector
#' @export
select_folder <- function (x, msg) {
  n <- 1:length(x)
  indInput <- paste(paste0("\n", n, ": ", x))
  cat(paste0("\n", msg, "\n"))
  cat(indInput)
  cat("\n\n")
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
