#' Check Downloaded Inputs
#'
#' Internal function that is used to check if an input has already been downloaded and if so, to ask the user
#' if they want to download it again.
#' @param folders character; path of a specific input folder
#' @keywords internal
#' @export
check_downloaded <- function (folders) {
  indFolder <- paste(paste0("\n", folders))    
  cat("\nInput was already created at the following time:\n")
  cat(indFolder)
  yn <- utils::menu(c("YES","NO"), title="\n\nWould you like to download it again?")
  if (yn == 0) {
    stop_quietly("You exit the function.")
  }
  if (yn == 2) {
    stop_quietly("Download canceled")
  }
}