#' Select Input
#'
#' Internal function that is used to select an input when multiple ones are available
#' @param folders character; input folder names, usually obtained with the \code{check_exists} function
#' @param msg character; used to modify the presentation of the different options
#' @param mostRecent logical; should the most recent input be selected? If FALSE and if there are multiple
#' available inputs, the user is interactively asked to select the input based on time.
#' @return character; time of the input folder creation
#' @keywords internal
#' @export
select_input <- function (folders, msg, mostRecent = FALSE) {
  if (length(folders) > 1) {
    if (!mostRecent) {
      selInd <- utils::menu(paste(msg, folders), title = "\nSelect the data you would like to use.")
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