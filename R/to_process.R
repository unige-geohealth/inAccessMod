#' Decision On Input Processing
#'
#' Internal function that is used to check if an input has already been processed and if so, to ask the user
#' if they want to process it again.
#' @param path character; path of an input folder
#' @param alwaysProcess logical; should the raw input always be processed, even if it has already been 
#' processed? If FALSE and if the raw input has already been processed the user is 
#' interactively asked whether they want to process it again or not.
#' @export
to_process <- function (path, alwaysProcess) {
  prFiles <- list.files(gsub("raw", "processed", path), recursive = TRUE, pattern = "*.tif|*.shp")
  if (length(prFiles) > 0) {
    if (!alwaysProcess) {
      inputName <- gsub("/raw$", "", gsub("^.*/data/", "", path))
      yn <- utils::menu(c("YES", "NO"), title = cat(paste("\n", inputName, "was already processed. Would you like to reprocess it?")))
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