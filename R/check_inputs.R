#' Check Inputs
#'
#' Check whether a raw or processed input exists
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param type character; 'raw' or 'processed' depending on whether the required input is
#' the raw input or the already processed one.
#' @param onlyPrint logical; should the function just print which inputs are available and which are not available. Internally
#' \code{onlyPrint} is set FALSE, but when the function is used directly by the user, print should be set to TRUE.
#' @return character vector when \code{onlyPrint} = FALSE, and NULL when \code{onlyPrint} = TRUE
##' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' initiate_project(mainPath, country)}
#' 
#' # Replace myCountry with the country name you are working on (workDir subfolder)
#' \dontrun{
#' country <- "myCountry"
#' check_inputs(mainPath, country, type = "raw")}
#' @keywords internal
#' @export
check_inputs <- function (mainPath, country, type, onlyPrint = TRUE) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  if (!dir.exists(paste(mainPath, country, sep = "/"))) {
    stop(paste(paste(mainPath, country, sep = "/"), "does not exsit. Run the initiate_project first or check the path."))
  }
  if (!type %in% c("raw","processed")) {
    stop("type must be 'raw' or 'processed'")
  }
  if (!is.logical(onlyPrint)) {
    stop("print must be 'logical'")
  }
  fileLst <- list.files(paste0(mainPath, "/", country, "/data"), recursive = TRUE)
  fileLst <- fileLst[!grepl("zToAccessMod", fileLst)]
  fileAv <- fileLst[grepl(paste0(type, "/.*(\\.tif|\\.shp|\\.img)"), fileLst)]
  folderAv <- unique(gsub("/[0-9].*$", "", fileAv))
  folderLst <- list.dirs(paste0(mainPath, "/", country, "/data"), recursive = TRUE)[-1]
  folderLst <- folderLst[!grepl("zToAccessMod", folderLst)]
  folderLst <- unique(gsub("/[0-9].*$", "", folderLst))
  folderLst <- gsub("^.*/data/", "", folderLst)
  folderNAv <- folderLst[!folderLst %in% folderAv]
  
  # Not necessary anymore and it prevented to process vFacilities with no particular scenario
  # With folderLst <- folderLst[grepl("^[0-9]{14}$", folderLst)] in check_exists, it should be always ok
  # folderAv <- folderAv[!grepl("^vFacilities$", folderAv)]
  # folderNAv <- folderNAv[!grepl("^vFacilities$", folderNAv)]
  
  # Remove from unvailable if we have some available facility shapefile
  if (any(grepl("vFacilities", folderAv))) {
    folderNAv <- folderNAv[!grepl("vFacilities", folderNAv)]
  }

  if (length(folderAv) > 0) {
    if (length(folderAv) == 1) {
      message(paste("\nThe following", type, "input is AVAILABLE:"))
      cat(folderAv)
      cat("\n")
    } else {
      message(paste("\nThe following", type, "inputs are AVAILABLE:"))
      cat(paste(folderAv, collapse = ", "))
      cat("\n")
    }
  }
  if (length(folderNAv) > 0) {
    if (length(folderNAv) == 1) {
      message(paste("\nThe following", type, "input is UNAVAILABLE:"))
      cat(folderNAv)
      cat("\n")
    } else {
      message(paste("\nThe following", type, "inputs are UNAVAILABLE:"))
      cat(paste(folderNAv, collapse = ", "))
      cat("\n")
    }
  }
  if (!onlyPrint) {
    return(folderAv)
  }
}
