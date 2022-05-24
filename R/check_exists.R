#' Check Existing Inputs
#'
#' Internal function that is used to check if an input already exists, and if so, to retrieve the time of 
#' the folder creation.
#' @param path character; path of the country folder
#' @param type character; 'raw' or 'processed'
#' @param layer logical; is the input a spatial layer (e.g. raster or shapefile)?
#' @param extension character; if \code{layer} = FALSE, the extension of the input (e.g. 'xlsx')
#' @return character; time of the input folder creation
#' @export
check_exists <- function (path, type, layer = TRUE, extension = NULL) {
  if (!is.character(path)) {
    stop("path must be 'character'")
  }
  if (!dir.exists(path)) {
    stop(paste(path,"does not exist. Run the initiate_project function first or check the input parameters."))
  }
  if (!type %in% c("raw", "processed")) {
    stop("type must be 'raw' or 'processed")
  }
  if (!is.logical(layer)) {
    stop("layer must be 'logical'")
  }
  if (!layer) {
    if (is.null(extension)) {
      stop("extension is required when 'layer' = FALSE")
    }
    if (!is.character(extension)) {
      stop("extension must be 'character'")
    }
  }
  fileLst <- list.files(path, full.names = FALSE, recursive = TRUE)
  if (type == "raw"){
    if (layer) {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.tif|/", type, "/.*\\.shp"), fileLst)], 1, 14)
    } else {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.", extension), fileLst)], 1, 14)
    }
  } else {
    if (layer) {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.tif|/", type, "/.*\\.shp"), fileLst)], 1 + 25, 14 + 25)
    } else {
      folderLst <- substr(fileLst[grepl(paste0("/", type, "/.*\\.", extension), fileLst)], 1 + 25, 14 + 25)
    }
  }
  if (length(folderLst) != 0) {
    # Only keep sys.time folders
    folderLst <- folderLst[grepl("^[0-9]{14}$", folderLst)]
    folders <- paste0(substr(folderLst, 1, 4), "-", substr(folderLst, 5, 6), "-", substr(folderLst, 7, 8), " ", substr(folderLst, 9, 10), ":", substr(folderLst, 11, 12), ":", substr(folderLst, 13, 14), " CEST")
    return(folders)
  } else {
    return(NULL)
  }
}
