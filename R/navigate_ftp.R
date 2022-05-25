#' Navigate Wolrd Pop FTP
#'
#' Internal function used to navigate through the World Pop FTP's folders.
#' @param folderLst character vector; folder names within the main World Pop FTP directory 
#' @param iso character; the country ISO alpha-3 code
#' @param pathFTP character; the path of the main World Pop FTP directory. The variable IS updated during the process.
#' @param pathFTP0 character; the path of the main World Pop FTP directory. The variable IS NOT updated during the process.
#' @return a list of length 2; the first element is a character vector that contains the available files in the targeted folder
#' and the second element is the path of the targeted folder.
#' @details The function keeps running while no folder whose name is the country ISO code is reached.
#' @export
navigate_ftp <- function (folderLst, iso, pathFTP, pathFTP0) {
  navig <- TRUE
  while (navig) {
    # If there is a folder with our country code, select it
    isoProp <- sum(grepl("^[A-Z]{3}$", folderLst)) / length(folderLst)
    if (any(grepl(paste0("^", iso, "$"), folderLst))) {
      pathFTP <- paste0(pathFTP, iso,"/")
      navig <- FALSE
    }else{
      if (isoProp == 1 & !any(grepl(paste0("^", iso, "$"), folderLst))) {
        pathFTP <- paste0(pathFTP,"../")
        message(paste(iso, "is not available in this dataset."))
      }else{
        if (!pathFTP == pathFTP0) {
          folderLst <- c(folderLst, "PREVIOUS DIRECTORY", "EXIT FUNCTION")
        }else{
          folderLst <- c(folderLst, "EXIT FUNCTION")
        }
        folderNb <- utils::menu(folderLst, title="\nSelect folder (type the corresponding number or zero to get back to the root directory)?")
        if (folderNb == length(folderLst)) {
          return(NULL)
        }else if (folderNb == (length(folderLst)-1)) {
          pathFTP <- paste0(pathFTP, "../")
        }else if (folderNb == 0) {
          pathFTP <- pathFTP0
        }else{
          selectedFolder <- folderLst[folderNb]
          pathFTP <- paste0(pathFTP, selectedFolder, "/")
        }
      }
    }
    gc()
    folderLst <- RCurl::getURL(pathFTP, verbose=FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
    folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
    if (length(folderLst) == 0) {
      message("Empty folder!")
      pathFTP <- pathFTP0
      folderLst <- RCurl::getURL(pathFTP, verbose=FALSE, ftp.use.epsv = TRUE, dirlistonly = TRUE)
      folderLst <- unlist(strsplit(x = gsub("\\r\\n", " ", folderLst), split=" "))
    }
  }
  return(list(folderLst, pathFTP))
}
