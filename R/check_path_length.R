#' Check path length
#'
#' Check path length to avoid Windows error
#' @param path character
check_path_length <- function (path) {
  if (Sys.info()["sysname"] == "Windows") {
    ncharact <- nchar(path)
    if(ncharact > 247) {
      print(path)
      stop_quietly("Path length limit exceeded !")
    }
  }
}
