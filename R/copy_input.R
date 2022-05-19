#' Copy input
#'
#' Copy a manually downloaded input to the project corresponding directory before being processed.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param input character; the absolute path of the input. Can be a single path (corresponding to e.g. a health facility table) 
#' or a vector of different paths (corresponding to e.g. shapefile documents)
#' @details The user is interactively asked to select the 'input' folder destination.
#' @export
copy_input <- function (mainPath, region, input) {
  for (i in 1:length(input)) {
    if (!is.character(input[i])) {
      stop("input must be 'character'")
    } else {
      if (!file.exists(input[i])) {
        stop(paste(input[i], "does not exist."))
      }
    }
  }
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  path <- paste0(mainPath, "/", region, "/data")
  if (!dir.exists(paste0(mainPath, "/", region, "/data"))) {
    stop(paste(path, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  folders <- gsub("^.*/data/", "", list.dirs(path, recursive = FALSE))
  folders <- folders[!grepl("zToAccessMod", folders)]
  fold <- utils::menu(folders, title = "Which data would you like to load?")
  folder <- list.dirs(path, recursive = FALSE)[grepl(folders[fold], list.dirs(path, recursive = FALSE))]
  sysTime <- Sys.time()
  timeFolder <- gsub("-|[[:space:]]|\\:", "", sysTime)
  dir.create(paste0(folder, "/", timeFolder, "/raw"), recursive = TRUE)
  folder <- paste0(folder, "/", timeFolder, "/raw")
  for (i in 1:length(input)) {
    file.copy(input[i], folder, overwrite = TRUE, copy.date = TRUE)
    cat(paste0("\n",input[i], " copied to: ", folder))
  }
}