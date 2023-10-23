#' Copy Input
#'
#' Copy a manually downloaded input to the project corresponding directory before being processed. To avoid possible further conflicts, 
#' the input has to be a "raw" input (i.e. not projected).
#' @param mainPath character; the parent directory of the country folder
#' @param country character; the country folder name
#' @param input character; the absolute path of the raw input. Can be a single path (corresponding to e.g. a health facility table) 
#' or a vector of different paths (corresponding to e.g. shapefile documents). Can also be the path of a folder. In this case, the function
#' will copy all the files that are inside this folder.
#' @examples
#' # Replace workDir with the actual path to your working directory
#' \dontrun{
#' mainPath <- "workDir"
#' intiate_project(mainPath, country)}
#' 
#' # Replace myCountry with the country name you are working on (workDir subfolder)
#' # Replace myInputPath with the actual path to the file to be copied or to the directory
#' # that contains the files to be copied (e.g., shapfile documents)
#' \dontrun{
#' country <- "myCountry"
#' inputPath <- "myInputPath"
#' copy_input(mainPath, country, inputPath)}
#' @details The user is interactively asked to select the 'input' folder destination.
#' @export
copy_input <- function (mainPath, country, input) {
  if (length(input) == 1) {
    if (!is.character(input)) {
      stop("input must be 'character'")
    }
    if (dir.exists(input)) {
      dirInput <- TRUE
    } else {
      if (!file.exists(input)) {
        stop("The input argument is neither an existing directory nor an existing file.")
      } else {
        dirInput <- FALSE
      }
    }
  } else {
    for (i in 1:length(input)) {
      if (!is.character(input[i])) {
        stop("input must be 'character'")
      }
      if (dir.exists(input[i])) {
        stop("Multiple inputs including a folder path. Required: Either multiple files or a single directory.")
      }
      if (!file.exists(input[i])) {
        stop(paste(input[i], "does not exist."))
      }
    }
    dirInput <- FALSE
  }
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  path <- paste0(mainPath, "/", country, "/data")
  if (!dir.exists(paste0(mainPath, "/", country, "/data"))) {
    stop(paste(path, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  folders <- gsub("^.*/data/", "", list.dirs(path, recursive = FALSE))
  folders <- folders[!grepl("zToAccessMod", folders)]
  fold <- utils::menu(folders, title = "Which data would you like to copy ?")
  folder <- list.dirs(path, recursive = FALSE)[grepl(folders[fold], list.dirs(path, recursive = FALSE))]
  timeFolder <- format(Sys.time(), "%Y%m%d%H%M%S")
  folder <- file.path(folder, timeFolder, "raw")
  dir.create(folder, recursive = TRUE)
  if (!dirInput) {
    for (i in 1:length(input)) {
      file.copy(input[i], folder, overwrite = TRUE, copy.date = TRUE)
      cat(paste0("\n",input[i], " copied to: ", folder))
    }
  } else {
    files <- list.files(input, full.names = TRUE, recursive = FALSE)
    for (i in 1:length(files)) {
      file.copy(files[i], folder, overwrite = TRUE, copy.date = TRUE)
      cat(paste0("\n",files[i], " copied to: ", folder))
    }
  }
}