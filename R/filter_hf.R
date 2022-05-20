#' Filter health facilities
#'
#' Filter the HeRAMS health facility raw table based on a set of variables and export a table that contains only 
#' the selected facilities. The selection is recorded within a selected_hf.txt file stored in a sub-project folder. Different
#' selections will create new sub-project folders. In the same sub-project folder different 'raw' sub-folders may be created
#' depending on the original Excel document modification time.
#' @param mainPath character; the parent directory of the country folder
#' @param region character; the country folder name
#' @param pathTable character; path to the HeRAMS Excel Table
#' @export
filter_hf <- function (mainPath, region, pathTable) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(region)) {
    stop("region must be 'character'")
  }
  if (!is.character(pathTable)) {
    stop("pathTable must be 'character'")
  } else {
    if (!file.exists(pathTable)) {
      stop("pathTable does not exists!")
    }
  }
  pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
  if (!dir.exists(paste0(pathFacilities))) {
    stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
  }
  newTib <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 2)}, error = function(e){NULL})
  if (is.null(newTib)) {
    stop(paste(paste(pathFacilities, file, sep = "/"), "could not be opened."))
  }
  logTxt <- paste0(mainPath, "/", region, "/data/log.txt")
  ctime <- file.info(pathTable)$mtime
  variables <- c(health_facility_types = "MoSD3", 
                 facility_ownership = "MoSD7", 
                 functionality_status = "HFFUNCT", 
                 facility_status = "MoSD4",
                 accessibility_status = "HFACC"
  )
  tempDir <- paste0(pathFacilities, "/temp")
  dir.create(tempDir)
  for (i in 1:length(variables)){
    backupTib <- newTib
    newTib <- tibble_subset(newTib, variables[i], tempDir)
    if (is.null(newTib)) {
      message("\nInvalid index ! All options were kept.")
      newTib <- backupTib
      next
    }
  }
  lines1 <- readLines(paste(tempDir, "selected_hf.txt", sep = "/"))
  fileLst <- list.files(pathFacilities, recursive = TRUE)
  logTxtLst <- fileLst[grepl("selected_hf\\.txt$", fileLst)]
  logTxtLst <- logTxtLst[!grepl("temp/", logTxtLst)]
  subProjDir <- NULL
  if (length(logTxtLst) > 0) {
    for (i in 1:length(logTxtLst)) {
      lines2 <- readLines(paste(pathFacilities, logTxtLst[i], sep = "/"))
      if (all(lines1 == lines2)){
        subProjDir <- gsub("/selected_hf\\.txt$", "", logTxtLst[i])
        message(paste("Existing sub-project:", subProjDir))
        break
      }
    }
  }
  if (is.null(subProjDir)) {
    subProjDir <- list.dirs(pathFacilities, recursive = FALSE)
    subProjDir <- grepl("^subProj[0-9]{3}", subProjDir)
    if (length(as.character(length(subProjDir) + 1)) == 1) {
      subProjDir <- paste0("subProj00", length(subProjDir) + 1)
    } else if (length(as.character(length(subProjDir) + 1)) == 2) {
      subProjDir <- paste0("subProj0", length(subProjDir) + 1)
    } else {
      subProjDir <- paste0("subProj", length(subProjDir) + 1)
    }
    message(paste("\nNew sub-project:", subProjDir))
    dir.create(paste(pathFacilities, subProjDir, sep = "/"))
    file.copy(paste(tempDir, "selected_hf.txt", sep = "/"), paste(pathFacilities, subProjDir, sep = "/"))
  }
  unlink(tempDir, recursive = TRUE)
  outTimeFolder <- gsub("-|[[:space:]]|\\:", "", ctime)
  outFolder <- paste(pathFacilities, subProjDir, outTimeFolder, "raw", sep = "/")
  dir.create(outFolder, recursive = TRUE)
  if (file.exists(paste(outFolder, "health_facilities.csv", sep = "/"))){
    warning(paste(paste(outFolder, "health_facilities.csv", sep = "/"), "\nhas been overwritten."))
  }
  write.csv(newTib, file = paste(outFolder, "health_facilities.csv", sep = "/"))
  write(paste0(Sys.time(), ": Health facilities where filtered - sub-project folder: ", subProjDir, " - input folder: ", outTimeFolder), file = logTxt, append = TRUE)
  cat(paste0("\n", outFolder, "/health_facilities.csv\n"))
}
