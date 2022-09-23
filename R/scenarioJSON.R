#' Create JSON files for multiple travel scenarios
#'
#' From multiple travel scenarios table, the function creates a JSON file that can be used when running an AccessMod replay by command line.
#' @param inputFolder character; the path to the input folder, which must contains an administrative unit shapefile, a merged 
#' landcover imported from AccessMod and an Excel or CSV table for each travel scenario.
#' @param landcoverFile character; the file name of the original merged landcover (with extension).
#' @param outputFolder character; the output folder where the JSON file will be written.
#' @param fileName character; the name of the output JSON file (without extension).
#' @details A JSON file is created in the output folder.
#' @export

scenarioJSON <- function (inputFolder, landcoverFile, outputFolder, fileName) {
  if (!is.character(inputFolder)) {
    stop("inputFolder must be 'character'")
  }
  if (!dir.exists(inputFolder)) {
    stop(paste(inputFolder, "does not exist"))
  }
  if (!is.character(landcoverFile)) {
    stop("landcoverFile must be 'character'")
  }
  if (!is.character(outputFolder)) {
    stop("outputFolder must be 'character'")
  }
  if (!dir.exists(outputFolder)) {
    stop(paste(outputFolder, "does not exist"))
  }
  landcover <- terra::rast(paste(inputFolder, landcoverFile, sep = "/"))
  vLc <- terra::values(landcover)[, 1]
  vLc <- unique(vLc[!is.na(vLc)])
  vLc <- vLc[order(vLc)]
  xls <- list.files(inputFolder, pattern = "\\.xls|\\.csv", full.names = TRUE)
  xls <- xls[!grepl("\\~\\$", xls)]
  if (any(duplicated(gsub("\\.xls|\\.xlsx|\\.csv", "", xls)))) {
    stop("Duplicated names for travel scenario tables.")
  }
  if (length(xls) == 0) {
    stop("No Excel file (travel scenario) in the input folder.")
  }
  xlsLst <- vector("list", length = length(xls))
  for (i in 1:length(xls)) {
    if (grepl("\\.xlsx$", xls[i])) {
      xlsi <- readxl::read_xlsx(xls[i])
    } else if (grepl("\\.xls$", xls[i])){
      xlsi <- readxl::read_xls(xls[i])
    } else {
      xlsi <- tibble::as_tibble(read.csv(xls[i], header = TRUE))
    }
    xlsiFile <- xls[i]
    check_ts_table(xlsi, xlsiFile, vLc)
    xlsLst[[i]] <- as.data.frame(xlsi[order(xlsi$class), c("class", "speed", "mode")])
  }
  xlsNames <- list.files(inputFolder, pattern = "\\.xls|\\.xlsx|\\.csv", full.names = FALSE)
  xlsNames <- gsub("\\.xls|\\.xlsx|\\.csv", "", xlsNames)
  names(xlsLst) <- xlsNames
  
  scenarios <- list()
  for (i in 1:length(xlsLst)) {
    scenarios$scenarios[[names(xlsLst)[i]]] <- xlsLst[[i]]
  }
  data <- toJSON(scenarios)
  write(data, file = paste0(outputFolder, "/", fileName, ".json"))
  message(paste("JSON file created:", paste0(outputFolder, "/", fileName, ".json")))
}


