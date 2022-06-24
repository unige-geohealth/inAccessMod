#' Set HeRAMS table parameters
#'
#' Internal function to modify (if needed) the default values for column codes
#' @param tableParam list; internal data with default values
#' @export
set_HeRAMS_table_parameters <- function (tableParam) {
  message("Variables and column names (compatible with regular expression)")
  for (i in 1:length(tableParam)){
    cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
  }
  
  yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any value ?")
  if (yn == 1) {
    for (i in 1:length(tableParam)) {
      msg <- "value (compatible with regular expression)"
      cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
      cat(paste("\nType the new", msg, "or just ENTER to keep the default value."))
      newCode <- readline(prompt = "New value: ")
      if (nchar(newCode) > 0) {
        tableParam[[i]] <- newCode
      }
    }
  }
  return(tableParam)
}