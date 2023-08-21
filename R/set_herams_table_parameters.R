#' Set HeRAMS table parameters
#'
#' Internal function to modify (if needed) the default values for column codes
#' @param tableParam list; internal data with default values
#' @keywords internal
#' @export
set_HeRAMS_table_parameters <- function (tableParam, regex = TRUE) {
  if (regex) {
    message("Variables and column names (compatible with regular expression)")
  } else {
    message("Variables and column names")
  }
  for (i in 1:length(tableParam)){
    cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
  }
  yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any value ?")
  if (yn == 1) {
    for (i in 1:length(tableParam)) {
      if (regex) {
        msg <- "value (compatible with regular expression)"
      } else {
        msg <- "value"
      }
      cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
      cat(paste("\nType the new", msg, "or just ENTER to keep the default value."))
      newCode <- readline(prompt = "New value: ")
      if (nchar(newCode) > 0) {
        tableParam[[i]] <- newCode
      }
    }
  }
  ynN <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to add another field used for filtering ?")
  while (ynN == 1) {
    cat(paste("\nType the column code name"))
    newCode <- readline(prompt = "Field: ")
    tableParam[[newCode]] <- newCode
    ynN <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to add another field used for filtering ?")
  }
  
  return(tableParam)
}