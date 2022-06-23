#' Set HeRAMS impairment values
#'
#' Internal function to modify (if needed) the default values for the codes that indicate that there is an impairment
#' @param stopLst list; internal data with default values
#' @export
set_HeRAMS_impairment_values <- function (impairValues) {
  message("\nValues that indicate that there is an impairment")
  cat(impairValues)
  yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify the values ?")
  if (yn == 1) {
    cat(paste("\nType the new values (have to be compatible with regular expression) or just ENTER to keep the default values."))
    newCode <- readline(prompt = "New values: ")
    if (nchar(newCode) > 0) {
      impairValues <- newCode
    }
  }
  return(impairValues)
}
