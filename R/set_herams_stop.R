#' Set HeRAMS stop responses
#'
#' Internal function to modify (if needed) the default values for the responses that stop the questionnaire
#' @param stopLst list; internal data with default values
#' @keywords internal
#' @export
set_HeRAMS_stop <- function (stopLst) {
  message("\nStop response code (response that stops the questionnaire)")
  for (i in 1:length(stopLst)){
    cat(paste0("\n", gsub("_", " ", names(stopLst)[i]), ": ", stopLst[[i]]))
  }
  yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any stop response code (response that stops the questionnaire) ?")
  if (yn == 1) {
    cat(paste0("\n", gsub("_", " ", names(stopLst)[i], ": ", stopLst[[i]])))
    cat(paste("\nType the new code or just ENTER to keep the default value."))
    newCode <- readline(prompt = "New value: ")
    if (nchar(newCode) > 0) {
      stopLst[[i]] <- newCode
    }
  }
  return(stopLst)
}
