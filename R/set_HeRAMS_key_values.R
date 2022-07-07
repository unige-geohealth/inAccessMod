#' Set HeRAMS key values
#'
#' Internal function to modify (if needed) the default values for the key codes
#' @param values character; default values (regex compatible)
#' @export
set_HeRAMS_key_values <- function (values, msg) {
  message(paste0("\n", msg))
  cat(values)
  yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify the values ?")
  if (yn == 1) {
    cat(paste("\nType the new values (have to be compatible with regular expression) or just ENTER to keep the default values."))
    newCode <- readline(prompt = "New values: ")
    if (nchar(newCode) > 0) {
      values <- newCode
    }
  }
  return(values)
}
