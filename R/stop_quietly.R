#' Stop quietly
#'
#' Internal function that allows to stop a function with a non-error message.
#' @param msg Character string
#' @examples
#' stop_quietly("This is a non-error message");
#' @export
stop_quietly <- function(msg) {
  message(msg)
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop(msg)
}
