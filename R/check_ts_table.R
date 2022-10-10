#' Check travel scenario table
#'
#' Check the format and content of the travel scenario table (internal function).
#' @param xlsi \code{tibble}; travel scenario table
#' @param xlsiFile character; travel scenario file path
#' @param vLc numeric vector; unique values of the merged landcover raster
#' @keywords internal
#' @export
check_ts_table <- function (xlsi, xlsiFile, vLc) {
  xlsi <- xlsi[complete.cases(xlsi), ]
  if (!all(colnames(xlsi) == c("class", "label", "speed", "mode"))) {
    stop(paste0(xlsiFile, ": column names must be 'class', 'label', 'speed' and 'mode'"))
  }
  vLci <- xlsi[, "class", drop = TRUE]
  if (!all(vLc %in% vLci)) {
    missVLc <- vLc[!vLc %in% vLci]
    missVLc <- paste(missVLc, collapse = ", ")
    stop(paste0(xlsiFile, ": Missing information for landcover value(s) ", missVLc))
  }
  vLcDupl <- vLci[duplicated(vLci)]
  if (length(vLcDupl) > 0) {
    stop(paste0(xlsiFile, ": Duplicated landcover class: ", vLcDupl))
  }
  vSpeed <- xlsi[, "speed", drop = TRUE]
  if (!is.numeric(vSpeed)) {
    stop(paste0(xlsiFile, ": Speed column has to be numeric."))
  }
  if (any(vSpeed < 0)) {
    stop(paste0(xlsiFile, ": Speed can only be positive or equal to zero."))
  }
  vMode <- xlsi[, "mode", drop = TRUE]
  if (!all(vMode %in% c("WALKING", "MOTORIZED", "BICYCLING"))) {
    stop(paste0(xlsiFile, ": mode can only be 'WALKING' 'MOTORIZED' or 'BICYCLING'."))
  }
  return(0)
}
