#' Resample Raster Layer
#'
#' Internal function used to resample rasters
#' @param ras1 \code{SpatRaster} object to be resampled
#' @param ras0 \code{SpatRaster} object with the geometry that \code{ras1} should be resampled to
#' @param rasInit \code{SpatRaster} object; original 'raw' raster (before projecting)
#' @param resampMeth character; method used for estimating the new cell values. If NULL, the user is interactively
#' asked to select one of the available methods for \code{terra::resample} function.
#' @return a list of length 2; the first element is the processed \code{SpatRaster} object and the second element is the selected
#' resampling method (for track record).
#' @export
resample_raster <- function (ras1, ras0, rasInit, resampMeth) {
  if (is.null(resampMeth)) {
    resamplingMethod <- c("near", "bilinear", "cubic", "cubicspline", "lanczos", "sum", "min", "q1", "q3", "max", "average", "mode", "rms")
    resm <- utils::menu(resamplingMethod, title = cat(paste("\n\nSelect resampling method for:\n", rasInit %>% terra::sources(),"\nSee terra::resample function help for more details.")))
    if (resm == 0) {
      return(NULL)
    } else {
      resampMeth <- resamplingMethod[resm]
    }
  }
  cat(paste("\nResampling:\n", rasInit %>% terra::sources(),"\n"))
  rasResamp <- terra::resample(ras1, ras0, method = resampMeth)
  return(list(rasResamp, resampMeth))
}