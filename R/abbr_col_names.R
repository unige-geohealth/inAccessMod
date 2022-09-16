#' Abbreviate HeRAMS Attribute Table Column Names (internal)
#'
#' To avoid truncated names and warnings when writing shapefiles
#' @param tib \code{tibble}; futur attribute table
#' @export
abbr_col_names <- function (tib) {
  colnames(tib) <- gsub("SQ", "", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("CONTACT", "CONT", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("HF", "", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("SUP", "S", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("QHeRAMS", "QH", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("external", "extern", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("synced", "sync", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("workspace", "worksp", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  colnames(tib) <- gsub("MoSDGPS", "GPS", colnames(tib))
  # colnames(tib)[nchar(colnames(tib)) > 10]
  return(tib)
}

# If changes, check create_hf_shapefile:
# print(dfNA[, c("extern_id", "worksp_id", "date", "MoSD3", "NAME")])
# print(df[!inter, c("extern_id", "worksp_id", "date", "MoSD3", "NAME")])
