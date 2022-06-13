#' Select By Attribute
#'
#' Internal function used to select the OSM shapefiles by attribute
#' @param sfObject \code{sf} object
#' @param columnName character; the column name corresponding to the attribute used for filtering the shapefile
#' @return A list of length 2; the first element is a \code{sf} object and the second element is the selected attribute values
#' @export
select_categories <- function (sfObject, columnName, defaultClasses, classes) {
  sfDataFrame <- sfObject
  sf::st_geometry(sfDataFrame) <- NULL
  categories <- unique(sfDataFrame[, columnName])
  nCat <- 1:length(categories)
  indCat <- paste(paste0("\n", nCat, ": ", categories))
  cat(indCat)
  if (defaultClasses) {
    categ <- classes
    sfObject <- subset(sfObject,eval(parse(text=columnName)) %in% categ)
    
  } else {
    cat("\n\nEnter all the indices that correspond to categories you want to keep.\nOn the same line separated by a space, or just skip to select all categories.\n")
    selInd <- readline(prompt = "Selection: ")
    selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
    if (length(selInd) != 0) {
      if (0 %in% selInd) {
        message("\nNot valid index: 0; all categories will be kept.")
        categ <- categories
      } else {
        categ <- categories[selInd]
        sfObject <- subset(sfObject,eval(parse(text=columnName)) %in% categ)
      }
    } else {
      categ <- categories
    }
  }
  return(list(sfObject, categ))
}
