#' Analysis scenario selection (HeRAMS data)
#'
#' Interactive selection of the health facility attributes to determine the analysis scenario for the accessibility modelling
#' @param hf_attributes list; interal data with all the available attribute values
#' @details Selection is sequential and is based on 1) the status, 2) the operationality and 3) the service availability. For operationality
#' and service availability, selection based on barriers/impairment can be made when impaired facilities are selected.
#' @export
analysis_scenario <- function (hf_attributes) {
  stopFiltering <- FALSE
  # Status
  message("\nHealth facility status")
  categories <- c("Existing", "Closed")
  instructions <- "Enter all the indices that correspond to the values you want to keep."
  selInd <- select_hf_classes(categories, instructions)
  if (is.null(selInd)){
    status <- categories
  } else {
    status <- categories[selInd]
  }
  if (all(status == "Closed")) {
    stopFiltering <- TRUE
  } else {
    message("\nHealth facility operationality")
  }
  # Alphabetical order, so the order of selection doesn't mind (important for further comparisons)
  status <- status[order(status)]
  write(paste0("Status: ", paste(status, collapse = " + ")), file = paste(tempDir, "selected_hf.txt", sep = "/"), append = TRUE)
  
  stopFiltering <- select_hf_main(operationality, stopFiltering)
  
  if (stopFiltering) {
    message("Analysis scenario selection: done!")
    return(NULL)
  }
  
  yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on specific service availability?"))
  if (yn ==2) {
    message("Analysis scenario selection: done!")
    return(NULL)
  } else {
    message("\nMain pillars selection")
    pillarNames <- gsub("_", " ", names(inAccMod::hf_attributes$Services))
    instructions <- "Enter all the indices that correspond to pillars you would like to focus on."
    selInd <- select_hf_classes(pillarNames, instructions)
    if (is.null(selInd)) {
      pillars <- hf_attributes$Services
    } else {
      pillars <- hf_attributes$Services[selInd]
    }
    for (i in 1:length(pillars)) {
      message(paste("\nCategory selection for", gsub("_", " ", names(pillars)[i])))
      categoryNames <- gsub("_", " ", names(pillars[[i]]))
      instructions <- "Enter all the indices that correspond to the pillar categories you would like to focus on."
      selInd <- select_hf_classes(categoryNames, instructions)
      if (is.null(selInd)) {
        pillarCategories <- pillars[[i]]
      } else {
        pillarCategories <- pillars[[i]][selInd]
      }
      for (j in 1:length(pillarCategories)) {
        stopFiltering <- select_hf_main(pillarCategories[[j]], stopFiltering = FALSE)
      }
    }
  }
}
  
  
  
  
  

















