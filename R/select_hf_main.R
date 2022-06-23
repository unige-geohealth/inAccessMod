#' #' High level of the health facility attribute selection process (HeRAMS data)
#' #'
#' #' Get the attributes and the causes for service impairment, run the sub-routine for attribute selection, store the information.
#' #' @param attLst character; list of health facility attributes (sub-list from internal data)
#' #' @export
#' select_hf_main <- function(attLst, stopFiltering) {
#'   for (i in 1:length(attLst)) {
#'     if (!stopFiltering) {
#'       cat(paste0("\n", gsub("_", " ", attLst[[i]]$question), ":\n"))
#'       categories <- attLst[[i]]$categories
#'       instructions <- "Enter all the indices that correspond to the values you want to keep."
#'       selInd <- select_hf_classes(categories, instructions)
#'       if (is.null(selInd)) {
#'         categories <- categories
#'       } else {
#'         categories <- categories[selInd]
#'       }
#'       if (any(grepl(attLst[[i]]$grepImpair, categories, ignore.case = TRUE))) {
#'         yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on", attLst[[i]]$msg, "?"))
#'       } else {
#'         yn <- 2
#'       }
#'       if (yn == 1) {
#'         causes <- attLst[[i]]$causes
#'         selInd <- select_hf_classes(causes, instructions)
#'         if (is.null(selInd)) {
#'           causes <- causes
#'         } else {
#'           causes <- causes[selInd]
#'         }
#'       } else {
#'         causes <- "Not relevant"
#'       }
#'       # Alphabetical order, so the order of selection doesn't mind (important for further comparisons)
#'       categories <- categories[order(categories)]
#'       causes <- causes[order(causes)]
#'       write(paste0(names(attLst)[i], ": ", paste(categories, collapse = " + ")), file = paste(tempDir, "selected_hf.txt", sep = "/"), append = TRUE)
#'       write(paste0(names(attLst)[i], "_barriers: ", paste(causes, collapse = " + ")), file = paste(tempDir, "selected_hf.txt", sep = "/"), append = TRUE)
#'       if (all(categories == attLst[[i]]$stopFiltering)) {
#'         stopFiltering <- TRUE
#'       }
#'     }
#'   }
#'   return(stopFiltering)
#' }
