#' Filter facilities
#'
#' Internal function used in the filter_hf function.
#' @param tib tibble;
#' @param var character vector; variable names (from filter_hf function)
#' @param outFolder character; output folder
#' @return A tibble
#' @export
filter_facilities <- function (tib, var, outFolder) {
  categories <- dplyr::pull(tib, var) %>% unique()
  # If no NA
  if (!any(is.na(categories))) {
    # Only one category
    if (length(categories) == 1){
      cat(paste0("\nAll entries has '", categories, "' value for ", gsub("_", " ", names(var))," column.\n"))
      write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", categories), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
      return(tib)
    } else {
      cat("\n")
      gsub("_", " ", names(var)) %>% stringr::str_to_sentence() %>%  message()
      nCat <- 1:length(categories)
      indCat <- paste(paste0("\n", nCat, ": ", categories))
      cat(indCat)
      cat(paste("\n\nEnter all the indices that correspond to", gsub("_", " ", names(var)), "you want to keep.\nOn the same line separated by a space, or just skip to select all options.\n"))
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
      # All categories are selected
      if (length(selInd) == 0){
        write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(tib)
        # If invalid index, all categories are selected
      } else if (!all(selInd %in% nCat)) {
        write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(NULL)
      } else {
        # Only selected categories
        tib <- tib[tib[, var, drop = TRUE] %in% categories[selInd], ]
        write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories[selInd], collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(tib)
      }
    }
  } else {
    # If only NA
    if (all(is.na(categories))) {
      cat("\n")
      message(paste("\n\nThere are ONLY missing values for", gsub("_", " ", names(var)),":"))
      yn <- utils::menu(c("YES", "NO"), title = paste("\nDo you want to keep all the health facilities (if not, the script will stop and no output will be produced)?"))
      # We keep all the data and keep running the script
      if (yn == 1) {
        write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(tib)
      }else{
        stop_quietly("You exited the script as a variable had only missing values. No output has been produced.")
      }
      # Some NA
    } else {
      categories <- categories[!is.na(categories)]
      # Besides NA, only one category
      if (length(categories) == 1){
        message(paste("\n\nThere are missing values for", gsub("_", " ", names(var)), "for the following facilities:\n"))
        print(tib[is.na(dplyr::pull(tib, var)), c("external_id", "workspace_id", "date", "MoSD3", "HFNAME", var)])
        yn <- utils::menu(c("YES", "NO"), title = paste("\nDo you want to keep these health facilities?"))
        # We keep the category and the NA
        if (yn == 1) {
          cat(paste0("Besides missing values, all entries has '", categories, "' value for ", gsub("_", " ", names(var))," column.\n"))
          write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, ", NA")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        } else {
          # We only keep the category, discarding the NA
          cat(paste0("Besides missing values, all entries has '", categories, "' value for ", gsub("_", " ", names(var))," column.\n"))
          write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", categories), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          tib <- tib[!is.na(tib[, var, drop = TRUE]), ]
          return(tib)
        }
      }
      # Besides NA, several categories
      cat("\n")
      gsub("_", " ", names(var)) %>% stringr::str_to_sentence() %>%  message()
      nCat <- 1:length(categories)
      indCat <- paste(paste0("\n", nCat, ": ", categories))
      cat(indCat)
      cat(paste("\n\nEnter all the indices that correspond to", gsub("_", " ", names(var)), "you want to keep.\nOn the same line separated by a space, or just skip to select all options.\n"))
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
      # All are selected
      if (length(selInd == 0)) {
        message(paste("\nThere are missing values for", gsub("_", " ", names(var)), "for the following facilities:\n"))
        print(tib[is.na(dplyr::pull(tib, var)), c("external_id", "workspace_id", "date", "MoSD3", "HFNAME", var)])
        yn <- utils::menu(c("YES", "NO"), title = paste("\nDo you want to keep these health facilities?"))
        # All categories and NA are kept
        if (yn == 1) {
          write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", "), ", NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        } else {
          # All categories but no NA
          tib <- tib[!is.na(tib[, var, drop = TRUE]), ]
          write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        }
        # Invalid index, all categories and NA are kept
      } else if (!all(selInd %in% nCat)) {
        write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories, collapse = ", "), ", NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
        return(NULL)
      } else {
        message(paste("\nThere are missing values for", gsub("_", " ", names(var)), "for the following facilities:\n"))
        print(tib[is.na(dplyr::pull(tib, var)), c("external_id", "workspace_id", "date", "MoSD3", "HFNAME", var)])
        yn <- utils::menu(c("YES", "NO"), title = paste("\nDo you want to keep these health facilities?"))
        # Selected categories and NA are kept
        if (yn == 1) {
          tib <- tib[tib[, var, drop = TRUE] %in% categories[selInd] | is.na(tib[, var, drop = TRUE]), ]
          write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories[selInd], collapse = ", "), ", NA"), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        } else {
          # Only selected categories
          tib <- tib[tib[, var, drop = TRUE] %in% categories[selInd], ]
          write(paste0(stringr::str_to_sentence(gsub("_", " ", names(var))), ": ", paste(categories[selInd], collapse = ", ")), file = paste(outFolder, "log.txt", sep = "/"), append = TRUE)
          return(tib)
        }
      }
    }
  }
}
