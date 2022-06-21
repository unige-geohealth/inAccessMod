#' Analysis scenario selection (HeRAMS data)
#'
#' Interactive selection of the health facility attributes to determine the analysis scenario for the accessibility modelling
#' @param hf_attributes list; internal data with all the available attribute values
#' @details Selection is sequential and is based on 1) the status, 2) the operationality and 3) the service availability. For operationality
#' and service availability, selection based on barriers/impairment can be made when impaired facilities are selected.
#' @export
# analysis_scenario <- function (HeRAMSTable)) {
#   questions <- readxl::read_excel("./data/HeRAMS_questions.xlsx", sheet = 1)
# 
#   
#   
#   message("Health facility main information")
#   mainInfo <- tableParam[1:2]
#   print(mainInfo)
#   yn <- utils::menu(c("YES", "NO"), title = "\nWould you like to modify any column code ?")
#   if (yn == 1) {
#     for (i in 1:length(mainInfo)) {
#       message(paste0("\n", names(mainInfo)[i], ": ", mainInfo[[i]]))
#       cat("Type the new code or just ENTER to keep the default value.")
#       newCode <- readline(prompt = "New code: ")
#       if (nchar(newCode) > 0) {
#         mainInfo[i] <- newCode
#       }
#     }
#   }
#   
#   message("Health facility operationality")
#   operationality <- questions[3:9, ]
#   print(operationality)
#   yn <- utils::menu(c("YES", "NO"), title = "\nWould you like to modify any column code ?")
#   if (yn == 1) {
#     for (i in 1:nrow(questions)) {
#       message(paste0("\n", operationality[i, 1, drop = TRUE], ": ", operationality[i, 2, drop = TRUE]))
#       cat("Type the new code or just ENTER to keep the default value.")
#       newCode <- readline(prompt = "New code: ")
#       if (nchar(newCode) > 0) {
#         operationality[i, 2] <- newCode
#       }
#     }
#   }
#   
#   message("Essential health services")
#   services <- questions[10:11, ]
#   print(services)
#   yn <- utils::menu(c("YES", "NO"), title = "\nWould you like to modify any column code ?")
#   if (yn == 1) {
#     for (i in 1:nrow(services)) {
#       message(paste0("\n", services[i, 1, drop = TRUE], ": ", services[i, 2, drop = TRUE]))
#       cat("Type the new code or just ENTER to keep the default value.")
#       newCode <- readline(prompt = "New code: ")
#       if (nchar(newCode) > 0) {
#         services[i, 2] <- newCode
#       }
#     }
#   }
#   
#   infoOper <- rbind(mainInfo, operationality)
#   
#   HeRAMSTableCode <- readxl::read_excel(pathTable, skip = 1, sheet = 1)
#   HeRAMSTableTxt <- readxl::read_excel(pathTable, skip = 1, sheet = 2)
#   all(colnames(HeRAMSTableCode) == colnames(HeRAMSTableTxt))
#   
#   for (i in 1:nrow(infoOper)) {
#     colN <- gsub("_[0-9]*", "", colnames(HeRAMSTableCode))
#     
#     
#     duplic <- sum(duplicated(colnames(HeRAMSTableCode)[questions$column_code[i] == colN]))
#     
#   }
#   
#   
# 
#   
#   stopFiltering <- FALSE
#   HeRAMSTableCode <- readxl::read_excel(pathTable, skip = 1, sheet = 2)
#   colnames(HeRAMSTableCode)
#   for (i in 1:nrow(questions)) {
#     message(paste0("\n", stringr::str_to_sentence(questions[i, 1, drop = TRUE])))
#     colN <- questions[i, 2, drop = TRUE]
#     categories <- unique(HeRAMSTableCode[, colN, drop = TRUE])
#     categories <- categories[order(categories)]
#     instructions <- "Enter all the indices that correspond to the values you want to keep."
#     selInd <- select_hf_classes(categories, instructions)
#     if (is.null(selInd)){
#       categories <- categories
#     } else {
#       categories <- categories[selInd]
#     }
#   }
# 
#  
#   
#   categories <- c("Existing", "Closed")
#   instructions <- "Enter all the indices that correspond to the values you want to keep."
#   selInd <- select_hf_classes(categories, instructions)
#   if (is.null(selInd)){
#     status <- categories
#   } else {
#     status <- categories[selInd]
#   }
#   if (all(status == "Closed")) {
#     stopFiltering <- TRUE
#   } else {
#     message("\nHealth facility operationality")
#   }
#   # Alphabetical order, so the order of selection doesn't mind (important for further comparisons)
#   status <- status[order(status)]
#   write(paste0("Status: ", paste(status, collapse = " + ")), file = paste(tempDir, "selected_hf.txt", sep = "/"), append = TRUE)
#   
#   stopFiltering <- select_hf_main(operationality, stopFiltering)
#   
#   if (stopFiltering) {
#     message("Analysis scenario selection: done!")
#     return(NULL)
#   }
#   
#   yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on specific service availability?"))
#   if (yn ==2) {
#     message("Analysis scenario selection: done!")
#     return(NULL)
#   } else {
#     message("\nMain pillars selection")
#     pillarNames <- gsub("_", " ", names(inAccMod::hf_attributes$Services))
#     instructions <- "Enter all the indices that correspond to pillars you would like to focus on."
#     selInd <- select_hf_classes(pillarNames, instructions)
#     if (is.null(selInd)) {
#       pillars <- inAccMod::hf_attributes$Services
#     } else {
#       pillars <- inAccMod::hf_attributes$Services[selInd]
#     }
#     for (i in 1:length(pillars)) {
#       message(paste("\nCategory selection for", gsub("_", " ", names(pillars)[i])))
#       categoryNames <- stringr::str_to_title(gsub("_", " ", names(pillars[[i]])))
#       instructions <- "Enter all the indices that correspond to the pillar categories you would like to focus on."
#       selInd <- select_hf_classes(categoryNames, instructions)
#       if (is.null(selInd)) {
#         pillarCategories <- pillars[[i]]
#       } else {
#         pillarCategories <- pillars[[i]][selInd]
#       }
#       for (j in 1:length(pillarCategories)) {
#         stopFiltering <- select_hf_main(pillarCategories[[j]], stopFiltering = FALSE)
#       }
#     }
#   }
# }
#   
#   
#   
#   
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
