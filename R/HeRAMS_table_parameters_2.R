# 
# # Get column code and label
# tableParam <- HeRAMS_table_parameters()
# set_HeRAMS_variables <- function (tableParam) {
#   message("Variables and column names (compatible with regular expression)")
#   for (i in 1:length(tableParam)){
#     cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
#   }
#   yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any value ?")
#   if (yn == 1) {
#     for (i in 1:length(tableParam)) {
#       msg <- "value (compatible with regular expression)"
#       cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
#       cat(paste("\nType the new", msg, "or just ENTER to keep the default value."))
#       newCode <- readline(prompt = "New value: ")
#       if (nchar(newCode) > 0) {
#         tableParam[[i]] <- newCode
#       }
#     }
#   }
#   return(tableParam)
# }
# 
# codeColumns <- set_HeRAMS_variables(tableParam)
# # Get stop values
# stopLst <- inAccMod::HeRAMS_stop_filtering()
# 
# set_HeRAMS_stop <- function (stopLst) {
#   message("\nStop response code (response that stops the questionnaire)")
#   for (i in 1:length(stopLst)){
#     cat(paste0("\n", gsub("_", " ", names(stopLst)[i]), ": ", stopLst[[i]]))
#   }
#   yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any stop response code (response that stops the questionnaire) ?")
#   if (yn == 1) {
#     cat(paste0("\n", gsub("_", " ", names(stopLst)[i], ": ", stopLst[[i]])))
#     cat(paste("\nType the new code or just ENTER to keep the default value."))
#     newCode <- readline(prompt = "New value: ")
#     if (nchar(newCode) > 0) {
#       stopLst[[i]] <- newCode
#     }
#   }
#   return(stopLst)
# }
# 
# 
# stopLst <- set_HeRAMS_stop(stopLst)
# herTxt <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 2, trim_ws = FALSE)}, error = function(e){NULL})
# herCode <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 1, trim_ws = FALSE)}, error = function(e){NULL})
# 
# # Check same order
# all(herCode$external_id == herTxt$external_id)
# 
# # Process the filtering with txt table, but check with code for stop filtering
# # All columns taken into account
# cols <- NULL
# for (i in 1:length(codeColumns)) {
#   if (grepl("suffix", names(codeColumns)[i], ignore.case = TRUE)) {
#     next
#   }
#   cols <- c(cols, colnames(herTxt)[grep(codeColumns[[i]], colnames(herTxt))])
# }
# 
# tibTxt <- herTxt
# tibCode <- herCode
# 
# colStop <- NULL
# for (var in names(stopLst)) {
#   colStop <- c(colStop, unlist(codeColumns)[grep(var, names(unlist(codeColumns)))])
# }
# 
# # From which column the questionnaire can be stopped
# indStatus <- which(cols == codeColumns$Health_facility_status)
# remainCols <- cols[indStatus:length(cols)]
# 
# # Adding "does not apply" value when questionnaire has stopped
# for (i in 1:length(names(stopLst))) {
#   var <- names(stopLst)[i]
#   message(var)
#   colCode <- codeColumns[[var]]
#   varStop <- stopLst[[var]]
#   remainCols <- remainCols[!grepl(colCode, remainCols)]
#   if (any(is.na(tibCode[, colCode, drop = TRUE]))) {
#     cat("\nValues for the following facilities are missing.\n")
#     print(tibTxt[is.na(tibCode[, colCode, drop = TRUE]), c(1:10)])
#     yn <- utils::menu(c("Ignore these facilities", "Exit the script and solve the issue manually"), title = "Select an option.")
#     if (yn == 2) {
#       stop_quietly("You exit the script.")
#     }
#   }
#   if (sum(tibCode[, colCode, drop = TRUE] == varStop) > 0) {
#     tibCode[tibCode[, colCode, drop = TRUE] == varStop, remainCols] <- "Does not apply (questionnaire was stopped before)"
#     tibTxt[tibCode[, colCode, drop = TRUE] == varStop, remainCols] <- "Does not apply (questionnaire was stopped before)"
#   }
# }
# 
# # To get the name of the services (we don't skip the first row)
# herTxtNames <- tryCatch({readxl::read_excel(pathTable, skip = 0, sheet = 2, trim_ws = FALSE)}, error = function(e){NULL})
# 
# ### List of services to select
# ### Default NO causes
# ### write
# 
# # Main information an operationality
# tibC <- tibCode
# tibT <- tibTxt
# for (i in 1:length(codeColumns)) {
#   # Look if there is a perfect match
#   varCol <- colnames(tibT)[grep(paste0("^", codeColumns[[i]], "$"), colnames(tibT))]
#   # If not (e.g. suffix element)
#   if (length(varCol) == 0) {
#     next
#   }
#   # If only one match (main info or operationality columns)
#   if (length(varCol) == 1) {
#     codeName <- names(codeColumns)[i]
#     message(paste0("\n", gsub("_", " ", codeName)))
#     newTib <- subsetting_process(tibT, tibC, varCol, impairValues = "A2|A3", stopQuest = TRUE, codeName, stopLst)
#     tibT <- newTib[[1]]
#     tibC <- newTib[[2]]
#   } else {
#     # Services
#     message("\n\nEssential health services")
#     yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on specific health services ?"))
#     if (yn == 1) {
#       pillars <- c("General clinical and emergency care services",
#                    "Child health and nutrition",
#                    "Communicable diseases",
#                    "Sexual and reproductive health",
#                    "Noncommunicable diseases")
#       nCat <- 1:length(pillars)
#       indCat <- paste(paste0("\n", nCat, ": ", pillars))
#       cat(indCat)
#       cat(paste("\n\nEnter all the indices that correspond to the pillars that include the services you would like to focus.\nOn the same line separated by a space, or just skip to select all options.\n"))
#       selInd <- readline(prompt = "Selection: ")
#       selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
#       if (length(selInd) == 0){
#         selInd <- nCat
#       }
#       for (j in selInd){
#         message(paste("\nPillar: ", pillars[j]))
#         subVarCol <- varCol[grepl(paste0(j, "[0-9]{2}"), varCol)]
#         
#         pillarServices <- NULL
#         for (k in 1:length(subVarCol)) {
#           pillarServices <- c(pillarServices, colnames(herTxtNames)[grep(paste0("^", subVarCol[k], "$"), herTxtNames[1, ])])
#         }
#         nCat <- 1:length(pillarServices)
#         indCat <- paste(paste0("\n", nCat, ": ", pillarServices))
#         cat(indCat)
#         cat(paste("\n\nEnter all the indices that correspond to the services you would like to focus.\nOn the same line separated by a space, or just skip to select all options.\n"))
#         selInd <- readline(prompt = "Selection: ")
#         selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
#         if (length(selInd) == 0){
#           selInd <- nCat
#         }
#         for (ind in selInd) {
#           subSubVarCol <- subVarCol[ind]
#           message(paste0("\n", colnames(herTxtNames)[grep(paste0("^", subSubVarCol, "$"), herTxtNames[1, ])]))
#           subSubVarCol <- subVarCol[ind]
#           newTib <- subsetting_process(tibT, tibC, subSubVarCol, impairValues = "A2|A3", stopQuest = FALSE)
#           tibT <- newTib[[1]]
#           tibC <- newTib[[2]]
#         }
#       }
#     }
#   }
# }
# 
# subsetting_process <- function (tibT, tibC, varCol, impairValues = "A2|A3", stopQuest = TRUE, codeName = NULL, stopLst = NULL) {
#   categories <- unique(tibT[, varCol, drop = TRUE])
#   selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
#   if (is.null(selInd)) {
#     categories <- categories
#   } else {
#     categories <- categories[selInd]
#   }
#   # If selInd is equal to the length of categories + 1
#   if (length(categories) == 1) {
#     if (is.na(categories)) {
#       stop_quietly("You canceled the filtering process.")
#     }
#   }
#   tibC <- tibC[tibT[, varCol, drop = TRUE] %in% categories, ]
#   tibT <- tibT[tibT[, varCol, drop = TRUE] %in% categories, ]
#   # Check if stop
#   if (stopQuest) {
#     if (codeName %in% names(stopLst)) {
#       if (all(tibC[tibT[, varCol, drop = TRUE] %in% categories, varCol, drop = TRUE] == stopLst[[codeName]])) {
#         print("STOP")
#         return(list(tibT, tibC))
#       }
#     }
#   }
#   # Check if they are possible barriers
#   colBarriers <- colnames(tibT)[grep(paste0(varCol, codeColumns$Barrier_suffix), colnames(tibT))]
#   if (length(colBarriers) == 0) {
#     return(list(tibT, tibC))
#   } else {
#     impair <- grepl(impairValues, tibC[, varCol, drop = TRUE])
#     if (any(impair)){
#       yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on the causes for the impairment ?"))
#       if (yn == 1) {
#         # Get possible responses
#         resps <- NULL
#         for (j in 1:length(colBarriers)) {
#           resp <- tibT[, colBarriers[j], drop = TRUE]
#           resp <- resp[complete.cases(resp)]
#           resps <- c(resps, resp)
#         }
#         categories <- unique(resps)
#         selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
#         if (is.null(selInd)) {
#           categories <- categories
#         } else {
#           categories <- categories[selInd]
#         }
#         # As there are different columns that can contain the value
#         condMat1 <- matrix(NA, nrow = nrow(tibT), ncol = length(colBarriers))
#         for (j in 1:length(colBarriers)) {
#           condMat2 <- matrix(NA, nrow = nrow(tibT), ncol = length(categories))
#           for (k in 1:length(categories)) {
#             condMat2[, k] <- categories[k] == tibT[, colBarriers[j], drop = TRUE]
#           }
#           condMat1[, j] <- apply(condMat2, 1, any)
#         }
#         tibC <- tibC[apply(condMat1, 1, any, na.rm = TRUE), ]
#         tibT <- tibT[apply(condMat1, 1, any, na.rm = TRUE), ]
#       }
#     }
#   }
#   return(list(tibT, tibC))
# }

