
HeRAMS_table_parameters <- function() {
  list(
    Health_facility_type = "MoSD3", 
    Ownership = "MoSD7",
    Health_facility_status = "MoSD4", 
    Building_condition = "CONDB", 
    Functionality = "HFFUNCT",
    Accessibility = "HFACC",
    Services = "QHeRAMS[0-9]{3}",
    Barrier_suffix = "x_[0-9]" 
  )
}

HeRAMS_stop_filtering <- function() {
  list(
    Health_facility_status = "A3",
    Building_condition = "A3",
    Functionality = "A3",
    Accessibility = "A3"
  )
}

# Get column code and label
tableParam <- inAccMod::HeRAMS_table_parameters()
set_HeRAMS_variables <- function (tableParam) {
  message("Variables and column names (compatible with regular expression)")
  for (i in 1:length(tableParam)){
    cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
  }
  yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any value ?")
  if (yn == 1) {
    for (i in 1:length(tableParam)) {
      msg <- "value (compatible with regular expression)"
      cat(paste0("\n", gsub("_", " ", names(tableParam)[i]), ": ", tableParam[[i]]))
      cat(paste("\nType the new", msg, "or just ENTER to keep the default value."))
      newCode <- readline(prompt = "New value: ")
      if (nchar(newCode) > 0) {
        tableParam[[i]] <- newCode
      }
    }
  }
  return(tableParam)
}

codeColumns <- set_HeRAMS_variables(tableParam)
# Get stop values
stopValues <- inAccMod::HeRAMS_stop_filtering()

set_HeRAMS_stop <- function (stopValues) {
  for (i in 1:length(stopValues)){
    message(paste0("\n", gsub("_", " ", names(stopValues)[i])))
    cat(paste0("\n", gsub("_", " ", names(stopValues)[i]), ": ", stopValues[[i]]))
    yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify the code stop value (the value that stop the questionnaire) ?")
    if (yn == 1) {
      cat(paste0("\n", gsub("_", " ", names(stopValues)[i], ": ", stopValues[[i]])))
      cat(paste("\nType the new code or just ENTER to keep the default value."))
      newCode <- readline(prompt = "New value: ")
      if (nchar(newCode) > 0) {
        stopValues[[i]] <- newCode
      }
    }
  }
  return(stopValues)
}


stopValues <- set_HeRAMS_stop(stopValues)
herTxt <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 2, trim_ws = FALSE)}, error = function(e){NULL})
herCode <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 1, trim_ws = FALSE)}, error = function(e){NULL})

# Check same order
all(herCode$external_id == herTxt$external_id)

# Work with txt table, but check with code for stop filtering
# All columns 
cols <- NULL
for (i in 1:length(newTableParam)) {
  subParam <- newTableParam[[i]]
  for (j in 1:length(subParam)) {
    if (grepl("suffix", names(subParam)[j], ignore.case = TRUE)) {
      next
    }
    cols <- c(cols, colnames(herTxt)[grep(subParam[[j]], colnames(herTxt))])
  }
}

tibTxt <- herTxt
tibCode <- herCode

colStop <- NULL
for (var in names(stopValues)) {
  colStop <- c(colStop, unlist(codeColumns)[grep(var, names(unlist(codeColumns)))])
}

indStatus <- which(cols == codeColumns$Health_facility_status)
remainCols <- cols[indStatus:length(cols)]

# Adding "does not apply" value when questionnaire has stopped
for (i in 1:length(names(stopValues))) {
  var <- names(stopValues)[i]
  message(var)
  colCode <- codeColumns[[var]]
  varStop <- stopValues[[var]]
  remainCols <- remainCols[!grepl(colCode, remainCols)]
  if (any(is.na(tibCode[, colCode, drop = TRUE]))) {
    cat("\nValues for the following facilities are missing.\n")
    print(tibTxt[is.na(tibCode[, colCode, drop = TRUE]), c(1:10)])
    yn <- utils::menu(c("Ignore these facilities", "Exit the script and solve the issue manually"), title = "Select an option.")
    if (yn == 2) {
      stop_quietly("You exit the script.")
    }
  }
  if (sum(tibCode[, colCode, drop = TRUE] == varStop) > 0) {
    tibCode[tibCode[, colCode, drop = TRUE] == varStop, remainCols] <- "Does not apply"
    tibTxt[tibCode[, colCode, drop = TRUE] == varStop, remainCols] <- "Does not apply"
  }
}

herTxtNames <- tryCatch({readxl::read_excel(pathTable, skip = 0, sheet = 2, trim_ws = FALSE)}, error = function(e){NULL})

### List of services to select
### Default NO causes
### write

# Main information an operationality
tibC <- tibCode
tibT <- tibTxt
for (i in 1:length(codeColumns)) {
  varCol <- colnames(tibT)[grep(paste0("^", codeColumns[[i]], "$"), colnames(tibT))]
  if (length(varCol) == 0) {
    next
  }
  if (length(varCol) == 1) {
    # Main info or operationality
    message(paste0("\n", gsub("_", " ", names(codeColumns)[i])))
    categories <- unique(tibT[, varCol, drop = TRUE])
    categories <- categories[order(categories)]
    selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
    if (is.null(selInd)) {
      categories <- categories
    } else {
      categories <- categories[selInd]
    }
    tibC <- tibC[tibT[, varCol, drop = TRUE] %in% categories, ]
    tibT <- tibT[tibT[, varCol, drop = TRUE] %in% categories, ]
    # Check if stop
    if (names(codeColumns)[i] %in% names(stopValues)) {
      if (all(tibC[tibT[, varCol, drop = TRUE] %in% categories, varCol, drop = TRUE] == stopValues[[names(codeColumns)[i]]])) {
        print("STOP")
      }
    }
    # Check if they are possible barriers
    colBarriers <- colnames(tibT)[grep(paste0(varCol, codeColumns$Barrier_suffix), colnames(tibT))]
    if (length(colBarriers) == 0) {
      next
    } else {
      impair <- grepl("A2|A3", tibC[, varCol, drop = TRUE])
      if (any(impair)){
        yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on the causes for the impairment ?"))
        if (yn == 1) {
          # Get possible responses
          resps <- NULL
          for (j in 1:length(colBarriers)) {
            resp <- tibT[, colBarriers[j], drop = TRUE]
            resp <- resp[complete.cases(resp)]
            resps <- c(resps, resp)
          }
          categories <- unique(resps)
          categories <- categories[order(categories)]
          selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
          if (is.null(selInd)) {
            categories <- categories
          } else {
            categories <- categories[selInd]
          }
          condMat1 <- matrix(NA, nrow = nrow(tibT), ncol = length(colBarriers))
          for (j in 1:length(colBarriers)) {
            condMat2 <- matrix(NA, nrow = nrow(tibT), ncol = length(categories))
            for (k in 1:length(categories)) {
              condMat2[, k] <- categories[k] == tibT[, colBarriers[j], drop = TRUE]
            }
            condMat1[, j] <- apply(condMat2, 1, any)
          }
          
          tibC <- tibC[apply(condMat1, 1, any, na.rm = TRUE), ]
          tibT <- tibT[apply(condMat1, 1, any, na.rm = TRUE), ]
        }
      }
    }
  } else {
    # Services
    yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on specific health services ?"))
    if (yn == 1) {
      pillars <- c("General clinical and emergency care services",
                   "Child health and nutrition",
                   "Communicable diseases",
                   "Sexual and reproductive health",
                   "Noncommunicable diseases")
      nCat <- 1:length(pillars)
      indCat <- paste(paste0("\n", nCat, ": ", pillars))
      cat(indCat)
      cat(paste("\n\nEnter all the indices that correspond to the pillars that include the services you would like to focus.\nOn the same line separated by a space, or just skip to select all options.\n"))
      selInd <- readline(prompt = "Selection: ")
      selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
      if (length(selInd) == 0){
        selInd <- 1:5
      }
      for (j in selInd){
        subVarCol <- varCol[grepl(paste0(j, "[0-9]{2}"), varCol)]
        
        for (k in 1:length(subVarCol)) {
          message(colnames(herTxtNames)[grep(paste0("^", subVarCol[k], "$"), herTxtNames[1, ])])
          subSubVarCol <- subVarCol[k]
          categories <- unique(tibT[, subSubVarCol, drop = TRUE])
          categories <- categories[order(categories)]
          selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
          if (is.null(selInd)) {
            categories <- categories
          } else {
            categories <- categories[selInd]
          }
          tibC <- tibC[tibT[, subSubVarCol, drop = TRUE] %in% categories, ]
          tibT <- tibT[tibT[, subSubVarCol, drop = TRUE] %in% categories, ]
          # Check if they are possible barriers
          colBarriers <- colnames(tibT)[grep(paste0(subSubVarCol, codeColumns$BarrierSuffix), colnames(tibT))]
          if (length(colBarriers) == 0) {
            next
          } else {
            impair <- grepl("A2|A3", tibC[, subSubVarCol, drop = TRUE])
            if (any(impair)){
              yn <- utils::menu(c("YES", "NO"), title = paste("\nWould you like to filter the health facilities on the causes for the impairment ?"))
              if (yn == 1) {
                # Get possible responses
                resps <- NULL
                for (j in 1:length(colBarriers)) {
                  resp <- tibT[, colBarriers[j], drop = TRUE]
                  resp <- resp[complete.cases(resp)]
                  resps <- c(resps, resp)
                }
                categories <- unique(resps)
                categories <- categories[order(categories)]
                selInd <- select_hf_classes(categories, "Select the values that you would like to keep")
                if (is.null(selInd)) {
                  categories <- categories
                } else {
                  categories <- categories[selInd]
                }
                condMat1 <- matrix(NA, nrow = nrow(tibT), ncol = length(colBarriers))
                for (j in 1:length(colBarriers)) {
                  condMat2 <- matrix(NA, nrow = nrow(tibT), ncol = length(categories))
                  for (k in 1:length(categories)) {
                    condMat2[, k] <- categories[k] == tibT[, colBarriers[j], drop = TRUE]
                  }
                  condMat1[, j] <- apply(condMat2, 1, any)
                }
                tibC <- tibC[apply(condMat1, 1, any, na.rm = TRUE), ]
                tibT <- tibT[apply(condMat1, 1, any, na.rm = TRUE), ]
              }
            }
          }
        }
      }
    }
  }
}

