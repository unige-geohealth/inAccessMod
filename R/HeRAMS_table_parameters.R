
HeRAMS_table_parameters <- function() {
  list(
    Main_information = list(
      Health_facility_type = "MoSD3", 
      Ownership = "MoSD7"),
    Operationality = list(
      Health_facility_status = "MoSD4", 
      Building_condition = "CONDB", 
      Functionality = "HFFUNCT",
      Accessibility = "HFACC",
      opSuffix = "_x"),
    Services = list(
      Services = "QHeRAMS",
      serSuffix = "[0-9]{3}x")
  )
}

# Get column code and label
tableParam <- inAccMod::HeRAMS_table_parameters()
get_HeRAMS_codes <- function (tableParam) {
  for (i in 1:length(tableParam)){
    message(paste0("\n", gsub("_", " ", names(tableParam)[i])))
    for (j in 1:length(tableParam[[i]])) {
      cat(paste0("\n", gsub("_", " ", names(tableParam[[i]])[j]), ": ", tableParam[[i]][j]))
    }
    yn <- utils::menu(c("YES", "NO"), title = "\n\nWould you like to modify any column code ?")
    if (yn == 1) {
      for (j in 1:length(tableParam[[i]])) {
        cat(paste0(gsub("_", " ", names(tableParam[[i]])[j]), ": ", tableParam[[i]][j]))
        cat("\nType the new code or just ENTER to keep the default value.")
        newCode <- readline(prompt = "New code: ")
        if (nchar(newCode) > 0) {
          tableParam[[i]][j] <- newCode
        }
      }
    }
  }
  return(tableParam)
}
newTableParam <- get_HeRAMS_codes(tableParam)
