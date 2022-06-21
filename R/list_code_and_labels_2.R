create_service_list <- function (mainPath, country, HeRAMSTable) {
  if (!is.character(mainPath)) {
    stop("mainPath must be 'character'")
  }
  if (!is.character(country)) {
    stop("country must be 'character'")
  }
  pathFacilities <- paste0(mainPath, "/", country, "/data/vFacilities")
  if (!dir.exists(pathFacilities)) {
    stop(paste(pathFacilities, "does not exist. Run the initiate_project function first or check the input parameters."))
  }
  her <- tryCatch({readxl::read_excel(HeRAMSTable, skip = 0, sheet = 1, trim_ws = FALSE)}, error = function(e){NULL})
  if (is.null(her)) {
    stop("HeRAMS Excel table is missing.")
  }
  her <- as.data.frame(her)
  codeTablePath <- paste(pathFacilities, "code_and_labels.xlsx", sep = "/")
  codeTable  <- tryCatch({readxl::read_excel(codeTablePath, skip = 0, sheet = 1, trim_ws = FALSE)}, error = function(e){NULL})
  if (is.null(codeTable)) {
    message(paste("\nPlease check the answer codes and labels in the following Excel table:\n", codeTablePath))
    message("Modify it according to the")
  }
  
  writexl::write_xlsx(inAccMod::HeRAMS_codes_and_labels, codeTable)

  
  codes <- tryCatch({readxl::read_excel(codeTable, skip = 0, sheet = 1, trim_ws = FALSE)}, error = function(e){NULL})
  codes <- as.data.frame(codes)
  nQuestions <- gsub("QHeRAMS|x_[0-9]*", "", her[1, ncol(her) - 1])
  vals <- paste0("QHeRAMS",1:nQuestions)
  pillar <- NULL
  services <- list()
  colProcessed <- NULL
  for (i in 1:ncol(her)) {
    colN <- her[1, i]
    nameCol <- colnames(her)[i]
    colN <- gsub("\\_[0-9]$", "", colN)
    if (colN %in% colProcessed) {
      next
    }
    if (colN %in% vals) {
      colProcessed <- c(colProcessed, colN)
      message(colN)
      pillar <- c(pillar, as.numeric(substr(colN, 8, 8)))
      if (as.numeric(substr(colN, 8, 8)) == 3) {
        print(nameCol)
      }
      objectName <- paste0(gsub("[^[[:alnum:]]", "", stringr::word(nameCol, 1, 2, sep = " ")), "_", colN)
      # objectName <- readline(prompt = "objectName: ")
      services[[objectName]] <- list(categories = c("Fully available", "Partially available", "Not available", "Not normally provided"),
                                     grepImpair = "partially available|not available",
                                     causes = c("Lack of staff", 
                                                "Lack of training", 
                                                "Lack of medical supplies (drugs and consumables)", 
                                                "Lack of medical equipment (incl. logistic, fuel, vehicles)",
                                                "Lack of financial resources"),
                                     stopFiltering = "NULL",
                                     msg = paste("the barriers for:", nameCol),
                                     question = nameCol)
      
    }
  }
  
  pillarNames <- c("General_clinical_and_emergency_care", "Child_health_and_nutrition", "Communicable_diseases", "Sexual_and_reproductive_health", "Noncommunicable_diseases")
  pillars <- vector("list", 5)
  names(pillars) <- pillarNames
  for (i in 1:length(pillars)) {
    pillars[[i]] <- services[pillar == i]
  }
  
}





