
her <- tryCatch({readxl::read_excel(pathTable, skip = 0, sheet = 1, trim_ws = FALSE)}, error = function(e){NULL})
head(her)
her <- as.data.frame(her)

vals <- paste0("QHeRAMS",1:513)
pillar <- NULL
services <- list()
colProcessed <- NULL
m <- 0
for (i in 1:ncol(her)) {
  colN <- her[1, i]
  nameCol <- colnames(her)[i]
  colN <- gsub("\\_[0-9]$", "", colN)
  if (colN %in% colProcessed) {
    next
  }
  if (colN %in% vals) {
    m <- m + 1
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

subCat <- list()
  
subCat[["general_clinical_and_emergency_care_services"]] <- c(ambulance = 1, triage = 2, emergency_care = 3, referral = 4, outpatient_services = 2, home_visit = 1, trauma_care = 4,
            inpatient_capacity = 5, laboratory = 3, blood_bank = 1, hemodialysis_unit = 1, radiology = 2, medevac = 1, discharge = 1)
subCat[["child_health_and_nutrition"]] <- c(management_of_childhood_illnesses = 4, EPI = 2, malnutrition = 6)

subCat[["communicable_diseases"]] <- c(surveillance = 2, malaria = 2, vector_control = 1, MDA = 1, tuberculosis = 2, priority_diseases = 10)

subCat[["sexual_and_reproductive_health"]] <- c(prevention = 3, STIs = 1, HIV_testing_and_treatment = 3, family_planning = 1, antenatal_care = 1, safe_delivery = 2,
                 emergency_obstetric_care = 2, post_partum_care = 1, abortion_care = 1, sexual_violence = 3)
subCat[["noncommunicable_diseases"]] <- c(self_care = 1, NCD_clinic = 1, ASTHMA_COPD = 1, hypertension = 1, diabetes = 1, rehabilitation_services = 3, oral_health = 1,
                 mental_health = 4)

howMany <- NULL
for (i in 1:length(subCat)) {
  howMany <- c(howMany, sum(subCat[[i]]))
}
for (i in 1:length(subCat)) {
  message(i)
  print(length(pillar[pillar == i]) == sum(subCat[[i]]))
}

newLst <- list()
for (i in 1:length(pillars)) {
  newLst[[names(pillars)[i]]] <- list()
  sub <- subCat[[i]]
  initialLst <- pillars[[i]]
  for (j in 1:length(sub)) {
    newLst[[i]][[names(sub)[1]]] <- initialLst[1:sub[1]]
    initialLst <- initialLst[-c(1:sub[1])]
    sub <- sub[-1]
  }
}



# newLst <- list()
# for (i in 1:length(pillars)) {
#   newLst[[names(pillars)[i]]] <- list()
#   initialLst <- pillars[[i]]
#   while (length(initialLst) > 0) {
#     nServ <- 1:length(initialLst)
#     serv <- names(initialLst)
#     indCat <- paste(paste0("\n", nServ, ": ", serv))
#     cat(indCat)
#     cat("\nOn the same line separated by a space, or just skip to select all options.\n")
#     selInd <- readline(prompt = "Selection: ")
#     selInd <- as.numeric(unlist(strsplit(x = selInd, split=" ")))
#     catName <- readline(prompt = "Name of category: ")
#     newLst[[i]][[catName]] <- initialLst[selInd]
#     initialLst <- initialLst[!initialLst %in% initialLst[selInd]]
#   }
# }
