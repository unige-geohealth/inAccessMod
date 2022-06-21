# 
# # Operationality ------------------------------
# building <- list(categories = c("Not damaged: intact building or insignificant damage", "Partially damaged: requiring substantial to large scale repair, but at lower cost than full reconstruction", "Fully damaged: or major damage requiring complete reconstruction, or extent of damage is such that costs to repair would be as high as cost for reconstruction"),
#                  grepImpair = "partially|fully",
#                  causes = c("Natural disaster", "Man-made disaster", "Conflict / attack / looting", "Lack of maintenance", "Other"),
#                  stopFiltering = "Fully damaged: or major damage requiring complete reconstruction, or extent of damage is such that costs to repair would be as high as cost for reconstruction",
#                  msg = "the causes of damage",
#                  question = "Building condition")
# 
# 
# functionality <- list(categories = c("Fully functioning", "Partially functioning", "Non-functioning"),
#                       grepImpair = "partially|non",
#                       causes = c("Lack of staff", 
#                                 "Lack of medical supplies (drugs and consumables)", 
#                                 "Lack of medical equipment (logistics, fuel, vehicle)", 
#                                 "Lack of physical access: (e.g. distance, road blocks)",
#                                 "Insecurity",
#                                 "Lack of finances",
#                                 "Damage of the health facility",
#                                 "Other"),
#                       stopFiltering = "Not functioning",
#                       msg = "the causes of dysfunctionality",
#                       question = "Funcionality")
# 
# 
# accessibility <- list(categories = c("Fully accessible", "Partially accessible", "Not accessible"),
#                       grepImpair = "partially|not",
#                       causes = c("Security situation", 
#                                  "Physical barriers: (e.g. distance, road/bridge damage)", 
#                                  "Financial (fees for patients)", 
#                                  "Cultural barrier",
#                                  "Not designed for people with limited mobility",
#                                  "Other"),
#                       stopFiltering = "Not accessible",
#                       msg = "the causes of inaccessibility",
#                       question = "Accessibility")
# 
# 
# operationality <- list(Building = building, Functionality = functionality, Accessibility = accessibility)
# 
# # General clinical and emergency care services ------------------------------
# 
# # Ambulance 
# ambulance <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                   grepImpair = "partially available|not available",
#                   causes = c("Lack of staff", 
#                              "Lack of training", 
#                              "Lack of inputs", 
#                              "Lack of equipment",
#                              "Lack of financial resources"),
#                   stopFiltering = "NULL",
#                   msg = "the barriers for ambulance service")
# 
# ambulance <- list(Request_for_ambulance_services_by_the_patient = ambulance)
# 
# # Triage
# recogn_danger <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                       grepImpair = "partially available|not available",
#                       causes = c("Lack of staff", 
#                                  "Lack of training", 
#                                  "Lack of medical supplies", 
#                                  "Lack of equipment",
#                                  "Lack of financial resources"),
#                       stopFiltering = "NULL",
#                       msg = "the barriers for the recognition of danger signs")
# 
# acuity_formal <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                       grepImpair = "partially available|not available",
#                       causes = c("Lack of staff", 
#                                  "Lack of training", 
#                                  "Lack of medical supplies", 
#                                  "Lack of equipment",
#                                  "Lack of financial resources"),
#                       stopFiltering = "NULL",
#                       msg = "the barriers for the acuity-based formal triage")
# 
# 
# triage <- list(Recognition_of_danger_signs = recogn_danger, Acuity_based_formal_triage = acuity_formal)
# 
# # Final pillar list
# general_clinical_and_emergency_care_services <- list(Ambulance = ambulance, Triage = triage)
# 
# # Child health and nutrition ------------------------------
# 
# # Management of childhood illness
# comm_first_aid <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                                   grepImpair = "partially available|not available",
#                                   causes = c("Lack of staff", 
#                                              "Lack of training", 
#                                              "Lack of medical supplies", 
#                                              "Lack of equipment",
#                                              "Lack of financial resources"),
#                                   stopFiltering = "NULL",
#                                   msg = "the barriers for the community-based first aid")
# 
# comm_IMNCI <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                               grepImpair = "partially available|not available",
#                               causes = c("Lack of staff", 
#                                          "Lack of training", 
#                                          "Lack of medical supplies", 
#                                          "Lack of equipment",
#                                          "Lack of financial resources"),
#                               stopFiltering = "NULL",
#                               msg = "the barriers for the community-based IMNCI (Integrated Management of Newborn and Childhood and Illnesses)")
# 
# 
# management_of_childhood_illnesses <- list(Community_based_first_aid = comm_first_aid, Community_based_IMNCI = comm_IMNCI)
# 
# # EPI
# comm_mob_EPI <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                    grepImpair = "partially available|not available",
#                    causes = c("Lack of staff", 
#                               "Lack of training", 
#                               "Lack of medical supplies", 
#                               "Lack of equipment",
#                               "Lack of financial resources"),
#                    stopFiltering = "NULL",
#                    msg = "the barriers for the community mobilization for EPI")
# 
# 
# exp_pr_imun <- list(categories = c("Available", "Partially available", "Not available", "Not normally provided"),
#                      grepImpair = "partially available|not available",
#                      causes = c("Lack of staff", 
#                                 "Lack of training", 
#                                 "Lack of medical supplies", 
#                                 "Lack of equipment",
#                                 "Lack of financial resources"),
#                      stopFiltering = "NULL",
#                      msg = "the barriers for the EPI (Expanded Programme on Immunization)")
# 
# epi <- list(Communtiy_mobilization_for_EPI = comm_mob_EPI, EPI = exp_pr_imun)
# 
# # Final pillar list
# child_health_and_nutrition <- list(Management_of_childhood_illnesses = management_of_childhood_illnesses, 
#                                    EPI = epi)
# 
# servicePillars <- list(General_clinical_and_emergency_care_services = general_clinical_and_emergency_care_services,
#                        Child_health_and_nutrition = child_health_and_nutrition)
# 
# hf_attributes <- list(Operationality = operationality, Services = servicePillars)
# # hf_attributes <- list(Operationality = operationality, Services = newLst)
# 
# usethis::use_data(hf_attributes, overwrite = TRUE)
# 
# HeRAMS_codes_and_labels <- readxl::read_excel("./data/HeRAMS_codes_and_labels.xlsx", skip = 0, sheet = 1, trim_ws = FALSE)
# usethis::use_data(HeRAMS_codes_and_labels, overwrite = TRUE)
# 
# 
# 
# col_correspondence <- list(Status = "MoSD4",
#                 Building = "CONDB",
#                 Building_barriers = paste0("CONDBx_", 1:5),
#                 Functionality = "HFFUNCT",
#                 Functionality_barriers = paste0("HFFUNCTx_", 1:8),
#                 Accessibility = "HFACC",
#                 Accessibility_barriers = paste0("HFACCx_", 1:6))
# 
# usethis::use_data(col_correspondence, overwrite = TRUE)

