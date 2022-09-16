#' Select HeRAMS Scenario (internal function)
#'
#' @param scenarioDirs character; Scenarios' folders
#' @export
select_scenario <- function (scenarioDirs) {
  if (length(scenarioDirs) == 0) {
    stop("Filtered health facility table is missing. Run the filter_hf function.")
  }
  scenario <- stringr::str_extract(scenarioDirs, "scenario[0-9]{3}$")
  if (length(scenario) > 1) {
    scenario <- c(scenario, "VIEW")
    subInd <- utils::menu(scenario, title = "\nSelect the scenario or the VIEW option to see the selected HFs for each scenario.")
    while (subInd == length(scenario)) {
      for (i in 1:(length(scenario)-1)) {
        message(scenario[i])
        cat(paste(readLines(paste(pathFacilities, scenario[i], "selected_hf.txt", sep = "/")), collapse = "\n"))
        readline(prompt="Press [enter] to continue")
      }
      subInd <- utils::menu(scenario, title = "Select the scenario or the VIEW option to see the selected HFs for each scenario.")
    }
    scenario <- scenarioDirs[subInd]
  } else {
    scenario <- scenarioDirs
  }
  return(scenario)
}
