#' Select HeRAMS Scenario Folder (internal function)
#'
#' @param scenario character; Scenario folder
#' @export
select_scenarioTime <- function (scenario) {
  scenarioTime <- list.dirs(scenario, recursive = FALSE)
  scenarioTime <- scenarioTime[grepl("[0-9]{14}", scenarioTime)]
  scenarioTime <- stringr::str_extract(scenarioTime, "[0-9]{14}")
  if (length(scenarioTime) > 1) {
    scenarioTimeForm <- paste0(substr(scenarioTime, 1, 4), "-", substr(scenarioTime, 5, 6), "-", substr(scenarioTime, 7, 8), " ", substr(scenarioTime, 9, 10), ":", substr(scenarioTime, 11, 12), ":", substr(scenarioTime, 13, 14), " CEST")
    scenarioTimeForm <- c(scenarioTimeForm, "VIEW")
    subInd <- utils::menu(scenarioTimeForm, title = "Select the filtered table time creation or the VIEW option to see the selection parameters for each time.")
    while (subInd == length(scenarioTimeForm)) {
      for (i in 1:(length(scenarioTimeForm)-1)) {
        message(scenarioTimeForm[i])
        cat(paste(readLines(paste(scenario, scenarioTime[i], "time_frame.txt", sep = "/")), collapse = "\n"))
        readline(prompt="Press [enter] to continue")
      }
      subInd <- utils::menu(scenarioTimeForm, title = "Select the filtered table time creation or the VIEW option to see the selection parameters for each time.")
    }
    scenarioTime <- scenarioTime[subInd]
  }
  return(scenarioTime)
}
