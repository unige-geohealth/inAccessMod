#' HeRAMS stop filtering
#'
#' Internal function to retrieve the values that stop the questionnaire
#' @keywords internal
#' @export
HeRAMS_stop_filtering <- function() {
  list(
    Health_facility_status = "A2|A3",
    Building_condition = "A3",
    Functionality = "A3",
    Accessibility = "A3"
  )
}