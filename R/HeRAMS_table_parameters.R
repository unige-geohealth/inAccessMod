#' HeRAMS table parameters
#'
#' Internal function to retrieve a list of the variable codes for filtering the HeRAMS table
#' @export
HeRAMS_table_parameters <- function() {
  list(
    Health_facility_type = "MoSD3", 
    Ownership = "MoSD7",
    Health_facility_status = "MoSD4", 
    Building_condition = "CONDB", 
    Equipment_condition = "CONDE",
    Functionality = "HFFUNCT",
    Accessibility = "HFACC",
    Partner_support = "HFSUP1",
    Partners = "HFSUP3_SQ[0-9]{3}",
    Services = "QHeRAMS[0-9]{3}",
    Barrier_suffix = "x_[0-9]"
  )
}
