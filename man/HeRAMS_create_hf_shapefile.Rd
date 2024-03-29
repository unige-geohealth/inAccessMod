% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HeRAMS_create_hf_shapefile.R
\name{HeRAMS_create_hf_shapefile}
\alias{HeRAMS_create_hf_shapefile}
\title{Create A Health Facility Point Shapefile (HeRAMS)}
\usage{
HeRAMS_create_hf_shapefile(
  mainPath,
  country,
  mostRecentBoundaries = TRUE,
  lonlat = TRUE,
  epsg = NULL,
  rmNA = NULL,
  rmOut = NULL,
  scenario = NULL,
  nameCSV = NULL
)
}
\arguments{
\item{mainPath}{character; the parent directory of the country folder}

\item{country}{character; the country folder name}

\item{mostRecentBoundaries}{logical; should the most recent processed boundary shapefile be used? If FALSE and if there are multiple
available inputs, the user is interactively asked to select the input based on file creation time.}

\item{lonlat}{logical; are the coordinates indicated in the health facility table given in lon/lat?}

\item{epsg}{numeric or character (coerced to character); ESPG code - Coordinate systems worldwide (EPSG/ESRI)}

\item{rmNA}{logical; should the health facilities with non-available coordinates be removed? If NULL or FALSE the user is interactively
asked whether they want to remove them or not.}

\item{rmOut}{logical; should the health facilities falling outside of the country be removed? If NULL or FALSE the user is 
interactively asked whether they want to remove them or not.}

\item{scenario}{character; a string of three characters that correspond to the scenario folder suffix like '001', '002'...'010'...'099'...'100'
If NULL, the user is interactively asked to choose the scenario from the available ones.}

\item{nameCSV}{character; name of csv file WITHOUT extension corresponding to filtered facilities. If null, it will take the default name used in 
the HeRAMS_filter_hf function (health_facilities.csv).}
}
\description{
Create a point shapefile of health facilities based on a pre-processed HeRAMS health facility table obtained with the
\code{HeRAMS_filter_hf} function.
}
\details{
Once the missing coordinate issue is addressed, the function checks whether the health facilities fall within the
country boundary. There is a track record of both the facilities with missing coordinates and the ones that fall
outside the country boundary.
}
\examples{
# Replace workDir with the actual path to your working directory
\dontrun{
mainPath <- "workDir"
initiate_project(mainPath)}

# Replace myCountry with the country name you are working on (workDir subfolder)
\dontrun{
country <- "myCountry"
download_boundaries(mainPath, country, adminLevel = 1, type = "gbOpen", alwaysDownload = TRUE)}

# Replace myHeRAMScodeTable with the path of the HeRAMS table that contains codes; set to NULL to use example data
# Replace myHeRAMStextTable with the path of the HeRAMS table that contains text; set to NULL to use example data
\dontrun{
pathTableCode <- "myHeRAMScodeTable"
pathTableText <- "myHeRAMStextTable"
HeRAMS_filter_hf(mainPath, country, pathTableCode, pathTableText, barriers = FALSE, mostRecentObs = TRUE)
HeRAMS_create_hf_shapefile(mainPath, country, mostRecentBoundaries = TRUE, lonlat = TRUE, rmNA = TRUE, rmOut = TRUE, scenario = NULL)} 
}
