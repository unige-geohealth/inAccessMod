devtools::load_all(".")
devtools::install_local(".")
library(inAccessMod)
mainPath <- getwd()
mainPath <- "C:/Users/timoner/Documents/GeoHealth/Scripts/Tests/inAccessMod"
mainPath <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/"

location <- "Central_African_Republic"
location <- "Mali"

copy_input(mainPath, location, "C:/Users/timoner/Documents/GeoHealth/HeRAMS/MALI/data/vBorders/20230130144116/raw")
copy_input(mainPath, location, "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Mali/herams_shp/shp_article/")
initiate_project(mainPath)

initiate_project(mainPath, allowInteractivity = FALSE, city = TRUE, name = "Zurich")
location <- "Bern"
location <- "Switzerland"
location <- "United_States"
download_boundaries(mainPath, location, adminLevel = 1, type = "gbOpen")
set_projection(mainPath, location, mostRecent = TRUE, alwaysSet = TRUE, bestCRS = FALSE, allowInteractivity = FALSE)
download_dem(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)
download_population(mainPath, location, allowInteractivity = FALSE)
download_landcover(mainPath, location, alwaysDownload = TRUE, mostRecent = TRUE)
download_osm(mainPath, location, "roads", alwaysDownload = TRUE, mostRecent = TRUE, defaultClasses = TRUE)
# download_osm(mainPath, location, "waterLines", alwaysDownload = TRUE, mostRecent = TRUE, defaultClasses = TRUE)
# download_osm(mainPath, location, "waterPolygons", alwaysDownload = TRUE, mostRecent = TRUE, defaultClasses = TRUE)
process_inputs(mainPath, location, "vBorders", mostRecent = FALSE, 
               alwaysProcess = TRUE, 
               defaultMethods = FALSE, 
               changeRes = FALSE, 
               popCorrection = TRUE, 
               gridRes = 2000, 
               allowInteractivity = FALSE)

label_landcover(mainPath, location, mostRecent = TRUE, overwrite = TRUE, defaultLabels = TRUE)
compile_processed_data(mainPath, location, mostRecent = TRUE)


wd <- getwd()

location <- "Bern"
#iso3string <- "CHE"

if (file.exists(location)) {
  unlink(normalizePath(location), recursive = TRUE)
}


initiate_project(
  wd,
  city = TRUE,
  name = location,
  #iso = iso3string,
  allowInteractivity = FALSE
)

download_boundaries(mainPath, location, type = "gbOpen", adminLevel = 1)


process_inputs(mainPath, location, "vFacilities/scenario001", mostRecent = TRUE, alwaysProcess = TRUE, defaultMethods = TRUE, changeRes = FALSE, popCorrection = TRUE, gridRes = 2000, allowInteractivity = FALSE)

pathCode <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Central_African_Republic/herams_data/2024-05-27/20240527_code.csv"
pathTxt <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Central_African_Republic/herams_data/2024-05-27/20240527_txt.csv"

pathCode <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Mali/herams_data/20240716 115612_code.csv"
pathTxt <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Mali/herams_data/20240716 114615_txt.csv"

HeRAMS_filter_hf(mainPath, location, pathTableCode = pathCode, pathTableText = pathTxt, mostRecentObs = TRUE, type = TRUE, ownership = TRUE)
HeRAMS_create_hf_shapefile(mainPath, location, scenario = "002")
# download_dem(mainPath, country)
download_landcover(mainPath, country)
test_that("test download_dem", {
  expect_equal(download_dem(mainPath, country, alwaysDownload = TRUE, mostRecent = TRUE), TRUE)
})

mainPath <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS"
country <- "Ukraine"
pathCode<- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Ukraine/HeRAMS_Data/herams_0204_code 2.csv"
pathTxt <- "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Ukraine/HeRAMS_Data/herams_0204_text 2.csv"
HeRAMS_filter_hf(mainPath, country, pathTableCode = pathCode, pathTxt, mostRecentObs = TRUE)

copy_input(mainPath, country, "C:/Users/timoner/OneDrive - unige.ch/HeRAMS/Ukraine/data_unoccupied_area/admin1")

HeRAMS_create_hf_shapefile(mainPath, country, mostRecentBoundaries = FALSE, scenario = "002")

process_inputs(mainPath, location, selectedInputs = "vFacilities/scenario002", defaultMethods = TRUE)

install.packages("testthat")
library(testthat)
test_that("function outputs expected results", {
  expect_equal(1 + 1, 2)
  expect_equal(1 + 13, 14)
})

interactive_function <- function() {
  response <- readline(prompt="Please provide a number: ")
  as.integer(response) * 2
}

# Test could look like this
test_that("interactive function behaves as expected with the mock input", {
  with_mock(
    readline = function(prompt) {
      return("7")
    },
    {
      expect_equal(interactive_function(), 14)
    }
  )
})

