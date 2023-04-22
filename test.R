# Test
devtools::load_all(".")
mainPath <- "C:/Test"
initiate_project(mainPath)
country <- "Mozambique"
download_boundaries(mainPath, country, adminLevel = 1, alwaysDownload = TRUE)
subset_regions(mainPath, country)
set_projection(mainPath, country)
download_dem(mainPath, country)
download_landcover(mainPath, country)
download_osm("roads", mainPath, country)
download_population(mainPath, country)
process_inputs(mainPath, country)

