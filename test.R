# Test

devtools::load_all(".")
mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/"
initiate_project(mainPath)
country <- "MOZAMBIQUE"
download_boundaries(mainPath, country, adminLevel = 3, type = "gbOpen", alwaysDownload = TRUE)
subset_regions(mainPath, country, mostRecent = FALSE)
set_projection(mainPath, country)
download_dem(mainPath, country)
download_landcover(mainPath, country)
download_osm("roads", mainPath, country)
download_osm("waterLines", mainPath, country)
download_osm("naturalPolygons", mainPath, country)
download_population(mainPath, country)
check_inputs(mainPath, country, "raw")


process_inputs(mainPath, country, defaultMethods = TRUE)

files <- c("C:/Users/timoner/Documents/GeoHealth/HeRAMS/MOZAMBIQUE/data/zToAccessMod/multi_ts/rLandcover_merge.img",
           "C:/Users/timoner/Documents/GeoHealth/HeRAMS/MOZAMBIQUE/data/zToAccessMod/multi_ts/rLandcover_merge.img.aux.xml")
copy_input(mainPath, country, files)

pathTableCode <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/MOZAMBIQUE_backup/Facilities/MOZ_code.csv"
pathTableText <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/MOZAMBIQUE_backup/Facilities/MOZ_text_en.csv"
HeRAMS_filter_hf(mainPath, country, pathTableCode, pathTableText, region = TRUE)
