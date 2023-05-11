# Test
devtools::load_all(".")
mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/"
initiate_project(mainPath)
country <- "Iraq"
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

raster_files <- list.files("C:/Users/timoner/Documents/GeoHealth/HeRAMS//Mozambique/data/rPopulation/20230508143129/raw", full.names = TRUE)
library(raster)

# create a list of raster file names
raster_files <- c("file1.tif", "file2.tif", "file3.tif")

# load the rasters from the files and stack them
raster_stack <- stack(raster_files)

# calculate the sum of all rasters in the stack
raster_sum <- sum(raster_stack)

# view the resulting raster
plot(raster_sum)
raster::writeRaster(raster_sum, "C:/Users/timoner/Documents/GeoHealth/HeRAMS//Mozambique/data/rPopulation/20230508143129/raw/rPopSum15_45.tif")
