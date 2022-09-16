# library(terra)
# 
# mainPath <- "C:/Users/timoner/Documents/GeoHealth/HeRAMS/Switzerland/Export"
# popRaster <- "raster_population_population.img"
# allocRaster <- "raster_cost_allocation_travel_time.img"
# facilityLayer <- "vector_facility_facility2"
# zoneLayer <- "vector_zone_for_stat_borders"
# 
# popRaster <- raster::raster(paste(mainPath, popRaster, sep = "/"))
# allocRaster <- raster::raster(paste(mainPath, allocRaster, sep = "/"))
# # summary(raster::getValues(allocRaster))
# facilityLayer <- sf::st_read(mainPath, facilityLayer)
# 
# popPoints <- raster::rasterToPoints(popRaster, spatial = TRUE)
# colnames(popPoints@data) <- "pop"
# dt <- data.table::as.data.table(popPoints@data)
# 
# dt[, "facilityID"] <- raster::extract(allocRaster, popPoints)
# 
# facilityDt <- data.table::as.data.table(facilityLayer)
