

pathFacilities <- paste0(mainPath, "/", region, "/data/vFacilities")
if (!dir.exists(paste0(pathFacilities))) {
  stop(paste(pathFacilities, " does not exist. Run the initiate_project function."))
}
newTib <- tryCatch({readxl::read_excel(pathTable, skip = 1, sheet = 2)}, error = function(e){NULL})
rr <- rast("C:/Users/timoner/Documents/GeoHealth/HeRAMS/Switzerland/data/rDEM/20220524153936/raw/srtm.tif")
shp <- get_boundaries(mainPath, region, "raw")

rr <- terra::crop(rr, shp)
rr <- terra::mask(rr, as(shp, "SpatVector"))
plot(rr)
pts <- randomPoints(raster(rr), nrow(newTib))
newTib$external_id <- as.character(1:nrow(newTib))
newTib$workspace_id <- "Switzerland"
tid <- table(newTib$subject_id)
for (i in 1:length(tid)) {
  tt <- tid[i]
  newId <- do.call(paste0, Map(stri_rand_strings, n=1, length=c(2, 2, 1),
                      pattern = c('[A-Z]', '[0-9]', '[A-Z]')))
  newTib[newTib$subject_id == names(tt), "subject_id"] <- rep(newId, tt)
}
newTib$MoSD1 <- NA

newTib[newTib$MoSD3 == "Garrison Infirmary", "MoSD3"] <- "Infirmary"
newTib[grepl("Inter", newTib$MoSD3), "MoSD3"] <- "Medical Center"
newTib[grepl("Community", newTib$MoSD3), "MoSD3"] <- "Polyclinic"
newTib[grepl("Reference", newTib$MoSD3), "MoSD3"] <- "Military Hospital"

tid <- table(newTib$HFNAME)
for (i in 1:length(tid)) {
  tt <- tid[i]
  newId <- gsub(",", "", randomNames(1, ethnicity = 5))
  newTib[newTib$HFNAME == names(tt), "HFNAME"] <- rep(newId, tt)
}

newTib$MoSD2 <- newTib$HFNAME
newTib$MoSD6 <- NA
newTib$MosD7a <- NA
newTib$GEO1 <- "Switzerland"
newTib$GEO3 <- NA
newTib$GEO5 <- NA
newTib$GEO6 <- NA
na_ind <- which(is.na(newTib$MoSDGPS_SQ001))
newTib$MoSDGPS_SQ001 <- pts[,1]
newTib$MoSDGPS_SQ002 <- pts[,2]
newTib[na_ind, c("MoSDGPS_SQ001","MoSDGPS_SQ002")] <- NA

newTib$MoSDPOP <- NA
newTib$HFCONTACT_NAME <- NA
newTib$HFCONTACT_FUNCT <- NA
newTib$HFCONTACT_PHONE <- NA
newTib$HFCONTACT_EMAIL <- NA

newTib <- cbind(newTib[1,], newTib)


usethis::use_data(newTib, random_herams_data, overwrite = TRUE)
shp <- as(shp, "Spatial")
spPts <- SpatialPoints(pts, proj4string = shp@proj4string)
plot(shp)
plot(spPts, add=TRUE)
