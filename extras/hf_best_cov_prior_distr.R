library(sf)
library(exactextractr)
library(terra)

# D'abord on regard les districts prioritaires (ordre shp). Clip popuplation. Intersect catchments et district prioritaire là ou il y a
# au moins une personne. 
# Ensuite procédure standard jusqu'à ce que la couverture souhaitée est atteinte ou nTot.

# Ensuite district suivant (utilisation des catchments originaux, mais pas forcément nécessaire vu que les parties ôtées n'ont pas d'incidence
# sur les nouveaux districs (pas d'overlap))

# Possible problème: on a un ordre pour les priority districts et si on a un nTot trop bas, certains districs peut être en désavantage par rapport
# à d'autres; nTot pas nécessaire (écononomise temps de calcul, idem pour hf_best_cov)

# A la fin un tableau: par district les meilleurs catchm et taux de couverture
# A la fin on calcule aussi la couverture à l'échelle nationale; même procedure que la procédure standard, mais on force avec la sélection des catch
# choisis dans les districts prioritaires; barplot (petit bosse initiales (districts prioritaires), ensuite augmentation si nTot > catch sélectionnés pour 
# les districts prioritaires)

hf_best_cov_priority <- function (workDir, catchShp, popRaster, priorityShp, catchHfColName, nTot, adminColName, priorityPer = 60) 
{
  #### LAYER INITIALIZATION ####
  message("Layer initialization...")
  #Init shapefile_cathchments
  catchShp <- file.path(workDir, paste0(catchShp, ".shp"))
  catchShp <- sf::st_read(catchShp, quiet = TRUE)
  
  #Init population raster
  pop <- file.path(workDir, popRaster)
  pop <- terra::rast(pop)
  
  #Overall total population calculated
  pop[is.na(pop)] <- 0
  overall_tot_pop <- terra::global(pop, fun = "sum")
  
  #Init priority district
  priorityShp <- file.path(workDir, paste0(priorityShp, ".shp"))
  priorityShp <- sf::st_read(priorityShp, quiet = TRUE)
  
  #Final Table creation
  finalTable <- data.frame(matrix(ncol = 4, nrow = nTot))
  names(finalTable) <- c("Facility name", "Population covered", 
                         "District","Percentage in country")
  
  #Total Population table creation
  populationTotDistrict <- data.frame(matrix(ncol = 2, nrow = length(st_drop_geometry(priorityShp)[,1])))
  names(populationTotDistrict) <- c("District","Total Population")
  populationTotDistrict$District <- st_drop_geometry(priorityShp[,adminColName])[,1]
  
  #### IDENTICAL CATCHMENT CHECK ####
  message("Identical catchment check...")
  catchShp$totalpop <- exactextractr::exact_extract(pop, catchShp, 
                                                    "sum", progress = FALSE)
  similarCatch <- catchShp[as.character(catchShp$totalpop) %in% 
                             names(table(catchShp$totalpop)[table(catchShp$totalpop) > 
                                                              1]), ]
  simi <- similarCatch
  simi$grp <- NA
  j <- 0
  i <- nrow(simi)
  bar <- txtProgressBar(min = 0, max = i, initial = 0, char = "=", 
                        width = NA, style = 3, file = "")
  while (i > 1) {
    x <- sf::st_equals(similarCatch$geometry[1], similarCatch$geometry, 
                       sparse = FALSE)
    if (any(x)) {
      j <- j + 1
      hfIdent <- sf::st_drop_geometry(similarCatch[, catchHfColName])[x, 
                                                                      1]
      ind <- which(sf::st_drop_geometry(simi[, catchHfColName])[, 
                                                                1] %in% hfIdent)
      simi$grp[ind] <- j
      similarCatch <- similarCatch[-which(x), ]
    }
    else {
      similarCatch <- similarCatch[-1, ]
    }
    i <- nrow(similarCatch)
    setTxtProgressBar(bar, nrow(simi) - i)
  }
  close(bar)
  similarCatch <- sf::st_drop_geometry(simi)
  similarTable <- data.frame()
  grp <- unique(similarCatch$grp)
  grp <- grp[order(grp)]
  for (i in 1:length(grp)) {
    similarTable[i, "Group"] <- grp[i]
    similarTable[i, "Health Facilities"] <- paste(similarCatch[similarCatch$grp == 
                                                                 grp[i], catchHfColName], collapse = " // ")
    similarTable[i, "Population"] <- similarCatch[similarCatch$grp == 
                                                    grp[i], "totalpop"][1]
  }
  UniquecatchShp <- catchShp[!duplicated(catchShp$geometry),]
  close(bar)
  
  #### DISTRICT POPULATION CALCULATION ####
  # Pour avoir ensuite les couvertures
  message("District population calculation...")
  #For each priority district, calculate the total population
  CALC_tot_pop <- rep(NA,length(st_drop_geometry(priorityShp)[,1]))
  priorityShp <- cbind(priorityShp,CALC_tot_pop)
  i = 1
  for (i in 1:nrow(priorityShp)){
    curr_priority_pop_value <- exactextractr::exact_extract(pop, priorityShp[i,], "sum", progress = FALSE)
    priorityShp$CALC_tot_pop[i] <- curr_priority_pop_value
  }
  
  #### GET PRIORITY BEST CATCHMENTS ####
  message("Priority district analysis...")
  UniquecatchShp$totalpop <- exactextractr::exact_extract(pop,UniquecatchShp,"sum", progress = FALSE)
  UniquecatchShp$district <- rep(NA,length(UniquecatchShp[,1]))
  UniquecatchShp$totalpop_dist <- rep(NA,length(UniquecatchShp[,1]))
  # Keep table structure (empty)
  SelectedCatch <- UniquecatchShp[FALSE,]
  Uniquecatchbackup <- UniquecatchShp
  
  i = 1
  # Test nTot
  y = 0
  for (i in 1:nrow(priorityShp)){
    UniquecatchShp <- Uniquecatchbackup
    #For each district of interest
    message(paste0("Currently checking ",st_drop_geometry(priorityShp[,adminColName])[i,1],"..."))
    
    #Clip the population raster to the district of interest and store it.
    priority_pop_raster <- terra::crop(pop, terra::vect(priorityShp[i,]), mask = TRUE)
    
    #Replace the NA values
    priority_pop_raster[is.na(priority_pop_raster)] <- 0
    
    #Extract the population covered in the priority district
    UniquecatchShp$totalpop_dist <- exactextractr::exact_extract(priority_pop_raster,UniquecatchShp,"sum", progress = FALSE)
    
    #Add the name of the district in the catchment shapefile (later on subset of UniquecatchShp)
    UniquecatchShp$district <- st_drop_geometry(priorityShp[,adminColName])[i,1]
    
    #Calculate the total population int the district
    total_pop_in_district <- priorityShp$CALC_tot_pop[i]
    
    #Add the total population to the reference table
    populationTotDistrict$`Total Population`[populationTotDistrict$District == st_drop_geometry(priorityShp[,adminColName])[i,1]] <- total_pop_in_district
    
    
    #Calculate the population coverage threshold
    cover_threshold <- priorityPer/100 * total_pop_in_district
    
    #Remove the cathcments that already have zero population (pop only in the priority district)
    UniquecatchShp <- UniquecatchShp[UniquecatchShp$totalpop_dist > 0,]
    
    
    curr_coverage = 0
    
    while((y < nTot) && (nrow(UniquecatchShp) > 0) && (curr_coverage < cover_threshold)){
      
      UniquecatchShp$totalpop_dist <- exactextractr::exact_extract(priority_pop_raster,UniquecatchShp,"sum", progress = FALSE)
      # We test again because in this loop, population is removed
      UniquecatchShp <- UniquecatchShp[UniquecatchShp$totalpop_dist > 0,]
      indMax <- which(UniquecatchShp$totalpop_dist == max(UniquecatchShp$totalpop_dist))
      
      if(length(indMax) > 1){
        indMax <- indMax[which(UniquecatchShp$totalpop_dist[indMax] == max(UniquecatchShp$totalpop_dist[indMax]))][1]
      }
      
      top <- UniquecatchShp[indMax,]
      
      SelectedCatch <- rbind(SelectedCatch, top)
      
      UniquecatchShp <- UniquecatchShp[-indMax,]
      
      # Check intersections and difference
      if (nrow(UniquecatchShp) > 0) {
        for (k in 1:nrow(UniquecatchShp)) {
          if (suppressWarnings(sf::st_intersects(UniquecatchShp[k, 
          ], top, sparse = FALSE)[1, 1])) {
            stock <- suppressWarnings(sf::st_difference(UniquecatchShp[k, 
            ], top))
            stock <- stock[, match(colnames(UniquecatchShp), 
                                   colnames(stock))]
            if (nrow(stock) == 0) {
              UniquecatchShp$totalpop_dist[k] <- 0
            }
            else {
              UniquecatchShp[k, ] <- stock
            }
          }
        }
      }
      if(y >= nTot){message(paste0("Max number of catchment reached in ",st_drop_geometry(priorityShp[,adminColName])[i,1]))}
      if(nrow(UniquecatchShp) <= 0){message(paste0("No more catchments to choose from in ",st_drop_geometry(priorityShp[,adminColName])[i,1]))}
      # Test current coverage
      curr_coverage <- sum(SelectedCatch$totalpop_dist[SelectedCatch$district == st_drop_geometry(priorityShp[,adminColName])[i,1]])
      y <- y+1
    }
  }
  
  priority_table <- st_drop_geometry(SelectedCatch)
  cumulative_pop <- rep(NA,length(priority_table$name))
  percentage_pop <- rep(NA,length(priority_table$name))
  priority_table <- cbind(priority_table,cumulative_pop,percentage_pop)
  priority_table <- priority_table[,c(catchHfColName,"district","totalpop_dist","cumulative_pop","percentage_pop")]
  #Cumulative sum calculation
  for (m in levels(as.factor(priority_table$district))){
    count <- 0
    for(g in 1:nrow(priority_table)){
      if(priority_table$district[g] == m){
        if (count == 0){
          priority_table$cumulative_pop[g] <- priority_table$totalpop_dist[g]
          count <- 1
        }
        else{
          priority_table$cumulative_pop[g] <- priority_table$cumulative_pop[g-1] + priority_table$totalpop_dist[g]
        }
      }
    }
  }
  
  #Coverage percentage calculation per catch at the district level
  for (z in 1:nrow(priority_table)){
    curr_district <- priority_table$district[z]
    priority_table$percentage_pop[z] <- (priority_table$cumulative_pop[z]*100)/(populationTotDistrict$`Total Population`[populationTotDistrict$District == curr_district][[1]])
  }
  
  #### Recalculate priority catchments coverage :
  # Coverage at the national level
  message("Priority catchments recalculation...")
  bar <- txtProgressBar(min = 0, max = nrow(finalTable), initial = 0, char = "=", width = NA, style = 3, file = "")
  UniquecatchShp <- Uniquecatchbackup
  SelectedCatch$totalpop <- exactextractr::exact_extract(pop,SelectedCatch,"sum", progress = FALSE)
  i <- 0
  while(nrow(SelectedCatch) > 0){
    if(i > 0){
      SelectedCatch$totalpop <- exactextractr::exact_extract(pop,SelectedCatch,"sum", progress = FALSE)
    }
    
    indMax <- which(SelectedCatch$totalpop == max(SelectedCatch$totalpop))
    if (length(indMax) > 1) {
      indMax <- indMax[which(SelectedCatch$totalpop[indMax] == 
                               max(SelectedCatch$totalpop[indMax]))][1]
    }
    i <- i + 1
    finalTable[i, "Facility name"] <- sf::st_drop_geometry(SelectedCatch[indMax, 
                                                                         catchHfColName])[1, 1]
    finalTable[i, "Population covered"] <- SelectedCatch$totalpop[indMax]
    finalTable[i, "District"] <- sf::st_drop_geometry(SelectedCatch[indMax,"district"])[1, 1]
    finalTable[i, "Percentage in country"] <- ((sf::st_drop_geometry(SelectedCatch)[indMax, "totalpop"])/overall_tot_pop)*100
    
    
    # Select the best covering catchment from the priority catchments
    top <- SelectedCatch[indMax, ]
    # Remove the best covering catchment from the priority cathchments
    SelectedCatch <- SelectedCatch[-indMax, ]
    # Remove it from the unique catchment list
    UniquecatchShp <- UniquecatchShp[!(top$name == st_drop_geometry(UniquecatchShp)[,catchHfColName]),]
    #While there still are rows in Selected Catch
    # First we check for the intersections within the selected catchments
    if (nrow(SelectedCatch) > 0) {
      #For each chosen catchment
      for (k in 1:nrow(SelectedCatch)) {
        if (suppressWarnings(sf::st_intersects(SelectedCatch[k,], top, sparse = FALSE)[1, 1])) {
          #Store the difference between the two in the SelectecCatch layer
          stock <- suppressWarnings(sf::st_difference(SelectedCatch[k, 
          ], top))
          stock <- stock[, match(colnames(SelectedCatch), 
                                 colnames(stock))]
          
          #If the differrence is null, set the population to 0.
          if (nrow(stock) == 0) {
            SelectedCatch$totalpop[k] <- 0
          }
          #Else replace it by the difference.
          else {
            SelectedCatch[k, ] <- stock
          }
        }
        UniquecatchShp <- UniquecatchShp[UniquecatchShp$totalpop > 0,]
        SelectedCatch <- SelectedCatch[SelectedCatch$totalpop > 0,]
        setTxtProgressBar(bar, ceiling( which(apply(finalTable, 1, function(x) all(is.na(x))))[1]))
      }
      # And here we check for the intersections taking into account all the catchments (the remaining ones)
      for (k in 1:nrow(UniquecatchShp)) {
        if (suppressWarnings(sf::st_intersects(UniquecatchShp[k,], top, sparse = FALSE)[1, 1])) {
          #Store the difference between the two in the UniquecatchShp layer
          stock2 <- suppressWarnings(sf::st_difference(UniquecatchShp[st_drop_geometry(UniquecatchShp)[,catchHfColName] == st_drop_geometry(UniquecatchShp)[k,catchHfColName],],top))
          stock2 <- stock2[, match(colnames(UniquecatchShp),colnames(stock2))]
          
          #If the differrence is null, set the population to 0.
          if (nrow(stock2) == 0){
            UniquecatchShp$totalpop[st_drop_geometry(UniquecatchShp)[,catchHfColName] == st_drop_geometry(UniquecatchShp)[k,catchHfColName]] <- 0
          }
          #Else replace it by the difference.
          else {
            UniquecatchShp[st_drop_geometry(UniquecatchShp)[,catchHfColName] == st_drop_geometry(UniquecatchShp)[k,catchHfColName],] <- stock2
          }
        }
        UniquecatchShp <- UniquecatchShp[UniquecatchShp$totalpop > 0,]
        SelectedCatch <- SelectedCatch[SelectedCatch$totalpop > 0,]
        setTxtProgressBar(bar, ceiling( which(apply(finalTable, 1, function(x) all(is.na(x))))[1]))
      }
    }
    
  }
  
  close(bar)
  #### Add remaining catchments :
  message("Addition of other catchments in the rest of the country...")
  # Standard procedure (hf_best_cov)
  #Continue from the moment that their are only NAs
  y = which(apply(finalTable, 1, function(x) all(is.na(x))))[1]
  bar <- txtProgressBar(min = 0, max = nrow(finalTable), initial = 0, char = "=", width = NA, style = 3, file = "")
  txtProgressBar(min = 0, max = nrow(finalTable), initial = 0, char = "=", width = NA, style = 3, file = "")
  
  while(y < nTot & nrow(UniquecatchShp) > 0){
    
    UniquecatchShp$totalpop <- exactextractr::exact_extract(pop,UniquecatchShp,"sum", progress = FALSE)
    
    indMax <- which(UniquecatchShp$totalpop == max(UniquecatchShp$totalpop))
    
    if(length(indMax) > 1){
      indMax <- indMax[which(UniquecatchShp$totalpop[indMax] == max(UniquecatchShp$totalpop[indMax]))][1]
    }
    
    finalTable[y, "Facility name"] <- sf::st_drop_geometry(UniquecatchShp[indMax,catchHfColName])[1, 1]
    finalTable[y, "Population covered"] <- UniquecatchShp$totalpop[indMax]
    finalTable[y, "District"] <- "Non-priority"
    finalTable[y, "Percentage in country"] <- ((sf::st_drop_geometry(UniquecatchShp)[indMax, "totalpop"])/overall_tot_pop)*100
    
    top <- UniquecatchShp[indMax,]
    UniquecatchShp <- UniquecatchShp[-indMax,]
    
    if (nrow(UniquecatchShp) > 0) {
      for (k in 1:nrow(UniquecatchShp)) {
        if (suppressWarnings(sf::st_intersects(UniquecatchShp[k, 
        ], top, sparse = FALSE)[1, 1])) {
          stock <- suppressWarnings(sf::st_difference(UniquecatchShp[k, 
          ], top))
          stock <- stock[, match(colnames(UniquecatchShp), 
                                 colnames(stock))]
          if (nrow(stock) == 0) {
            UniquecatchShp$totalpop[k] <- 0
          }
          else {
            UniquecatchShp[k, ] <- stock
          }
        }
      }
    }    
    y <- y+1
    setTxtProgressBar(bar, ceiling( which(apply(finalTable, 1, function(x) all(is.na(x))))[1]))
    
    UniquecatchShp <- UniquecatchShp[UniquecatchShp$totalpop > 0,]
  }
  close(bar)
  
  ####Final Table formatting
  colNamesFT <- colnames(finalTable)
  finalTable <- finalTable[complete.cases(finalTable),]
  
  for (i in 1:nrow(finalTable)) {
    finalTable$Rank[i] <- i
    finalTable$cumul[i] <- sum(finalTable[, "Population covered"][1:i])
  }
  
  for (i in 1:nrow(finalTable)) {
    finalTable$perc_cumul[i] <- sum(finalTable[, "Percentage in country"][1:i])
  }
  
  finalTable <- finalTable[, c("Rank", colNamesFT, "cumul","perc_cumul")]
  colnames(finalTable)[ncol(finalTable)-1] <- "Cumulative sum"
  colnames(finalTable)[ncol(finalTable)] <- "Cumulative total covered percentage"
  
  for (i in 1:nrow(finalTable)) {
    ind <- grepl(finalTable$`Facility name`[i], similarTable$`Health Facilities`)
    if (any(ind)) { 
      if (sum(ind) > 1) {
        stop(paste("Health facility in multiple groups:", 
                   finalTable$`Facility name`[i]))
      }
      finalTable$`Facility name`[i] <- similarTable$`Health Facilities`[ind]
    }
  }
  outFolder <- file.path(workDir, "outputs", format(Sys.time(), 
                                                    "%Y%m%d%H%M%S"))
  dir.create(outFolder, showWarnings = FALSE, recursive = TRUE)
  write.csv(finalTable, file.path(outFolder, "country_scale_facilities_table.csv"), 
            row.names = FALSE)
  
  ##Write the district csv
  write.csv(priority_table, file.path(outFolder, "district_scale_facilities_table.csv"),row.names = FALSE)
  
  cat(paste("Calculations completed./nOutputs can be found under:", 
            outFolder))
}

##Test
nTot <- 160
priorityPer <- 60

#walking 1h 60%
{
  workDir <- "C:/Users/timoner/Documents/GeoHealth/Loic/test_data/"
  catchShp <- "shape_catchment_accessibility_ALL_HF_FOR_SCRIPT_TEST"
  popRaster <- "adj_women_only_rural.tif"
  priorityShp <- "3_districts_border"
  catchHfColName <- "name"
  adminColName <- "ADM1_EN"
  hf_best_cov_priority(workDir, catchShp, popRaster, priorityShp, catchHfColName, adminColName, nTot = nTot, priorityPer = priorityPer)
}

