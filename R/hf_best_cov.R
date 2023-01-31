#' hf_best_cov
#'
#' Select the health facilities that offer the best population coverage using their 
#' catchments and a raster of population. This function also offers the possibility to ensure that N 
#' facilities are selected for each administrative unit.
#' @param workDir character; working directory that contains the inputs
#' @param catchShp character; name of the catchment shapefile WITHOUT extension
#' @param popRaster character; name of the population raster file WITH extension
#' @param hfShp character; name of the health facility shapefile WITHOUT extension
#' @param hfHfColName character; name of the health facility name column in the health facility shapefile
#' @param catchHfColName character; name of the health facility name column in the catchment shapefile
#' @param nTot integer; number of health facilities to be selected
#' @param adminCheck logical; whether a minimum of health facilities per administrative unit must be selected. FALSE by default
#' @param npAdmin integer; minimum number of health facilities to be selected per administrative unit (required if adminCheck = TRUE)
#' @param adminShp character; name of the administrative unit shapefile WITHOUT extention (required if adminCheck = TRUE)
#' @param adminColName character; name of the administrative unit name column in the administrative unit shapefile (required if adminCheck = TRUE)
#' @export

hf_best_cov <- function (workDir, 
                      catchShp, 
                      popRaster, 
                      hfShp, 
                      hfHfColName, 
                      catchHfColName, 
                      nTot,
                      adminCheck = FALSE,
                      npAdmin = NULL,
                      adminShp = NULL,
                      adminColName = NULL) {
  
  #Loading the inputs
  message("Loading the inputs...")
  if (!dir.exists(workDir)) {
    stop(paste(workDir, "does not exist."))
  }
  if (!is.numeric(nTot)) {
    stop("nTot must be 'numeric'")
  }
  if (!is.logical(adminCheck)) {
    stop("adminCheck must be 'logical'")
  }
  tempCatch <- paste0(paste(workDir, catchShp, sep="/"), ".shp")
  if (!file.exists(tempCatch)) {
    stop(paste(tempCatch, "does not exist."))
  }
  tempCatch <- sf::st_read(paste0(paste(workDir, catchShp, sep="/"), ".shp"), quiet = TRUE)
  pop <- paste0(workDir, "/", popRaster)
  if (!file.exists(pop)) {
    stop(paste(pop, "does not exist."))
  }
  pop <- terra::rast(pop)
  hf <- paste0(paste(workDir, hfShp, sep="/"),".shp")
  if (!file.exists(hf)) {
    stop(paste(hf, "does not exist."))
  }
  hf <- sf::st_read(hf, quiet = TRUE)
  if (!hfHfColName %in% colnames(hf)) {
    stop(paste(hfHfColName, "is not a valid column name in the facility shapefile."))
  }
  if (!catchHfColName %in% colnames(tempCatch)) {
    stop(paste(catchHfColName, "is not a valid column name in the catchment shapefile."))
  }
  
  if (adminCheck) {
    if (is.null(adminShp)) {
      stop("If adminCheck = TRUE, adminShp is required.")
    }
    if (is.null(npAdmin)) {
      stop("If adminCheck = TRUE, npAdmin is required.")
    } else {
      if (!is.numeric(npAdmin)) {
        stop("npAdmin must be 'numeric'")
      }
    }
    admin <- sf::st_read(paste0(paste(workDir, adminShp, sep="/"),".shp"), quiet = TRUE)
    if (!adminColName %in% colnames(admin)) {
      stop(paste(hfHfColName, "is not a valid column name in the facility shapefile."))
    }
    nUnits <- length(unique(sf::st_drop_geometry(admin[, adminColName])[, 1]))
    if (npAdmin * nUnits > nTot) {
      stop("npAdmin * number of adminstrative units > nTot")
    }
    # Create an admin column in the catchment attribute table
    tempCatch[, adminColName] <- NA
    for (i in 1:nrow(admin)) {
      # Get the name
      adminSubName <- sf::st_drop_geometry(admin[i, adminColName])[1, 1]
      # Intersect with the facilities (points)
      hfSub <- sf::st_drop_geometry(suppressWarnings(hf[sf::st_intersects(admin[i, ], hf, sparse = FALSE), ]))
      # Add the administrative name to the catchment attribute table for the intersected facilities
      tempCatch[sf::st_drop_geometry(tempCatch[, catchHfColName])[, 1] %in% hfSub[, hfHfColName], adminColName] <- adminSubName
      # Create a table that will keep track the number of selected facilities per admin
      units <- unique(sf::st_drop_geometry(tempCatch[, adminColName])[, 1])
      hfCounts <- data.frame(admin = units, count = 0)
    }
    # Final table preparation
    finalTable <- data.frame(matrix(ncol = 3, nrow = nTot))
    names(finalTable) <- c("Facility name","Population covered", "Region")
  } else {
    # Final table preparation
    finalTable <- data.frame(matrix(ncol = 2, nrow = nTot))
    names(finalTable) <- c("Facility name","Population covered")
  }
  
  message("Checking identical catchments...")
  # Extract population for each catchment
  tempCatch$totalpop <- exactextractr::exact_extract(pop, tempCatch, "sum", progress = FALSE)
  # Catchment having same population and potentially identical (verified using st_equals)
  similarCatch <- tempCatch[as.character(tempCatch$totalpop) %in% names(table(tempCatch$totalpop)[table(tempCatch$totalpop) > 1]), ]
  simi <- similarCatch
  simi$grp <- NA
  # Group number
  j <- 0
  # Remaining rows to check
  i <- nrow(simi)
  bar <- txtProgressBar(min = 0, max = i, initial = 0, char = "=", width = NA, style = 3, file = "")
  while (i > 1) {
    # print(i)
    x <- sf::st_equals(similarCatch$geometry[1], similarCatch$geometry, sparse = FALSE)
    if (any(x)) {
      j <- j + 1
      hfIdent <- sf::st_drop_geometry(similarCatch[, hfHfColName])[x, 1]
      ind <- which(sf::st_drop_geometry(simi[, hfHfColName])[, 1] %in% hfIdent)
      simi$grp[ind] <- j
      similarCatch <- similarCatch[-which(x), ] 
    } else {
      similarCatch <- similarCatch[-1, ] 
    }
    i <- nrow(similarCatch)
    setTxtProgressBar(bar, nrow(simi) - i)
  }
  close(bar)
  # # Other way to verify if they are identical, using the centroid and area. Although faster, this way may
  # # give some errors. Indeed, although it is very unlikely we could have two different catchments with the 
  # # same population, centroid and area. Very unlikely but still possible. Should give the same result anyway (see below)
  # similarCatch <- tempCatch[as.character(tempCatch$totalpop) %in% names(table(tempCatch$totalpop)[table(tempCatch$totalpop) > 1]), ]
  # centro <- NULL
  # area <- NULL
  # for (i in 1:nrow(similarCatch)) {
  #   # x <- sf::st_equals(similarCatch$geometry[1], similarCatch$geometry)
  #   centro <- append(centro, suppressWarnings(sf::st_centroid(similarCatch[i, ])$geometry))
  #   area <- c(area, sf::st_area(similarCatch[i, ]))
  # }
  # # Add area field and replace the catchment geometry by the centro geometry
  # similarCatch <- cbind(sf::st_drop_geometry(similarCatch), area, centro)
  # # Same area and centroid means that catchment are identical
  # similarCatch$grp <- as.numeric(as.factor(paste(similarCatch$area, similarCatch$geometry)))
  # # Comparing with the st_equals method
  # sameResults <- NULL
  # for (i in 1:length(similarCatch$grp)) {
  #   x <- which(similarCatch$grp==similarCatch$grp[i])
  #   y <- which(simi$grp==simi$grp[i])
  #   sameResults <- c(sameResults, all(y == x))
  # }
  # print(all(sameResults))
  
  
  # If first method is used (st_equals)
  similarCatch <- sf::st_drop_geometry(simi)
  
  # Create a clean data frame with information on identical catchments
  similarTable <- data.frame()
  grp <- unique(similarCatch$grp)
  grp <- grp[order(grp)]
  for (i in 1:length(grp)){
    similarTable[i, "Group"] <- grp[i]
    similarTable[i, "Health Facilities"] <- paste(similarCatch[similarCatch$grp == grp[i], catchHfColName], collapse = " // ")
    similarTable[i, "Population"] <- similarCatch[similarCatch$grp == grp[i], "totalpop"][1]
  }
  
  # Keep only the catchments that are different for the main algorithm
  tempCatchUnique <- tempCatch[!duplicated(tempCatch$geometry), ]
  
  # Main algorithm
  message("Main algorithm...")
  bar <- txtProgressBar(min = 0, max = 100, initial = 0, char = "=", width = NA, style = 3, file = "")
  i <- 0
  # The algorithm runs until the expected number of facility is reached or until we don't have any facility left.
  while (i < nTot & nrow(tempCatchUnique) > 0) {
    # Only required when i > 0
    if (i > 0) {
      # In principle, not necessary anymore:
      # Convert catchments that have been fragmented in several pieces
      # for (p in 1:nrow(tempCatchUnique)){
      #   if ("sfc_GEOMETRYCOLLECTION" %in% class(tempCatchUnique$geometry[p])){
      #     inter <- sf::st_collection_extract(tempCatchUnique[p, ], "POLYGON")
      #     inter2 <- sf::st_union(inter)
      #     inter2 <- cbind(sf::st_drop_geometry(tempCatchUnique[p, ]), inter2)
      #     tempCatchUnique[p, ] <- inter2
      #   }
      # }
      # toBeRemoved <- NULL
      # for (p in 1:nrow(tempCatchUnique)) {
      #   if (st_geometry_type(tempCatchUnique[p, ]) == "MULTILINESTRING" | st_geometry_type(tempCatchUnique[p, ]) == "STRING") {
      #     toBeRemoved <- c(toBeRemoved, p)
      #   }
      #   if (!is.null(toBeRemoved)) {
      #     tempCatchUnique <- tempCatchUnique[-c(toBeRemoved), ]
      #   }
      # }
      # Calculation of the current population present in every catchment
      tempCatchUnique$totalpop <- exactextractr::exact_extract(pop, tempCatchUnique, "sum", progress = FALSE)
    }
    # Keep the one with the highest population
    # If checkAdmin
    if (adminCheck) {
      # Check which are not complete yet
      notComplete <- hfCounts$admin[which(hfCounts$count < npAdmin)]
      if (length(notComplete) == 0) {
        # If all complete, no constraint
        indMax <- which(tempCatchUnique$totalpop == max(tempCatchUnique$totalpop))
      } else {
        # Which are the admin that have priority
        tempAdmin <- sf::st_drop_geometry(tempCatchUnique[, adminColName])[, 1]
        validRows <- tempAdmin %in% notComplete
        # Look for the max among them
        indMax <- which(tempCatchUnique$totalpop == max(tempCatchUnique$totalpop[validRows]))
        # Increment the count for the selected admin
        selAdmin <- sf::st_drop_geometry(tempCatchUnique[, adminColName])[indMax, ]
        hfCounts$count[hfCounts$admin == selAdmin] <- hfCounts$count[hfCounts$admin == selAdmin] + 1
      }
    } else {
      # Max with no constraint
      indMax <- which(tempCatchUnique$totalpop == max(tempCatchUnique$totalpop))
    }
    # Usually only one catchment (same population in two non-identical catchments is very unlikely)
    # But we never knows.
    for (j in 1:length(indMax)) {
      # Increment of i
      i <- i + 1
      finalTable[i, "Facility name"] <- sf::st_drop_geometry(tempCatchUnique[indMax, catchHfColName])[j, 1]
      finalTable[i, "Population covered"] <- tempCatchUnique$totalpop[indMax][j]
      if (adminCheck) {
        finalTable[i, "Region"] <- sf::st_drop_geometry(tempCatchUnique[indMax, adminColName])[j, 1]
      }
    }
    # Get catchment(s) with max population
    top <- tempCatchUnique[indMax, ]
    # Remove it from the potential catchment to be further selected
    tempCatchUnique <- tempCatchUnique[-indMax, ]
    # If we still have remaining catchments
    if (nrow(tempCatchUnique) > 0) {
      # For each one, check if it has been reduced by the overlap of the selected catchment
      for (k in 1:nrow(tempCatchUnique)){
        # If we have an intersection with the selected catchment (top)
        if (suppressWarnings(sf::st_intersects(tempCatchUnique[k, ], top, sparse = FALSE)[1, 1])){
          # Get the non-symmetrical difference of the intersection (x-y)
          stock <- suppressWarnings(sf::st_difference(tempCatchUnique[k, ], top))
          # Get the same catchment table column order
          stock <- stock[, match(colnames(tempCatchUnique), colnames(stock))]
          # If the selected catchment contains another catchment, delete the contained catchment.
          if (nrow(stock) == 0){
            tempCatchUnique$totalpop[k] <- 0
          } else {
            # If not, replace the catchment k by the remaining parts of the catchment
            tempCatchUnique[k, ] <- stock
          }
        }
      }
      setTxtProgressBar(bar, ceiling((((i - 1) * 100) / nTot) + (k * 100 / nTot) / nrow(tempCatchUnique)))
    }
  }
  close(bar)
  
  # Calculation of outputs
  colNamesFT <- colnames(finalTable)
  for (i in 1:nrow(finalTable)) {
    finalTable$Rank[i] <- i
    finalTable$cumul[i] <- sum(finalTable[, "Population covered"][1:i])
  }
  
  # Calculation of cumulative sum
  finalTable <- finalTable[, c("Rank", colNamesFT, "cumul")]
  colnames(finalTable)[ncol(finalTable)] <- "Cumulative sum"
  
  # Retrieve facility names from groups (when corresponding)
  for (i in 1:nrow(finalTable)) {
    ind <- grepl(finalTable$`Facility name`[i], similarTable$`Health Facilities`)
    if (any(ind)) {
      # In theory not possible.
      if (sum(ind) > 1) {
        stop(paste("Health facility in multiple groups:", finalTable$`Facility name`[i]))
      }
      finalTable$`Facility name`[i] <- similarTable$`Health Facilities`[ind]
    }
  }
  
  # Create output folder
  outFolder <- file.path(workDir, "outputs", gsub("-|[[:space:]]|\\:", "", Sys.time()))
  dir.create(outFolder, showWarnings = F)
  
  # Write the csv of the ranked candidates and their population cover
  write.csv(finalTable, paste0(outFolder, "/facilities_table.csv"), row.names = FALSE)
  
  # Create the plot png
  par(mar=c(8,4,4,2))
  png(filename = paste0(outFolder, "/cumulative_sum.png"), width = 1000, height = 1000, units = "px")
  barplot((finalTable$`Cumulative sum`/10000), main = "Cumulative sum of the population covered (per 10000 habitants)", ylab = "", col = "dodgerblue3", las = 2, names.arg = paste("Rank", finalTable$Rank), las=2)
  invisible(dev.off())
  cat(paste("Calculations completed.\nOutputs can be found under:", outFolder))
}