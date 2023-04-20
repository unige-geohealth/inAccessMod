# inAccessMod
AccessMod is a World Health Organization-recognized, free and open source software tool that allows users to model the geographic accessibility of health services. 
To model the geographic accessibility of health services, several layers of input data reflecting barriers and facilitators to the mobility of the target population 
are required in conjunction with data on the location and availability of health services. 

inAccessMod is a R package that allows the user to easily download and prepare all the required layers for AccessMod. A proper folder structure is created in order 
to manage multi-temporal data and/or multiple analysis scenarios. While the functions to process health facility tables are specifically designed to handle data
from The Health Resources and Services Availability Monitoring System (HeRAMS), the other ones can be used for any other project. The layer downloading, cropping, 
masking, resampling, exporting processes are automated to a large degree, making the preparation of the inputs quick and straightforward. 

## Installation
```
if (!require("devtools")) install.packages("devtools")

devtools::install_github("ptimoner/inAccessMod", build_vignettes = TRUE)
```

## Main functions
* `initiate_project`: Start the project, selecting the country, getting the ISO alpha-3 code, etc.
* `download_boundaries`: Download the administrative boundaries from geoBoundaries
* `set_projection`: Set the projected coordinate reference system for the project
* `HeRAMS_filter_hf`: Filter the health facilities (only compatible with HeRAMS data)
* `HeRAMS_create_hf_shapefile`: Create a shapefile of health facilities (only compatible with HeRAMS data)
* `download_dem`: Download a DEM raster (SRTM)
* `download_population`: Download a population raster (WorldPop)
* `download_landcover`: Download a landcover raster (Copernicus)
* `download_osm`: Download road, river, lake and other natural barriers shapefiles (OSM)
* `process_inputs`: Process all the raw inputs
* `multi_ts`: Allows to handle different travel scenarios for different administrative units
* `compile_processed_data`: Compile all the processed data into one single folder to facilitate the input importation into AccessMod
* `hf_best_cov`: Select the health facilities that offer the best population coverage using their catchments and a raster of population.

## Tutorial
See the package vignette.
