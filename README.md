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
devtools::install_github("unige-geohealth/inAccessMod", build_vignettes = TRUE)
```

## Main functions
* `initiate_project`: Allows starting a new project by choosing the country and retrieving the official ISO alpha-3 code.
* `download_boundaries`: Downloads the required administrative boundaries from the geoBoundaries database.
* `set_projection`: Sets the projected coordinate reference system (CRS) for the project to ensure that all input layers have a consistent projection.
* `HeRAMS_filter_hf`: Filters the health facility data based on user criteria (only compatible with WHO/HeRAMS data).
* `HeRAMS_create_hf_shapefile`: Creates a shapefile of health facilities (only compatible with WHO/HeRAMS data).
* `download_dem`: Downloads the Digital Elevation Model (DEM) raster for the country/area of interest from the Shuttle Radar Topography Mission (SRTM) dataset.
* `download_population`: Downloads the population count raster for the target country/area from the WorldPop project.
* `download_landcover`: Downloads the land cover raster for the country/area of interest from the Copernicus Land Monitoring Service.
* `download_osm`: Downloads road, river, lake, and other natural barriers shapefiles from the OpenStreetMap (OSM) platform
* `process_inputs`: Processes all the raw input data by cropping, masking, projecting, and resampling the geospatial layers with customizable user parameters.
* `multi_ts`: Handles different travel scenarios for different administrative units.
* `compile_processed_data`: Compiles all the processed data into one single folder, making it easier to import the input data into AccessMod.
* `hf_best_cov`: Selects the health facilities that provide the best population coverage based on their catchments and a population raster.

## Tutorial
Make sure to set
```
vignette("Tutorial", package = "inAccessMod") 
```

