[![DOI](https://joss.theoj.org/papers/10.21105/joss.05879/status.svg)](https://doi.org/10.21105/joss.05879)
# inAccessMod
AccessMod is a World Health Organization-recognized, free and open source software tool that allows users to model the geographic accessibility of health services. To model the geographic accessibility of health services, several layers of input data reflecting barriers and facilitators to the mobility of the target population are required in conjunction with data on the location and availability of health services. More information on AccessMod installation and use can be found here: https://www.accessmod.org/ 

inAccessMod is a R package that allows the user to easily download and prepare all the required layers for AccessMod. A proper folder structure is created in order 
to manage multi-temporal data and/or multiple analysis scenarios. While the functions to process health facility tables are specifically designed to handle data
from The Health Resources and Services Availability Monitoring System (HeRAMS), the other ones can be used for any other project. The layer downloading, cropping, 
masking, resampling, exporting processes are automated to a large degree, making the preparation of the inputs quick and straightforward. 

## Installation

This package requires R version 4.1.3 or later. It also requires the following packages: `crsuggest`, `data.table`, `dplyr`, `exactextractr`, `fasterize`, `fs`, `geodata`, `jsonlite`, `lubridate`, `osmextract`, `purrr`, `raster`, `RCurl`, `readxl`, `rgeoboundaries` (version 0.0.0.9000 or later), `rmarkdown`, `sf`, `stringi`, `stringr`, `testthat`, `tibble`, `utils`, and `writexl`. These dependencies should be installed automatically when `dependencies = TRUE` is set in the command used to install the package.

```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("unige-geohealth/inAccessMod", build_vignettes = TRUE, dependencies = TRUE)
```

If you decide to install the dependencies manually, please note that `rgeoboundaries` must be installed using remotes or devtools:

```
devtools::install_github("wmgeolab/rgeoboundaries")
```

## Main functions
* `initiate_project`: Allows starting a new project by choosing the country (or city) and retrieving the official ISO alpha-3 code.
* `download_boundaries`: Downloads the required administrative boundaries from the geoBoundaries database.
* `set_projection`: Sets the projected coordinate reference system (CRS) for the project to ensure that all input layers have a consistent projection.
* `HeRAMS_filter_hf`: Filters the health facility data based on user criteria (only compatible with WHO/HeRAMS data).
* `HeRAMS_create_hf_shapefile`: Creates a shapefile of health facilities (only compatible with WHO/HeRAMS data).
* `download_dem`: Downloads the Digital Elevation Model (DEM) raster for the country/area of interest from the Shuttle Radar Topography Mission (SRTM) dataset.
* `download_population`: Downloads the population count raster for the target country/area from the WorldPop project.
* `download_landcover`: Downloads the land cover raster for the country/area of interest from the Copernicus Land Monitoring Service.
* `download_osm`: Downloads road, river, lake, and other natural barriers shapefiles from the OpenStreetMap (OSM) platform
* `process_inputs`: Processes all the raw input data by cropping, masking, projecting, and resampling the geospatial layers with customizable user parameters.
* `compile_processed_data`: Compiles all the processed data into one single folder, making it easier to import the input data into AccessMod.
* `multi_ts`: Handles different travel scenarios for different administrative units.
* `hf_best_cov`: Selects the health facilities that provide the best population coverage based on their catchments and a population raster.

## Tutorial

You can access the tutorial directly in R using the following command, or by opening the TUTORIAL file located in the main directory of the repository. Remember that you must have a working internet connection in order to access all the functions that allow you to download data.
 
```
vignette("Tutorial", package = "inAccessMod") 
```

## Automated tests

After cloning the repository, go to the package main directory and run:
 
```
devtools::test()
```

## Contributor guidelines

Contributions are welcome and greatly appreciated! To contribute, please follow the following guidelines:

### Reporting issues

* Check that the issue has not already been reported on the [issue tracker](https://github.com/unige-geohealth/inAccessMod/issues).
* Submit an issue on the [issue tracker](https://github.com/unige-geohealth/inAccessMod/issues).

### Development process

* Fork the repository, make changes in your fork, and submit a pull request.
* Follow the existing coding style and structure.
* Write tests for any new functionality.
* Document any changes in the package documentation.

### Where to get help

* Maintainer: [Pablo Timoner](mailto:pablo.timoner@unige.ch)
