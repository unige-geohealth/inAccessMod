---
title: 'inAccessMod: An R package to automate data downloading and processing for
  AccessMod'
tags:
- R
- GIS
- geographical accessibility
- health coverage
date: "30 June 2023"
output:
  pdf_document: default
  html_document:
    df_print: paged
authors:
- name: Pablo Timoner
  orcid: "0000-0003-4757-4928"
  corresponding: yes
  affiliation: 1
- name: Fleur Hierink
  orcid: "0000-0002-2727-0540"
  affiliation: 1
- name: Loïc Baecher
  affiliation: 1
- name: Caroline Fuhrer
  affiliation: 2
- name: Nicolas Ray
  orcid: "0000-0002-4696-5313"
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: GeoHealth group, Institute of Global Health, Faculty of Medicine, University
    of Geneva, Geneva, Switzerland
  index: 1
- name: Health Resources and Services Availability Monitoring System (HeRAMS) Initiative,
    World Health Organization Headquarters. Geneva, Switzerland.
  index: 2
---

# Summary

`inAccessMod` is an R package that simplifies the process of downloading and preparing geospatial layers required for AccessMod, an official software from the World Health Organization (WHO) used to model physical accessibility to healthcare. The package makes it easy to prepare all necessary inputs by automating tasks such as data downloading, cropping, masking, projection, and resampling with easy-to-use functions. `inAccessMod` includes additional functions that help users modify inputs and perform complex analyses like merging sub-national travel scenarios. The package also facilitates handling of health facility data from The Health Resources and Services Availability Monitoring System (WHO/HeRAMS), and performs specialized result assessments like ranking of health facilities based on their coverage.

# Statement of need

AccessMod is a free and open-source geospatial tool for modeling physical accessibility of health services for a target population [@ray:2008]. The software can help estimate the proportion of target populations who may not receive care due to long travel time or due to a lack of capacity in terms of staff or equipment. Additionally, AccessMod can measure the time and distance required for referring patients between different types of services, and identify strategic locations to place new health facilities for enhanced population coverage, as part of a scaling up analysis. AccessMod is extensively used worldwide, with many applications reported in peer-reviewed articles and other publications from governmental and intergovernmental organizations (e.g., @ochoa:2023, @oliphant:2022, @hierink:2020, @joseph:2020, @ouma:2018, @unfpa:2020, @usaid:2016, @who:2015, and @who:2015-1).

To use AccessMod, users need several geospatial input layers related to population, elevation, land cover, roads, barriers, administrative boundaries, etc. These layers require downloading and processing by users before being used in AccessMod. However, the different GIS operations involved in pre-processing these input layers can be time-consuming and might cause issues with data consistency and quality.

`inAccessMod` provides a simple solution that enables users to download and preprocess geospatial input layers required for AccessMod quickly and easily. The package automates several data processing tasks, including downloading corresponding layers from popular sources like WorldPop, SRTM, Copernicus, OpenStreetMap, and geoBoundaries. Users can automatically crop, mask, project and resample the inputs by following straightforward parametrizations provided by the package. A clear directory structure is created with all datasets (Figure 1), facilitating their import into Accessmod. The package also enables users to handle health facility data from the WHO/HeRAMS system effectively.

Overall, `inAccessMod` simplifies the process of preparing geospatial input layers for AccessMod into a quicker, more reliable, and more user-friendly experience. Familiarity with GIS and geospatial data formats is recommended to use `inAccessMod`.

![](folder_structure.png){width=80%}
Figure 1: Directory structure we we have a folder for each input, and corresponding subfolders for raw and processed data.

# Main functions

-   `initiate_project`: Allows starting a new project by choosing the country and retrieving the official ISO alpha-3 code.
-   `download_boundaries`: Downloads the required administrative boundaries from the geoBoundaries database.
-   `set_projection`: Sets the projected coordinate reference system (CRS) for the project to ensure that all input layers have a consistent projection.
-   `HeRAMS_filter_hf`: Filters the health facility data based on user criteria (only compatible with WHO/HeRAMS data).
-   `HeRAMS_create_hf_shapefile`: Creates a shapefile of health facilities (only compatible with WHO/HeRAMS data).
-   `download_dem`: Downloads the Digital Elevation Model (DEM) raster for the country/area of interest from the Shuttle Radar Topography Mission (SRTM) dataset.
-   `download_population`: Downloads the population count raster for the target country/area from the WorldPop project.
-   `download_landcover`: Downloads the land cover raster for the country/area of interest from the Copernicus Land Monitoring Service.
-   `download_osm`: Downloads road, river, lake, and other natural barriers shapefiles from the OpenStreetMap (OSM) platform
-   `process_inputs`: Processes all the raw input data by cropping, masking, projecting, and resampling the geospatial layers with customizable user parameters.
-   `multi_ts`: Handles different travel scenarios for different administrative units.
-   `compile_processed_data`: Compiles all the processed data into one single folder, making it easier to import the input data into AccessMod.
-   `hf_best_cov`: Selects the health facilities that provide the best population coverage based on their catchments and a population raster.

# Installation

This package requires R version 4.1.3 or later. It also requires the following packages: `crsuggest`, `data.table`, `dplyr`, `exactextractr`, `fasterize`, `fs`, `gdalUtils` (version 2.0.3.2 or later), `geodata`, `jsonlite`, `lubridate`, `osmextract`, `purrr`, `raster`, `RCurl`, `readxl`, `rgdal`, `rgeoboundaries` (version 0.0.0.9000 or later), `rgeos`, `rmarkdown`, `sf`, `sp`, `stringi`, `stringr`, `testthat`, `tibble`, `utils`, and `writexl`. These dependencies should be installed automatically when `dependencies = TRUE` is set in the command used to install the package.

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("unige-geohealth/inAccessMod", 
      build_vignettes = TRUE,
      dependencies = TRUE)


# Author contributions

Pablo Timoner authored the original version of the package, developed the pipeline and majority of its functions, maintained the package, wrote the documentation, debugged the code, and contributed to the drafting of the article. Fleur Hierink wrote the initial code, forming the base for the package's main functions. Loïc Baecher was responsible for the creation of the `hf_best_cov` function core. Caroline Fuhrer assisted in conceptualizing the HeRAMS-related functions. Nicolas Ray supervised the project and participated in the article drafting process.

# Acknowledgements

We acknowledge contributions from Andrew Curtis, Yaniss Guigoz, Camille Chênes, Carlos Ochoa and Zeynabou Sy for their help in testing the package. Their input and feedback were crucial in identifying and resolving technical issues, ensuring the accuracy and reliability of the results.

# References
