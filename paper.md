---
title: 'inAccessMod: An R package to download and prepare data for AccessMod software'
tags:
- R
- GIS
- geographical accessibility
- health coverage
date: "14 April 2023"
output: pdf_document
authors:
- name: Pablo Timoner
  orcid: "0000-0003-4757-4928"
  corresponding: yes
  affiliation: '1'
- name: Fleur Hierink
  orcid: "0000-0002-2727-0540"
  affiliation: 1
- name: Loïc Baecher
  affiliation: 1
- name: Nicolas Ray
  orcid: "0000-0002-4696-5313"
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: GeoHealth group, Institute of Global Health, Faculty of Medicine, University
    of Geneva, Geneva, Switzerland
  index: 1
---

# Summary

`inAccessMod` is an R package that allows the user to easily download and prepare all the required layers for the World Health Organization-recognized AccessMod software used to model physical accessibility to health care. The layer downloading, cropping, masking, projecting, resampling processes are automated to a large degree, making the preparation of the inputs quick and straightforward. The package includes additional functions to modify inputs and facilitate complex analyses (e.g. sub-national travel scenarios), to handle health facility data from The Health Resources and Services Availability Monitoring System (HeRAMS), and to perform specific result assessments (e.g. ranking of health facilities based on their coverage).

# Statement of need
 
AccessMod is a World Health Organization-recognized, free and open source software tool that allows the user to create models that determine the level of physical accessibility of existing health services for the target population [@ray:2008]. The software can also estimate the proportion of the target population that may not receive care even though the services are physically accessible, due to a lack of capacity in terms of staff or equipment. Additionally, AccessMod can measure the time and distance required for referrals between health facilities, and identify strategic locations to place new health facilities for enhanced population coverage, as part of the scaling up analysis. This software has been used in several peer-reviewed articles and reports from governmental and intergovernmental organizations including, but not limited to @ochoa:2023, @oliphant:2022, @hierink:2020, @joseph:2020, @ouma:2018, @unfpa:2020, @usaid:2016, @who:2015, and @who:2015-1.  

Several layers of input data reflecting barriers and facilitators to the mobility of the target population are required in conjunction with data on the location of population and health facilities. Open data on population, elevation, land cover, roads, barriers, administrative boundaries, etc. are available but required to be downloaded and processed by the user before being uploaded into AccessMod, which can be time consuming. 

While the functions to process health facility tables are specifically designed to handle data from HeRAMS, the other ones can be used for any other project. These allow to create a clear directory structure for the inputs, download the required rasters from WorldPop (population), SRTM (elevation), Copernicus (land cover), OpenStreetMap (roads, barriers), and geoBoundaries (administrative boundaries) directly to their corresponding folders, to crop, mask, project and resample the inputs using default or user parameters while keeping track of all the operations. 

`inAccessMod` is intended for use by academic researchers, as well as GIS technicians who work closely with global health stakeholders and decision-makers.

# Main functions

* `initiate_project`: Start the project, selecting the country, getting the ISO alpha-3 code, etc.
* `download_boundaries`: Download the administrative boundaries from geoBoundaries
* `set_projection`: Set the projected coordinate reference system for the project
* `filter_hf`: Filter the health facilities (only compatible with HeRAMS data)
* `create_hf_shapefile`: Create a shapefile of health facilities (only compatible with HeRAMS data)
* `download_dem`: Download a DEM raster (SRTM)
* `download_population`: Download a population raster (WorldPop)
* `download_landcover`: Download a landcover raster (Copernicus)
* `download_osm`: Download road, river, lake and other natural barriers shapefiles (OSM)
* `process_inputs`: Process all the raw inputs
* `multi_ts`: Allows to handle different travel scenarios for different administrative units
* `compile_processed_data`: Compile all the processed data into one single folder to facilitate the input importation into AccessMod
* `hf_best_cov`: Select the health facilities that offer the best population coverage using their catchments and a raster of population.

# Acknowledgements

We acknowledge contributions from Andrew Curtis, Yaniss Guigoz, Camille Chênes, Carlos Ochoa and Zeynabou Sy for their help in testing the package. Their input and feedback were crucial in identifying and resolving technical issues, ensuring the accuracy and reliability of the results.

# References
