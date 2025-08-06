<!-- badges: start -->
[![build](https://github.com/JGCRI/rtemplate/workflows/build/badge.svg)](https://github.com/JGCRI/rtemplate/workflows/build/badge.svg?branch=main)
[![test_coverage](https://github.com/JGCRI/rtemplate/actions/workflows/test_coverage.yml/badge.svg?branch=main)](https://github.com/JGCRI/rtemplate/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/JGCRI/rtemplate/branch/main/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/rtemplate)
[![docs](https://github.com/JGCRI/rtemplate/actions/workflows/docs.yaml/badge.svg?branch=main)](https://github.com/JGCRI/rtemplate/actions/workflows/docs.yaml)
<!-- badges: end -->


# MONet Data Synthesis and Comparison 

## What is MONet?

The **M**olecular **O**bservation **Net**work is on ongoign project developed by the Environmental Molecular Sciences Laboratory at Pacific Northwest National Lab, with aims to create a continetial scale database of standardized soil molecular properties to advance the understanding of soil biogeochemistry.
The **MONet Data Synthesis Project** aims to systematically compare and integrate soil data from the **Monitoring Soil National Network (MONet)** with publicly available datasets, such as **SoilGrids** and the **Soil Respiration Database (SRDB)**. This repository contains scripts to analyze and compare key soil properties (e.g., soil respiration, pH, and clay content) across diverse climatic zones, with an emphasis on addressing spatial scale differences between global datasets and national-scale, high-resolution efforts like MONet.

### Goals of the Analysis

1. **Understand Soil Properties Relative to Environmental Drivers:**
   - Explore soil respiration variations with mean annual precipitation (MAP) and mean annual temperature (MAT).
   - Assess the spatial distribution and variability in pH and clay content at national and sub-national levels.
   - Compare global trends (e.g., SoilGrids) with regional/national observations (e.g., MONet).
2. **Cross-Dataset Validation:**
   - Benchmark soil properties across MONet and publicly available soil datasets to evaluate consistency and spatial variability.
3. **Enhance Data Accessibility:**
   - Provide processed data, visualizations, and detailed scripts to the soil science, biogeochemistry, and environmental science communities.

---

## Repository Overview

| **Folder**       | **Description**                                                                                  |
|-------------------|--------------------------------------------------------------------------------------------------|
| `data/`           | Directory for storing raw and publicly available datasets.                                       |
| `scripts/`        | R Markdown scripts for data collection and processing		                               |
| `figures/`        | Directory for saving plots and visual outputs from the analyses.                                 |
|`MONetSynthesis.Rmd`| R markdown file containing analysis of MONet data product.                                      |
|`MONetDataPreprocessing.R`| R script for downloading and preprocessing data					       |
| `README.md`       | Overview of the repository, datasets, and instructions for reproducing the analyses.             |

---

## Data Sources

This project integrates multiple high-quality datasets. A summary of their sources and attributes is given below:

| **Dataset**             | **Description**                                                                                                     | **Source**                                                                                                          |
|-------------------------|---------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------|
| MONet Soil Respiration  | Processed soil respiration data collected in the United States.                                                    | [MONet Soil Respiration - Zenodo](https://zenodo.org/records/15328215)                                              |
| MONet Clay and pH       | Clay content and pH of X number of soil samples in the United States                                               | [MONet](https://sc-data.emsl.pnnl.gov/monet)
| SoilGrids               | Global gridded clay content and pH at 10km resolution.                         					| [SoilGrids](https://soilgrids.org/)                                                                                 |
| SRDB                    | Soil respiration observations aggregated from published journal articles globally.                                  | [SRDB GitHub Repository](https://github.com/bpbond/srdb)                                                            |
| Climate Zones           | Koppen-Geiger classification shapefile used for grouping comparisons by climatic similarities.                     | [North American Climate Atlas](https://www.cec.org/north-american-environmental-atlas/climate-zones-of-north-america) |
| USA Shapefiles          | Spatial reference shapefiles for delineating site boundaries in the continental U.S.                               | [NOAA GIS US States Shapefiles](https://www.weather.gov/gis/USStates)                                               |

---

## Instructions to Gather and Prepare Data

1. **Download Raw Data**:
   - Clone this repository and navigate to the `data/` folder.
   - Download the MONet data, Climate Zones, and USA shapefiles from the sources provided above and store them in the organized subdirectories.
   - For SoilGrids and SRDB, run the get_SGdata.R and get_SRDBdata.R 
   
2. **Install Required R Packages**:
   - Install all necessary R libraries used in this analysis

3. **Run Data Processing Scripts**
   - Open and run provided R markdown (MONetDataProcessing.Rmd) script
   - This script will take some time to run and saves a Rdata file to the directory

4. **Run Analysis Scripts**:
   - Open the provided R markdown (MONetSynthesis.Rmd) script.
   - Run the script to:
     - Conduct exploratory analyses comparing respiration trends, MAP/MAT relationships, etc.
     - Perform spatial and statistical comparisons on pH and clay content.

---

## Statistical and Comparative Focus Areas
The analysis pipeline includes:

1. **Soil Respiration Analysis**:
   - Relating soil carbon flux (respiration) to MAP and MAT across the U.S.
   - Identifying climatic influences on observed trends using regression.

2. **pH and Clay Content Comparison**:
   - Cross-referencing MONet-observed properties with SoilGrids' datasets spatially and within climatic zones.

4. **Data Visualization**:
   - Generating maps of sample coverage and plots comparing different datasets.
