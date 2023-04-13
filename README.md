# LandGenBembix
Landscape genetics scripts

Source code and data for manuscript
Version software: R-4.1.3

Below is a short description of the folders and each script. All raw data, end data for one run and intermediary steps are provided. All scripts are provided to calculate genetic distances, set up all ascii-files, and run optimizations including bootstrap analyses. Data and outputs are not provided for the optimizations themselves (high amount of memory needed), but summarized data of bootstraps is provided to run the final script for model averaging and making plots. 

## 1 Genetic data assembly
Scripts to assemble the needed R objects (genind objects from the adegenet package) to calculate genetic distances from raw genetic data

Scripts:
- ‘Data formatting complete study area.R’: creating RDS-files to have genetic data ready for complete study area
- ‘Data formatting per areadune.R’: creating RDS-files to have genetic data ready for four subareas
- ‘Genetic distance calculations.R’: calculating the genetic distances and save them
- Helper scripts: ‘H_geneticdata.R’, ‘data4genind_script.R’

Data:
- needed from: ‘../data/genetic data/’
- created in: both ‘../data/genetic data/’ and ‘../data/genetic_distances/’

## 2 Resistance maps assembly
Scripts to assemble the raster files from raw data (reclassification, aggregation, etc) into the base tif-files of the landscape categories used in the landscape genetics analyses. Several resolutions are used. These tif-files are then used to create ascii-files for each separate optimization (per possible combination of categories, per area).

Scripts:
- ‘raster raw assembly.R’: the assembling of raster files from the raw data (BBK and dune classfications) into the reclassified landscape categories used in the manuscript (supplementary S1). Tif-files are created at chosen resolution of the complete study area and four dune area clusters.
-	‘raster ascii SS SC.R’: script to create ascii-files from the tif-files created in previous script. Only single categories (SC) are created here.
-	‘raster ascii MC.R’: script to create ascii-files from the tif-files created in previous script. Only multiple categories (MC) are created here (>1 category).

Data:
-	Needed form ‘../data/vector_raw/’, ‘../data/raster_aggregated/’
-	‘../data/raster_raw/’ is provided at https://drive.google.com/drive/folders/1HXit8ldvhjjAOUpez_C8vhnzuiV0Gv1t?usp=share_link	
- Created in: ‘../data/raster_aggregated/’ and own chosen directories for ascii-files

## 3 ResistanceGA optimizations
Scripts to optimize resistance rasters. Note: ascii-files are not provided, these should be created locally with the scripts above.

Scripts:
-	Prep_list_possible_combinations.R: script to create vector for the possible combinations from the ascii-files present (data created then in ‘../data/vector-combinations/’
-	Westcoast_SS_SC_hpc.R: data prep and actual optimization of single category rasters (ascii-files) for complete study area (westcoast). Only one raster at a time is optimized.
-	Westcoast_SS_MC_hpc.R: data prep and actual optimization of multiple category rasters (ascii-files) for complete study area (westcoast). Only one raster at a time is optimized
-	Areasdunes_SS_SC_hpc.R: data prep and actual optimization of single category rasters (ascii-files) for the four dune area clusters (westhoek, cabour, doornpanne, teryde).
-	Areasdunes_SS_MC_hpc.R: data prep and actual optimization of multiple category rasters (ascii-files) for the four dune area clusters (westhoek, cabour, doornpanne, teryde).

Data:
-	Needed from: own created directory with ascii-files, ‘../data/genetic_distances/’, ‘../data/vector-combinations/’
-	Created in: own created directory for outputs

## 4 Bootstrap analyses
Scripts to run bootstrap analyses from outputs from optimizations. Data from optimizations is not provided, should be created first with scripts from above. Output example from these bootstraps is given.

Scripts:
-	Bootstrapping_westcoast_SS_MC.R: bootstrap analyses for all possible combinations of categories for complete study area.
-	Bootstrapping_areasdunes_SS_MC.R: bootstrap analyses for all possible combinations of categories for the four dune area clusters.
-	Bootstrapping_westcoast_SS_SC.R: bootstrap analyses for single categories only for complete study area.
-	Bootstrapping_areasdunes_SS_SC.R: bootstrap analyses for single categories only for the four dune area clusters.

Data:
-	Needed from: own created directory with outputs from optimizations and ‘../data/genetic_distances/’
-	Created in: ‘../outputs/bootstrapping/’

## 5 summary analyses and plots
Scripts to assemble summary tables from the optimizations and bootstrap-results. To be able to run these, the optimizations should be run first. The model averaging and plots can be run with the data provided (which would be created with the summary-scripts).

Scripts:
-	Summary_[westcoast/areasdunes]_SS_[MC/SC].R: four scripts to compile/summarize results from the optimizations.
-	Bootstrap_summary: script to compile results from the optimizations (previous script has to be run first) and the bootstrap analyses.
-	Model_averaging_and_plots.R: script to perform model averaging and make plots of relative resistance values. This script can be run independently with the data provided.
- combinedfig_relative_resistancevalues.R: script to make relative resistance value boxplot graphs from main manuscript, where run 1 and run 2 are combined. Previous script makes needed summaries, but script can also be run independently with the data provided.

Data:
-	Needed from: own created directory with outputs from optimizations and ‘../outputs/bootstrapping/’; 
-	Created in: ‘../outputs/bootstrapping/’ and ‘../outputs/summaries/’
