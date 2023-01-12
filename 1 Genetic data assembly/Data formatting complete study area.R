####Aim script####
##This script is to format population genetics dataset to usable files for analyses (structure, genepop, geneclass,...)
##
##By: Femke Batsleer

library("dplyr")
library("tidyr")
library("tidyverse")
library("adegenet")
library("adespatial")
library("spdep")
library("poppr")
library("hierfstat")
library("diveRsity")
library("graph4lg")
library("miscTools")
library("stringr")
library("RColorBrewer")
library("MASS")
library("reshape2")
library("foreign")

###Loading data and making genind objects####
## Loading data
all_data <- read.csv("../data/genetic data/landgensamples_microsats_data.csv", sep=",")

#leave out the microsats that did not fullfill assumptions of HW, LD, Null alleles (see script population genetics manuscript)
all_data <- all_data %>% dplyr::select(-AGBro57_a, -AGBro57_b, -AGBro35_a, -AGBro35_b,
                                       -AGBro20_a, -AGBro20_b, -AGBro16_a, -AGBro16_b,
                                       -AGBro419_a, -AGBro419_b, -AGBro111_a, -AGBro111_b,
                                       -AGBro138_a, -AGBro138_b)

####GENIND objects####
#first get own helper function from script to easily convert data to format for genind
source("data4genind_script.R")

#make dataset with loci suitable for genind
loci_data <- data4genind(dplyr::select(all_data, Sample:AGBro218_b))

#make a genind-object with adegenet package, for population level
pop_genind <- df2genind(loci_data, sep="/", ind.names=NULL, loc.names=NULL, pop=all_data$Gebied,
                                 strata = dplyr::select(all_data, Gebied, Population, Year), NA.char="NA", ploidy=2, type="codom")
#add coordinates to the genind object
pop_genind@other$xy = as.vector(dplyr::select(all_data, "X_Lambert", "Y_Lambert"))

###Checking duplicated genotypes####
#recapture wetteren, recaptures from shipgatduinen

#Duplicated genotypes
mlg(pop_genind) #no duplicates present
dups <- mlg.id(pop_genind)
for (i in dups){
  if (length(dups[i]) > 1){print(i)}
}
# Create a vector of individual names without the duplicates
Nodups = indNames(pop_genind)[! indNames(pop_genind) %in% c("519", "520")] #recaptures
# Create a new genind object without the duplicates
pop_genind = pop_genind[Nodups, ]
mlg(pop_genind)


# ###saving genind-objects####
saveRDS(pop_genind, file = "../data/genetic data/allsamples_genind_pop.RDS")

#how to load it again, for in other and following scripts
#a <- readRDS("../data/genind_selection_pop.RDS")

####Select spatially all samples from 2018 that are within westcoast clipper####

library(sf)
#load westcoast clipper
westcoast_sf <- read_sf(dsn="../data/vector_raw", layer="westcoast_clipper_adjustedborders_metpolders")
plot(st_geometry(westcoast_sf))
#st_crs(31370)==st_crs(westcoast) #chick if in Lambert 72 epsg:31370

#make points from samples
samples_sf <- st_as_sf(all_data, coords=c("X_Lambert", "Y_Lambert"), crs=31370)
plot(st_geometry(samples_sf), col="red", add=T)

#make intersection
samples_wc_sf <- st_intersection(samples_sf, westcoast_sf) %>%
  bind_cols(as.data.frame(st_coordinates(.)) %>% #add coordinates in data columns as well
              rename("X_Lambert"="X", "Y_Lambert"="Y"))
plot(st_geometry(samples_wc_sf), col="blue", add=T)
#filter only data from 2018
samples_wc2018_sf <- samples_wc_sf %>% filter(Year==2018)
#make dataframe
wc2018_data <- as.data.frame(samples_wc2018_sf)

#make genind object
#make dataset with loci suitable for genind
loci_data_wc2018 <- data4genind(dplyr::select(wc2018_data, Sample:AGBro218_b))

#make a genind-object with adegenet package, for population level
pop_genind_wc2018 <- df2genind(loci_data_wc2018, sep="/", ind.names=NULL, loc.names=NULL, pop=wc2018_data$Gebied,
                        strata = dplyr::select(wc2018_data, Gebied, Population, Year), NA.char="NA", ploidy=2, type="codom")
#add coordinates to the genind object
pop_genind_wc2018@other$xy = as.vector(dplyr::select(wc2018_data, "X_Lambert", "Y_Lambert"))

#Duplicated genotypes
mlg(pop_genind_wc2018) #no duplicates present
dups <- mlg.id(pop_genind_wc2018)
for (i in dups){
  if (length(dups[i]) > 1){print(i)}
}
# Create a vector of individual names without the duplicates
Nodups_wc2018 = indNames(pop_genind_wc2018)[! indNames(pop_genind_wc2018) %in% c("519", "520")] #recapture from shipgatduinen
# Create a new genind object without the duplicates
pop_genind_wc2018 = pop_genind_wc2018[Nodups_wc2018, ]
mlg(pop_genind_wc2018)

# ###saving genind-objects####
saveRDS(pop_genind_wc2018, file = "../data/genetic data/wc2018_genind_pop.RDS")
