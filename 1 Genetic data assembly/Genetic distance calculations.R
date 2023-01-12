###Aim script###
#Calculate genetic distances between individuals to be used in resistanceGA and save them as RDS-files
#RDS-file is loaded which holds the genind-object (from package adegenet) calculated in script "Data formatting.R"

library(adegenet)
library(dplyr)
library(tidyr)
library(ecodist)

source('H_geneticdata.R')

#Complete study area----
###load genind data
gendata <- readRDS(file="../data/genetic data/wc2018_genind_pop.RDS")
###Get and save coordinates of samples
coord.samples <- as.matrix(gendata$other$xy[,c(1,2)])
saveRDS(coord.samples, file="../data/genetic_distances/westcoast/westcoast_coordsamples.RDS")
###Get and save genetic distance of samples
gen.distance <- calc_gendistance(gendata, type="PCA", n.axes=16)
saveRDS(gen.distance, file="../data/genetic_distances/westcoast/westcoast_PCA16.RDS")
gen.distance.m <- calc_gendistance_matrix(gendata, type="PCA", n.axes=16)

#Areas dunes----
for(areadune in c("westhoek", "doornpanne", "teryde", "cabour")){
  gendata <- readRDS(file=paste0("../data/genetic data/", areadune, "_genind.RDS"))
  coord.samples <- as.matrix(gendata$other$xy[,c(1,2)])
  saveRDS(coord.samples, file=paste0("../data/genetic_distances/areasdunes/", areadune, "_coordsamples.RDS"))
  gen.distance <- calc_gendistance(gendata, type="PCA", n.axes = 16)
  saveRDS(gen.distance, file = paste0("../data/genetic_distances/areasdunes/", areadune, "_PCA16.RDS"))
}

