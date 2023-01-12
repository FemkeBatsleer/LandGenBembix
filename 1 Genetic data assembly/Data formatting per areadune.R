###Make separate genind objects and save them as RDS files per dunearea###
#by: Femke Batsleer

library(adegenet)
library(raster)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(tibble)

#load all genetic data (genind object) and get coordinates
gendata <- readRDS("../data/genetic data/wc2018_genind_pop.RDS")
coord.alldata <- gendata$other$xy %>% add_column(Sample=rownames(gendata@tab))

#info
areadunes.v <- c("westhoek", "doornpanne", "teryde", "cabour")
resolution <- 50
raster.path <- "../data/raster_aggregated/"

#loop over all dune areas, make overlay with genetic data of individuals to select these and save separate genind objects for these
for(areadune in areadunes.v){
  #read rasterfile
  areadune.r <- mosaic(sprc(rast(paste0(raster.path, "classes7" , "_", as.character(resolution),"m_", areadune,".tif"))))
  
  #make shapefile of samples
  samples_sf <- st_as_sf(as.data.frame(coord.alldata), coords=c("X_Lambert", "Y_Lambert"), crs=31370)
  # plot(st_geometry(samples_sf), col="red", add=T)
  
  #extract values into points from raster file
  r.values_samples <- terra::extract(areadune.r, samples_sf)
  #remove the NA's
  samples.sel_df <- r.values_samples %>% drop_na(layer)
  
  #select from genind object gendata those that are present in areadune
  samples.sel_v <- samples.sel_df$ID
  gendata.sel <- gendata[samples.sel_v]
  print(nInd(gendata.sel))
  
  saveRDS(gendata.sel, file = paste0("../data/genetic data/", areadune, "_genind.RDS"))
}

