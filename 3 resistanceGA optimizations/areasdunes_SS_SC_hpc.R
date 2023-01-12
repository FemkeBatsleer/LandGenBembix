###Script to try optimiziation on only the westhoek data###
##WARNING: optimization can take very long! First try one iteration before upscaling
# #By: Femke Batsleer
hpc.bool <- FALSE

if(hpc.bool==TRUE){
  args <- commandArgs(TRUE)
  resolution <- as.numeric(as.character(args[1]))
  area_int <- as.numeric(as.character(args[2]))
  data.dir <- "./data/" #hpc
  output.dir <- "./data/outputs/SS_SC/" #hpc #
} else{
  resolution <- 20
  area_int <- 1
  data.dir <- "D:/Fbatslee/LandGenData/Github-files/" #local
  output.dir <- "D:/Fbatslee/LandGentData/Github-files/outputs/" #local
}


library(ResistanceGA)
library(raster)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(naniar)
library(tibble)
library(tictoc)


n_maxiter = 2#set to 300, 2 is to check if it runs #default=1000

#define area according to what is given in args
areas_v <- c("westhoek", "cabour", "doornpanne", "teryde")
area_char <- areas_v[area_int]
areasdunes <- c(area_char)

# #define category according to what is given in args
categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")#rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))

for(areadune in areasdunes){
  print(areadune)
  ####Load genetic distance####
  dist.v <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_PCA16.RDS"))
  #coordinates of the data
  coord.samples <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_coordsamples.RDS"))
  
  for(category in categories.v){
    ###Preparing variables and directories for resistanceGA run####
    ####give ascii-dir####
    ascii.dir <- paste0(data.dir, "ascii-files/areasdunes/SS_SC/", areadune, "/", resolution, "m/", category, "/")
    
    #if class does not exist in that area, go to next iteration (skip this optimization for particular category)
    asc.category <- read.table(paste0(ascii.dir, category, ".asc"), header=F, skip=6)# %>% replace_with_na_all(condition = ~.x == -9999)
    if(!any(asc.category==1, na.rm=TRUE)){
      #print(paste0("cateogry ", category, " doesn't exist in the raster file"))
      next}
    
    ###give directory for output results####
    output.dir.areadune <- paste0(output.dir, areadune, "/", as.character(resolution), "m/", category, "/")
    #check if folders exists for output, otherwise, create
    if (file.exists(output.dir.areadune)) {cat("The folder already exists")
    } else {dir.create(output.dir.areadune, recursive=T)}
    
    
    print(category)
    
    
    #prep genetic algorithm: specify all the details and options of how you want to conduct optimization with the genetic algorithm
    GA.inputprep <- GA.prep(ASCII.dir = ascii.dir,
                            Results.dir = output.dir.areadune, #"all.comb"
                            max.cat = 2500, #default=2500
                            method = "LL", #loglikelihood default
                            maxiter = n_maxiter,
                            parallel = 1 #parallel::detectCores() Don't use them all!
    )
    
    #prep genetic distance
    gdist.inputprep <- gdist.prep(n.Pops = nrow(coord.samples),
                                  response = dist.v,
                                  samples=coord.samples,
                                  method="costDistance")
    
    
    ###Run the optimization# for single surface####
    tic(paste("single surface", areadune, resolution, category, sep= ", "))
    SS_results.gdist <- SS_optim(gdist.inputs = gdist.inputprep,
                                 GA.inputs = GA.inputprep)
    saveRDS(SS_results.gdist, file = paste0(output.dir.areadune, "Results/", "results.gdist.RDS"))
    toc()
  }
}


