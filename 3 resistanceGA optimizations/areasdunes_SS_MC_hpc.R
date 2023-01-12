###Script to try do optimization with multicategorical surfaces for dune areas###
##WARNING: optimization can take very long! First try one iteration before upscaling
##By: Femke Batsleer

hpc.bool <- F

if(hpc.bool==TRUE){
  args <- commandArgs(TRUE)
  resolution <- as.numeric(as.character(args[1]))
  area_int <- as.numeric(as.character(args[2]))
  cat_n <- as.numeric(as.character(args[3]))
  #division_int <- ifelse(area_int==1, 4, ifelse(area_int==2, 8, 17))
  data.dir <- "./data/" #hpc
  output.dir <- "./data/outputs/SS_MC/" #hpc #
} else{
  resolution <- 20
  area_int <- 4 #ter yde choose from 1:4 c("westhoek", "cabour", "doornpanne", "teryde")
  cat_n <- 9 #depending on area, choose from 1:26/57/120 check combs_dunearea <- readRDS(file= paste0(data.dir, "vector-combinations/comb_", areadune, ".RDS"))
  data.dir <- "D:/Fbatslee/LandGenData/Github-files/" #local
  output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/" #local
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

ascii.dir.root <- paste0(data.dir, "ascii-files/areasdunes/")


for(areadune in areasdunes){
  print(areadune)
  ####Load genetic distance####
  dist.v <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_PCA16.RDS"))
  #coordinates of the data
  coord.samples <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_coordsamples.RDS"))
  
  #load vector with all combinations ofr this dunearea
  combs_dunearea <- readRDS(file= paste0(data.dir, "vector-combinations/comb_", areadune, ".RDS"))
  #print(combs_dunearea)
  #get categories for this run (from input data subpart_int)
  category <- combs_dunearea[cat_n]
  
  ##Preparing variables and directories for resistanceGA run####
  ###give ascii-dir####
  ascii.dir <- paste0(data.dir, "ascii-files/areasdunes/SS_MC/", areadune, "/", resolution, "m/", category, "/")

  ###give directory for output results####
  output.dir.areadune <- paste0(output.dir, areadune, "/", as.character(resolution), "m/", category, "/")
  #check if folders exists for output, otherwise, create
  if (file.exists(output.dir.areadune)) {cat("The folder already exists")
  } else {dir.create(output.dir.areadune, recursive=T)}


  print(category)
  print(ascii.dir)


  #prep genetic algorithm: specify all the details and options of how you want to conduct optimization with the genetic algorithm
  GA.inputprep <- GA.prep(ASCII.dir = ascii.dir,
                          Results.dir = output.dir.areadune, #"all.comb"
                          max.cat = 2500,
                          method = "LL", #loglikelihood default
                          maxiter = n_maxiter,#default=1000, just to be able to check if it runs, 2 iters done
                          parallel = 1 #parallel::detectCores() Don't use them all!
  )

  #prep genetic distance
  gdist.inputprep <- gdist.prep(n.Pops = nrow(coord.samples),
                                response = dist.v,
                                samples=coord.samples,
                                method="costDistance")


  ###Run the optimization# for multiple surface####
  tic(paste("Multiple categories", areadune, resolution, category, sep= ", "))
  SS_results.gdist <- SS_optim(gdist.inputs = gdist.inputprep,
                               GA.inputs = GA.inputprep)
  saveRDS(SS_results.gdist, file = paste0(output.dir.areadune, "Results/", "results.gdist.RDS"))
  toc()
}


