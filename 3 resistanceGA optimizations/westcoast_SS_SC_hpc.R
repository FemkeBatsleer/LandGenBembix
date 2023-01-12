###Script to run westcoast optimization (complete study area)###
##WARNING: optimization can take very long! First try one iteration before upscaling
# #By: Femke Batsleer

hpc.bool <- FALSE#boolean if script is run on hpc or not, to get data correct

if(hpc.bool==TRUE){#if run on hpc: arguments are given to script in commend line, other data directories
  args <- commandArgs(TRUE)
  resolution <- as.numeric(as.character(args[1]))
  cat_int <- as.numeric(as.character(args[2]))
  data.dir <- "./data/" #hpc
  output.dir <- "./data/outputs/SS_SC/" #hpc #
} else{
  resolution <- 50
  cat_int <- 1 #which category to run 1:7
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
areadune <- "westcoast"

#single surface categories
categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")#rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))
cat.char <- categories.v[cat_int]
cat.v <- c(cat.char)

####Load genetic distance####
dist.v <- readRDS(file = paste0(data.dir, "genetic_distances/westcoast/", areadune, "_PCA16.RDS"))
#coordinates of the data
coord.samples <- readRDS(file = paste0(data.dir, "genetic_distances/westcoast/", areadune, "_coordsamples.RDS"))

for(category in cat.v){
  ###Preparing variables and directories for resistanceGA run####
  ####give ascii-dir####
  ascii.dir <- paste0(data.dir, "ascii-files/westcoast/SS_SC/", resolution, "m/", category, "/")
  
  #if class does not exist in that area, go to next iteration (skip this optimization for particular category)
  asc.category <- read.table(paste0(ascii.dir, category, ".asc"), header=F, skip=6)# %>% replace_with_na_all(condition = ~.x == -9999)
  if(!any(asc.category==1, na.rm=TRUE)){
    #print(paste0("cateogry ", category, " doesn't exist in the raster file"))
    next}
  
  ###give directory for output results####
  output.dir.areadune <- paste0(output.dir, areadune,  "/", as.character(resolution), "m/", category, "/")
  #check if folders exists for output, otherwise, create
  if (file.exists(output.dir.areadune)) {cat("The folder already exists")
  } else {dir.create(output.dir.areadune, recursive=T)}
  
  
  print(category)
  
  
  #prep genetic algorithm: specify all the details and options of how you want to conduct optimization with the genetic algorithm
  GA.inputprep <- GA.prep(ASCII.dir = ascii.dir,
                          Results.dir = output.dir.areadune, #"all.comb"
                          max.cat = 2500,
                          method = "LL", #loglikelihood default
                          maxiter = n_maxiter,#set to 300, 2 is to check if it runs #default=1000
                          parallel = 1 #parallel::detectCores() Don't use them all! -2
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
