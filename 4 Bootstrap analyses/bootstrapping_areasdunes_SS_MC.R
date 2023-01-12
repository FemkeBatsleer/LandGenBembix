###Aim: areasdunes bootstrapping post hoc from results run on hpc###
#by: Femke Batsleer

#bootstrapping reruns the linear model, but only with part of the data, and then checks how consistently the pattern/results are
#bootstrapping is done for each separate study area (each dune area or westcoast)

hpc.bool <- F

if(hpc.bool==TRUE){
  n.iter <- 1000
  resolution <- 20
  data.dir <- "./data/" #hpc
  runs.SC.dir <- "./data/outputs/outputs_SS_SC_areasdunes_17102022/"
  runs.MC.dir <- "./data/outputs/outputs_SS_MC_areasdunes_31102022/"
  output.dir <- "./data/outputs/bootstrapping/" #hpc #
} else{
  resolution <- 20
  n.iter <- 2
  data.dir <- "D:/Fbatslee/LandGenData/Github-files/" #local
  runs.SC.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_SC_areasdunes_17102022/"#define folders where outputs are situated
  runs.MC.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_MC_areasdunes_31102022/"
  output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/bootstrapping/" #local
}

library(dplyr)
library(tidyr)
library(ResistanceGA)
library(tictoc)

areasdunes.v <- c("westhoek", "cabour", "doornpanne", "teryde")
categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")

for(areadune in areasdunes.v){
  #combinations of categories
  combs_cat <- readRDS(file= paste0(data.dir, "vector-combinations/comb_", areadune, ".RDS"))
  ##Recreate genetic distance response====
  #Load genetic distance#
  dist.v <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_PCA16.RDS"))
  #coordinates of the data
  coord.samples <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_coordsamples.RDS"))
  #create square distance matrix for response for use with the bootstrap function
  response <- matrix(0, nrow(coord.samples), nrow(coord.samples))
  response[lower.tri(response)] <- dist.v
  
  
  ##Get optimization results (resistance distance matrices & k-values) for all categories====
  mat.list <- c()
  k.df <- data.frame()
  #load data for each SC
  for(category in categories.v){
    #print(category)
    results.dir <- paste0(runs.SC.dir, areadune, "/", resolution, "m/", category, "/Results/results.gdist.RDS")
    if(!file.exists(results.dir)){
      print(paste0(results.dir, " does not exist"))#if category not present, go to next iteration
      next}
    #load results-rds to get cost/resistance distance matrices
    results.rds <- readRDS(results.dir)
    #add to data-lists of mat.list and k.df
    if(length(mat.list)==0){mat.list <- append(mat.list, results.rds$cd)} else{mat.list <- append(mat.list, results.rds$cd[1])}
    if(nrow(k.df)==0){k.df <- k.df %>% bind_rows(results.rds$k)} else{k.df <- k.df %>% bind_rows(results.rds$k[1,])}
  }
  #add data for each MC
  for(category in combs_cat){
    #print(category)
    results.dir <- paste0(runs.MC.dir, areadune, "/", resolution, "m/", category, "/Results/results.gdist.RDS")
    if(!file.exists(results.dir)){
      print(paste0(results.dir, " does not exist"))#if category not present, go to next iteration
      next}
    #load results-rds to get cost/resistance distance matrices
    results.rds <- readRDS(results.dir)
    #add to data-lists of mat.list and k.df
    if(length(mat.list)==0){mat.list <- append(mat.list, results.rds$cd)} else{mat.list <- append(mat.list, results.rds$cd[1])}
    if(nrow(k.df)==0){k.df <- k.df %>% bind_rows(results.rds$k)} else{k.df <- k.df %>% bind_rows(results.rds$k[1,])}
  }
  
  
  ##Run the bootstrapping for focal area for all categories together====
  tic(paste0("bootstrapping ", areadune, " MC"))
  AIC.boot <- Resist.boot(mod.names = names(mat.list),
                          dist.mat = mat.list,
                          n.parameters = k.df[,2],
                          sample.prop = 0.75,
                          iters = n.iter,
                          obs = nrow(coord.samples),
                          genetic.mat = response)
  toc()
  AIC.boot
  write.csv(AIC.boot, file = paste0(output.dir, "ResultsBootstrapping_", areadune, "_", n.iter, "it_", "SS_MC_31102022.csv"))
  
}



