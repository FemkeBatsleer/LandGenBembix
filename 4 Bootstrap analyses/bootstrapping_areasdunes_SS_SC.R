###Aim: areasdunes bootstrapping post hoc from results run on hpc###
#by: Femke Batsleer

#bootstrapping reruns the linear model, but only with part of the data, and then checks how consistently the pattern/results are
#bootstrapping is done for each separate study area (each dune area or westcoast)

hpc.bool <- F

if(hpc.bool==TRUE){
  n.iter <- 1000
  resolution <- 20
  data.dir <- "./data/" #hpc
  runs.dir <- "./data/outputs/outputs_SS_SC_areasdunes_17102022/"
  output.dir <- "./data/outputs/bootstrapping/" #hpc #
} else{
  resolution <- 20
  n.iter <- 2
  data.dir <- "D:/Fbatslee/LandGenData/Github-files/" #local
  runs.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_SC_areasdunes_17102022/"#define folders where outputs are situated
  output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/bootstrapping/" #local
}


library(dplyr)
library(tidyr)
library(ResistanceGA)
library(tictoc)



#define resolution, areas, categories

areasdunes.v <- c("westhoek", "cabour", "doornpanne", "teryde")
categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")

#for each areadune----
for(areadune in areasdunes.v){
  
  ##Recreate genetic distance response====
  #Load genetic distance#
  dist.v <- readRDS(file = paste0(data.dir, paste0("genetic_distances/areasdunes/", areadune, "_PCA16.RDS")))
  #coordinates of the data
  coord.samples <- readRDS(file = paste0(data.dir, "genetic_distances/areasdunes/", areadune, "_coordsamples.RDS"))
  #create square distance matrix for response for use with the bootstrap function
  response <- matrix(0, nrow(coord.samples), nrow(coord.samples))
  response[lower.tri(response)] <- dist.v
  
  
  ##Get optimization results (resistance distance matrices & k-values) for all categories====
  mat.list <- c()
  k.df <- data.frame()
  #load data for each category
  for(category in categories.v){
    cat.dir <- paste0(runs.dir, areadune, "/", resolution, "m/", category)
    if(!file.exists(cat.dir)){ #if category not present, go to next iteration
      next}
    #load results-rds to get cost/resistance distance matrices
    results.rds <- readRDS(paste0(cat.dir, "/Results/results.gdist.RDS"))
    #add to data-lists of mat.list and k.df
    if(length(mat.list)==0){mat.list <- append(mat.list, results.rds$cd)} else{mat.list <- append(mat.list, results.rds$cd[1])}
    if(nrow(k.df)==0){k.df <- k.df %>% bind_rows(results.rds$k)} else{k.df <- k.df %>% bind_rows(results.rds$k[1,])}
  }
  
  
  ##Run the bootstrapping for focal area for all categories together====
  tic(paste0("bootstrapping ", areadune))
  AIC.boot <- Resist.boot(mod.names = names(mat.list),
                          dist.mat = mat.list,
                          n.parameters = k.df[,2],
                          sample.prop = 0.75,
                          iters = n.iter,
                          obs = nrow(coord.samples),
                          genetic.mat = response)
  toc()
  print(AIC.boot)
  write.csv(AIC.boot, file = paste0(output.dir, "ResultsBootstrapping_", areadune, "_", n.iter, "it_", "SS_SC_17102022.csv"))
}
