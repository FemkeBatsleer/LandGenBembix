###Aim: summarize the results of the runs (on hpc) of the resistanceGA optimizations###
#by: Femke Batsleer

resolution <- 20

library(dplyr)
library(tidyr)

runs.SC.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_SC_areasdunes_17102022/"
runs.MC.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_MC_areasdunes_31102022/"

data.dir <- "D:/Fbatslee/LandGenData/Github-files/"
output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/summaries/"

# areadune <- "westhoek"
areasdunes.v <- c("westhoek", "cabour", "doornpanne", "teryde")
categories.v <- rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))


all.results.df <- data.frame() #initialize to add in loop the data
#combine results for all areas
for(areadune in areasdunes.v){
  #combinations per areadune
  combs_cat <- readRDS(file= paste0(data.dir, "vector-combinations/comb_", areadune,".RDS"))
  
  modelselection.results.df <- data.frame()#initialize
  resistance.results.df <- data.frame()
  
  ##first add single categories in loop====
  for(category in categories.v){
    #load results
    path.results <- paste0(runs.SC.dir, areadune, "/", resolution, "m/", category, "/", "Results")
    print(path.results)
    if (!file.exists(paste0(path.results, "/results.gdist.RDS"))) {
      print(paste0("Category ", category, " doesn't have results or does not exist for this area"))
      next
    }
    modelselection.results.cat.df <- read.csv(file = paste0(path.results, "/All_Results_Table.csv"), header=T)
    resistance.results.cat.df <- read.csv(file = paste0(path.results, "/CategoricalResults.csv"), header=T)
    #add them to dataframes
    modelselection.results.df <- modelselection.results.df %>% bind_rows(modelselection.results.cat.df)
    resistance.results.df <- resistance.results.df %>% bind_rows(resistance.results.cat.df)
  }
  
  ##second add multiple categories in loop====
  for(category in combs_cat){
    #load results
    path.results <- paste0(runs.MC.dir, areadune, "/", resolution, "m/", category, "/", "Results")
    print(path.results)
    if (!file.exists(paste0(path.results, "/results.gdist.RDS"))) {
      print(paste0("Category ", category, " doesn't have results or does not exist for this area"))
      next
    }
    modelselection.results.cat.df <- read.csv(file = paste0(path.results, "/All_Results_Table.csv"), header=T)
    resistance.results.cat.df <- read.csv(file = paste0(path.results, "/CategoricalResults.csv"), header=T)
    #add them to dataframes
    modelselection.results.df <- modelselection.results.df %>% bind_rows(modelselection.results.cat.df)
    resistance.results.df <- resistance.results.df %>% bind_rows(resistance.results.cat.df)
  }

    #add DeltaAIC and DeltaLL
  modelselection.results.df <- modelselection.results.df %>% distinct() %>%
    mutate(DeltaAIC = AIC - min(AIC),
           DeltaAICc = AICc - min(AICc),
           DeltaLL = max(LL) - LL) %>%
   #add resistance values to model selection results
    left_join((resistance.results.df %>% dplyr::select(starts_with(c("Surface", "Feature")))), by="Surface") %>%
    #add column with dunearea
    mutate(dunearea = areadune)
  #add data from dune area to complete dataset
  all.results.df <- all.results.df %>% bind_rows(modelselection.results.df)
}

all.results.df
write.csv(all.results.df, file=paste0(output.dir, "summary_areasdunes_MC_27122022.csv"))
