###Aim: summarize the results of the runs (on hpc) of the resistanceGA optimizations###
#by: Femke Batsleer

resolution <- 20

library(dplyr)
library(tidyr)

runs.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_SC_areasdunes_17102022/"
output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/summaries/"

# areadune <- "westhoek"
areasdunes.v <- c("westhoek", "cabour", "doornpanne", "teryde")
categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")#rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))


all.results.df <- data.frame() #initialize to add in loop the data
#combine results for all areas
for(areadune in areasdunes.v){
  ###ADJUST####
  
  modelselection.results.df <- data.frame()#initialize
  resistance.results.df <- data.frame()
  for(category in categories.v){
    #load results
    path.results <- paste0(runs.dir, areadune, "/", resolution, "m/", category, "/", "Results")
    print(path.results)
    if (!file.exists(path.results)) {
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
write.csv(all.results.df, file=paste0(output.dir, "summary_areasdunes_SC_17102022.csv"))
