###Aim: summarize the results of the runs (on hpc) of the resistanceGA optimizations###
#by: Femke Batsleer

resolution <- 50

library(dplyr)
library(tidyr)

runs.dir <- "D:/Fbatslee/LandGenData/outputs/outputs_SS_SC_westcoast_17102022/"#"D:/Fbatslee/LandGenData/outputs/outputs_SS_SC_westcoast_17102022/"
output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/summaries/"#"D:/Fbatslee/LandGenData/outputs/summaries/"

categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")

#combine results for all areas
modelselection.results.df <- data.frame()#initialize
resistance.results.df <- data.frame()
for(category in categories.v){
  #load results
  path.results <- paste0(runs.dir, "westcoast/", resolution, "m/", category, "/", "Results")
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
  left_join((resistance.results.df %>% dplyr::select(starts_with(c("Surface", "Feature")))), by="Surface")
modelselection.results.df

write.csv(modelselection.results.df , file=paste0(output.dir, "summary_westcoast_SC_17102022.csv")) #"summary_westcoast_SC_17102022
