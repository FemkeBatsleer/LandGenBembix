#Get bootstrapping results and summary results together and filter the results that are relevant
#to run this, first the scripts summary_areasdunes_SS_*C.R summary_westcoast_SS_*C.R should be rune
#By: Femke Batsleer

library(dplyr)
library(tidyr)

outputs.dir.bs <- "D:/Fbatslee/LandGenData/Github-files/outputs/bootstrapping/"
outputs.dir.su <- "D:/Fbatslee/LandGenData/Github-files/outputs/summaries/"

#westcoast SC----
bs.wc.sc <- read.csv(paste0(outputs.dir.bs, "/ResultsBootstrapping_westcoast_1000it_SS_SC_17102022.csv"), sep=",", header=T) #load bootstrapping results
su.wc.sc <- read.csv(paste0(outputs.dir.su, "/summary_westcoast_SC_17102022.csv"), sep=",", header=T) %>% rename("surface"="Surface") #load summary results

#combine datasets
wc.sc <- bs.wc.sc %>% left_join(dplyr::select(su.wc.sc, starts_with(c("surface", "Feature"))), by = c("surface")) %>% #add resistance values from summary table
  mutate(DeltaAICc = avg.AICc-min(avg.AICc)) %>% #make a DeltaAICc variable
  mutate(cat_type = "SC", area = "westcoast")
  # dplyr::filter(DeltaAICc < 2)#filter out only variables with DeltaAICc < 2
wc.sc

write.csv(wc.sc, file = paste0(outputs.dir.bs, "bootstrapsummary_westcoast_SC.csv")) #get top 5 according to DeltaAICc
# write.csv(wc.sc %>% top_n(-5, DeltaAICc), file = paste0(outputs.dir.bs, "bootstrapsummary_top5AICc_westcoast_SC.csv"))

#westcoast MC----
su.wc.mc <- read.csv(paste0(outputs.dir.su, "/summary_westcoast_MC_31102022.csv"), sep=",", header=T) %>% rename("surface"="Surface") #load summary results
bs.wc.mc <- read.csv(paste0(outputs.dir.bs, "/ResultsBootstrapping_westcoast_1000it_SS_MC_31102022.csv"), sep=",", header=T) #load bootstrapping results

#combine datasets
wc.mc <- bs.wc.mc %>% left_join(dplyr::select(su.wc.mc, starts_with(c("surface", "Feature"))), by = c("surface")) %>% #add resistance values from summary table
  mutate(DeltaAICc = avg.AICc-min(avg.AICc)) %>% #make a DeltaAICc variable
  #top_n(-5, DeltaAICc) %>% #get top 5 according to DeltaAICc
  mutate(cat_type = "MC", area = "westcoast")

write.csv(wc.mc, file = paste0(outputs.dir.bs, "bootstrapsummary_westcoast_MC.csv"))
#write.csv(wc.mc %>% top_n(-5, DeltaAICc), file = paste0(outputs.dir.bs, "bootstrapsummary_top5AICc_westcoast_MC.csv"))
#View(wc.mc %>% dplyr::filter(DeltaAICc < 2))

#areasdunes SC----
#areadune <- "westhoek"
areasdunes.v <- c("westhoek", "cabour", "doornpanne", "teryde")
su.ad.sc.all <- read.csv(paste0(outputs.dir.su, "/summary_areasdunes_SC_17102022.csv"), sep=",", header=T) %>% rename("surface"="Surface") #load summary results

ad.sc.all <- data.frame() #initialize dataframe
for(areadune in areasdunes.v){
  bs.ad.sc <- read.csv(paste0(outputs.dir.bs, "/ResultsBootstrapping_", areadune, "_1000it_SS_SC_17102022.csv"), sep=",", header=T) #load bootstrapping results
  su.ad.sc <- su.ad.sc.all %>% dplyr::filter(dunearea==areadune)
  
  #combine datasets
  ad.sc <- bs.ad.sc %>% left_join(dplyr::select(su.ad.sc, starts_with(c("surface", "Feature"))), by = c("surface")) %>% #add resistance values from summary table
    mutate(DeltaAICc = avg.AICc-min(avg.AICc)) %>% #make a DeltaAICc variable
    mutate(cat_type = "SC", area = areadune)
  
  ad.sc.all <- ad.sc.all %>% bind_rows(ad.sc)
}

#ad.sc.all %>% dplyr::filter(DeltaAICc < 2)
write.csv(ad.sc.all, file = paste0(outputs.dir.bs, "bootstrapsummary_areasdunes_SC.csv"))
# write.csv(ad.sc.all %>% top_n(-5, DeltaAICc), file = paste0(outputs.dir.bs, "bootstrapsummary_top5AICc_areasdunes_SC.csv"))

#areasdunes MC----
areasdunes.v <- c("westhoek", "cabour", "doornpanne", "teryde")
su.ad.mc.all <- read.csv(paste0(outputs.dir.su, "/summary_areasdunes_MC_31102022.csv"), sep=",", header=T) %>% rename("surface"="Surface") #load summary results

ad.mc.all <- data.frame() #initialize dataframe
for(areadune in areasdunes.v){
  bs.ad.mc <- read.csv(paste0(outputs.dir.bs, "/ResultsBootstrapping_", areadune, "_1000it_SS_MC_31102022.csv"), sep=",", header=T) #load bootstrapping results
  su.ad.mc <- su.ad.mc.all %>% dplyr::filter(dunearea==areadune)
  
  #combine datasets
  ad.mc <- bs.ad.mc %>% left_join(dplyr::select(su.ad.mc, starts_with(c("surface", "Feature"))), by = c("surface")) %>% #add resistance values from summary table
    mutate(DeltaAICc = avg.AICc-min(avg.AICc)) %>% #make a DeltaAICc variable
    #top_n(-5, DeltaAICc) %>% #get top 5 according to DeltaAICc
    mutate(cat_type = "MC", area = areadune)
  
  ad.mc.all <- ad.mc.all %>% bind_rows(ad.mc)
}

write.csv(ad.mc.all, file = paste0(outputs.dir.bs, "bootstrapsummary_areasdunes_MC.csv"))
# write.csv(ad.mc.all %>% top_n(-5, DeltaAICc), file = paste0(outputs.dir.bs, "bootstrapsummary_top5AICc_areasdunes_MC.csv"))
#View(ad.mc.all %>% dplyr::filter(DeltaAICc < 2))
