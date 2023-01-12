#script to calculate weight per model and calculate weight of each variable
#weights etc based on https://doi.org/10.1016/j.tree.2003.10.013
#by: Femke Batsleer


library(dplyr)
library(tidyr)
library(data.table) #%like%
library(stringr)
library(ggplot2)
library(gridExtra)
library(gridtext)
library(ggpubr)
library(cowplot)


outputs.dir.bs <- "D:/Fbatslee/LandGenData/Github-files/outputs/bootstrapping/"
outputs.dir.su <- "D:/Fbatslee/LandGenData/Github-files/outputs/summaries/"
data.dir <- "D:/Fbatslee/LandGenData/"


#westcoast MC----
##model averaging====
su.wc.mc <- read.csv(paste0(outputs.dir.su, "/summary_westcoast_MC_31102022.csv"), sep=",", header=T) %>% rename("surface"="Surface") #load summary results
bs.wc.mc <- read.csv(paste0(outputs.dir.bs, "/ResultsBootstrapping_westcoast_1000it_SS_MC_31102022.csv"), sep=",", header=T) #load bootstrapping results

#combine datasets
wc.mc <- bs.wc.mc %>% left_join(dplyr::select(su.wc.mc, starts_with(c("surface", "Feature"))), by = c("surface")) %>% #add resistance values from summary table
  mutate(DeltaAICc = avg.AICc-min(avg.AICc)) %>% #make a DeltaAICc variable
  #top_n(-5, DeltaAICc) %>% #get top 5 according to DeltaAICc
  mutate(cat_type = "MC", area = "westcoast")
head(wc.mc)

#calculate Akaike weight per model
# wc.mc <- wc.mc %>% mutate(w_akaike_nom = exp(-DeltaAICc/2)) %>% mutate(w_akaike = w_akaike_nom/sum(w_akaike_nom))
# head(wc.mc)
#sum weights per category
categories.v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")
weights.per.cat.wc <- data.frame()
sumweights.cat.wc <- data.frame()
for(category in categories.v){
  wc.mc.cat <- wc.mc %>% dplyr::filter(surface %like% category) #select all models with category in it
  cat.sumweight <- sum(wc.mc.cat$avg.weight)#calculate the sum of the Akaike's weights
  sumweights.cat.wc <- sumweights.cat.wc %>% bind_rows(data.frame(category = category, sumw_akaike = cat.sumweight))
  weights.per.cat.wc <- weights.per.cat.wc %>% bind_rows(data.frame(focal.category = category, wc.mc.cat))
  }

#Results of model averaging per category
arrange(sumweights.cat.wc, -sumw_akaike)
write.csv(arrange(sumweights.cat.wc, -sumw_akaike), paste0(outputs.dir.bs, "sum_akaikeweights_MC_WC.csv"))
# ggplot(weights.per.cat.wc, aes(x=avg.weight, fill=focal.category)) +
#   geom_histogram() +
#   facet_wrap(~focal.category)
# weights.per.cat.wc %>% group_by(focal.category) %>% summarize(avg = mean(avg.weight), median = median(avg.weight))
#write.csv(sumweights.cat.wc, paste0(outputs.dir.bs, "category_modelaveraging_westcoast.csv"))

##Resistance values====
#as resistance values are relative within a model, we should rescale within a model according to the focal category

resval.scaled.wc.df <- data.frame()#initialize dataframe
combs_cat <- readRDS(file= paste0(data.dir, "vector-combinations/comb_", "westcoast", ".RDS")) #load possible combinations of categories
for(cat.focal in categories.v){
  print(cat.focal)
  
  for(cats in combs_cat){
    if(grepl(cat.focal, cats)){ #if cat.focal is present in current combination
      #get focal row and select relevant measures
      row.focal <- wc.mc %>% filter(surface==cats) %>% dplyr::select(surface, starts_with("Feature"))
      
      #create column with the categories of this surface in
      ifelse(cats == "urban.water.agric.beach.scrub.fixed.opend", 0, 1)
      if(cats == "urban.water.agric.beach.scrub.fixed.opend"){
        cats.df <- as.data.frame(str_split(cats, pattern="[.]")) %>% rename ("category" = 1) 
      } else{
        cats.df <- as.data.frame(str_split(paste0("all_else.", cats), pattern="[.]")) %>% rename ("category" = 1) #create column with the categories of this surface in
      }
      #get data of value and categories toegether
      data.model <- row.focal %>% pivot_longer(cols = starts_with("Feature")) %>% drop_na %>% bind_cols(cats.df) 
      #get value of focal category, and then rescale according to this value
      value.focal.cat <- as.numeric(data.model %>% dplyr::filter(category==cat.focal) %>% dplyr::select(value))  
      data.model.resc <- data.model %>% mutate(value.resc = value - value.focal.cat) %>%
        mutate(category.focal = cat.focal)
      #put it in dataframe
      resval.scaled.wc.df <- resval.scaled.wc.df %>% bind_rows(data.model.resc)
    }
  }
}

#making boxplots for these resistance values

resval.scaled.wc.df <- resval.scaled.wc.df %>%
  mutate(category = replace(category, category=="fixed", "trees")) %>%
  mutate(category.focal = replace(category.focal, category.focal=="fixed", "trees")) %>%
  mutate(category = factor(category,
                           levels=c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else"))) %>%
  mutate(category.focal = factor(category.focal,
                                 levels = c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else")))


boxplot_anthro <- ggplot(resval.scaled.wc.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.wc.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_legend <- ggplot(resval.scaled.wc.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+#guides(fill="none")+
  xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
get_legend(boxplot_legend)

yleft = richtext_grob("<span style='font-size:18pt'>Relative resistance value</span>", rot=90)
bottom = richtext_grob("<span style='font-size:18pt'>category</span>")

grid.arrange(arrangeGrob(boxplot_anthro,as_ggplot(get_legend(boxplot_legend)), ncol=2, nrow=1, widths=c(3.26,1)),
             arrangeGrob(boxplot_natural, ncol=1, nrow=1), #
             nrow=2, #widths=c(2,1),
             left=yleft, bottom=bottom)

#areasdunes MC----
##model averaging====
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

#calculate summed Akaike weights
sumweights.cat.ad <- data.frame()
for(category in categories.v){
  ad.mc.cat <- ad.mc.all %>% dplyr::filter(surface %like% category) #select all models with category in it
  cat.sumweight <- ad.mc.cat %>% group_by(area) %>% summarise(sumw_akaike = sum(avg.weight)) %>% #calculate the sum of the Akaike's weights
    mutate(category = category)
  sumweights.cat.ad <- sumweights.cat.ad %>% bind_rows(cat.sumweight)

}
arrange(as.data.frame(sumweights.cat.ad) %>% group_by(area), -sumw_akaike, by_group = T)
#write.csv(sumweights.cat.ad, paste0(outputs.dir.bs, "sum_akaikeweights_MC_areasdunes.csv"))

##Resistance Values====
#Rescale resistance values per model, per category, per areadune

resval.scaled.ad.df <- data.frame()#initialize dataframe
for(areadune in areasdunes.v){
  print(areadune)
  combs_cat <- readRDS(file= paste0(data.dir, "vector-combinations/comb_", areadune, ".RDS")) #load possible combinations of categories
  
  for(cat.focal in categories.v){
    #get the row/value for the single surfaces
    print(cat.focal)
      #get value for this category for the multiple surfaces
      for(cats in combs_cat){
        if(grepl(cat.focal, cats)){ #if cat.focal is present in current combination
          #get focal row and select relevant measures
          row.focal <- ad.mc.all %>% filter(area==areadune & surface==cats) %>% dplyr::select(surface, starts_with("Feature"))
          #create column with the categories of this surface in
          if((areadune=="westhoek" & cats == "urban.beach.scrub.fixed.opend") | (areadune=="cabour" & cats=="urban.water.agric.scrub.fixed.opend") |
             ((!areadune %in% c("westhoek", "cabour")) & cats=="urban.water.agric.beach.scrub.fixed.opend")){
              cats.df <- as.data.frame(str_split(cats, pattern="[.]")) %>% rename("category" = 1)
            } else{
              cats.df <- as.data.frame(str_split(paste0("all_else.", cats), pattern="[.]")) %>% rename("category"=1)
            }
          
          #get data of value and categories toegether
          data.model <- row.focal %>% pivot_longer(cols = starts_with("Feature")) %>% drop_na %>% bind_cols(cats.df) 
          #get value of focal category, and then rescale according to this value
          value.focal.cat <- as.numeric(data.model %>% dplyr::filter(category==cat.focal) %>% dplyr::select(value))  
          data.model.resc <- data.model %>% mutate(value.resc = value - value.focal.cat) %>%
            mutate(category.focal = cat.focal)
          #put it in dataframe
          resval.scaled.ad.df <- resval.scaled.ad.df %>% bind_rows(data.frame("area"=areadune, data.model.resc))
        }
    }
    
    } }


#boxplots per areadune
resval.scaled.ad.df <- resval.scaled.ad.df %>%
  mutate(category = replace(category, category=="fixed", "trees")) %>%
  mutate(category.focal = replace(category.focal, category.focal=="fixed", "trees")) %>%
  mutate(category = factor(category,
                           levels=c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else"))) %>%
  mutate(category.focal = factor(category.focal,
                                 levels = c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else")))


###Westhoek====
areadune <- "westhoek"
resval.scaled.ad.areadune.df <- resval.scaled.ad.df %>% dplyr::filter(area==areadune)

boxplot_anthro <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_legend <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+#guides(fill="none")+
  xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
#get_legend(boxplot_legend)

yleft = richtext_grob("<span style='font-size:18pt'>Relative resistance value</span>", rot=90)
bottom = richtext_grob("<span style='font-size:18pt'>category</span>")

grid.arrange(arrangeGrob(boxplot_anthro, as_ggplot(get_legend(boxplot_legend)), ncol=2, nrow=1, widths=c(1.52,1)),
             arrangeGrob(boxplot_natural, ncol=1, nrow=1), #
             nrow=2, layout_matrix = rbind(c(1, 1, NA, NA), c(2,2,2,2)),
             left=yleft, bottom=bottom)

###Cabour====
areadune <- "cabour"
resval.scaled.ad.areadune.df <- resval.scaled.ad.df %>% dplyr::filter(area==areadune)

boxplot_anthro <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_legend <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+#guides(fill="none")+
  xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
#get_legend(boxplot_legend)

yleft = richtext_grob("<span style='font-size:18pt'>Relative resistance value</span>", rot=90)
bottom = richtext_grob("<span style='font-size:18pt'>category</span>")

grid.arrange(arrangeGrob(boxplot_anthro,as_ggplot(get_legend(boxplot_legend)), ncol=2, nrow=1, widths=c(3, 1)),#, widths=c(3.26,1)
             arrangeGrob(boxplot_natural, ncol=1, nrow=1), #
             nrow=2, layout_matrix = rbind(c(1, 1, 1, 1), c(2,2,2,NA)),
             left=yleft, bottom=bottom)
###Doornpanne====
areadune <- "doornpanne"
resval.scaled.ad.areadune.df <- resval.scaled.ad.df %>% dplyr::filter(area==areadune)

boxplot_anthro <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_legend <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+#guides(fill="none")+
  xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
#get_legend(boxplot_legend)

yleft = richtext_grob("<span style='font-size:18pt'>Relative resistance value</span>", rot=90)
bottom = richtext_grob("<span style='font-size:18pt'>category</span>")

grid.arrange(arrangeGrob(boxplot_anthro,as_ggplot(get_legend(boxplot_legend)), ncol=2, nrow=1, widths=c(3.26,1)),
             arrangeGrob(boxplot_natural, ncol=1, nrow=1), #
             nrow=2, #widths=c(2,1),
             left=yleft, bottom=bottom)

###TerYde====
areadune <- "teryde"
resval.scaled.ad.areadune.df <- resval.scaled.ad.df %>% dplyr::filter(area==areadune)

boxplot_anthro <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_legend <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                         aes(y=value.resc, x=category, fill=category)) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal, nrow=1)+#guides(fill="none")+
  xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
#get_legend(boxplot_legend)

yleft = richtext_grob("<span style='font-size:18pt'>Relative resistance value</span>", rot=90)
bottom = richtext_grob("<span style='font-size:18pt'>category</span>")

grid.arrange(arrangeGrob(boxplot_anthro,as_ggplot(get_legend(boxplot_legend)), ncol=2, nrow=1, widths=c(3.26,1)),
             arrangeGrob(boxplot_natural, ncol=1, nrow=1), #
             nrow=2, #widths=c(2,1),
             left=yleft, bottom=bottom)
