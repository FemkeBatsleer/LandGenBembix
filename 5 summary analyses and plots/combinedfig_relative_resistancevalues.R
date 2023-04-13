library(dplyr)
library(tidyr)
library(data.table) #%like%
library(stringr)
library(ggplot2)
library(gridExtra)
library(gridtext)
library(ggpubr)
library(cowplot)

outputs.dir.su <- "../outputs/summaries/"

###complete study area----
resval.scaled.wc.run1.df <- read.csv(paste0(outputs.dir.su, "resistance_values_scaled_run1.csv")) %>% mutate(run = "run1")
resval.scaled.wc.run2.df <- read.csv(paste0(outputs.dir.su, "resistance_values_scaled_run2.csv")) %>% mutate(run = "run2")
resval.scaled.wc.df <- resval.scaled.wc.run1.df %>% bind_rows(resval.scaled.wc.run2.df) %>% 
  mutate(category = factor(category, levels=c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else"))) %>%
  mutate(category.focal = factor(category.focal, levels = c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else")))



boxplot_anthro <- ggplot(resval.scaled.wc.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.wc.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
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

# grid.arrange(boxplot_anthro,as_ggplot(get_legend(boxplot_legend)), boxplot_natural, #
#              nrow=2, layout_matrix = rbind(c(1, 1, 1, 2), c(3, 3, 3, 3)),
#              left=yleft, bottom=bottom)

grid.arrange(arrangeGrob(boxplot_anthro,as_ggplot(get_legend(boxplot_legend)), ncol=2, nrow=1, widths=c(3.26,1)),
             arrangeGrob(boxplot_natural, ncol=1, nrow=1), #
             nrow=2, #widths=c(2,1),
             left=yleft, bottom=bottom)


###Dune areas----
resval.scaled.ad.run1.df <- read.csv(paste0(outputs.dir.su, "resistance_values_scaled_AD_run1.csv")) %>% mutate(run = "run1")
resval.scaled.ad.run2.df <- read.csv(paste0(outputs.dir.su, "resistance_values_scaled_AD_run2.csv")) %>% mutate(run = "run2")
resval.scaled.ad.df <- resval.scaled.ad.run1.df %>% bind_rows(resval.scaled.ad.run2.df) %>% 
  mutate(category = factor(category, levels=c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else"))) %>%
  mutate(category.focal = factor(category.focal, levels = c("urban", "water", "agric", "beach", "scrub", "trees", "opend", "all_else")))


###Westhoek====
areadune <- "westhoek"
resval.scaled.ad.areadune.df <- resval.scaled.ad.df %>% dplyr::filter(area==areadune)

boxplot_anthro <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("urban", "water", "agric"))),
                         aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
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
                         aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#3bab03", "#4c7300", "#ffffbe"))+
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
                         aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
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
                         aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
  facet_wrap(~category.focal)+guides(fill="none")+
  #xlab("category") + ylab("Relative resistance value")+
  theme_bw()+ theme(text = element_text(size=16), strip.text.x = element_text(size=16), 
                    axis.title.x = element_blank(), axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

boxplot_natural <- ggplot(resval.scaled.ad.areadune.df %>% filter(category !="all_else" & (category.focal %in% c("beach", "scrub", "trees", "opend"))),
                          aes(y=value.resc, x=category, fill=interaction(category, run))) +
  geom_boxplot()+
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  scale_fill_manual(values = c("#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe",
                               "#5a5a5a", '#00afff', "#ffff7d", "#dfa839", "#3bab03", "#4c7300", "#ffffbe"))+
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