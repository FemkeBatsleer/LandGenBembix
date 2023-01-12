
#area_int <- 1
#subpart_int <- 1
data.dir <- "D:/Fbatslee/LandGenData/Github-files/" #local
output.dir <- "D:/Fbatslee/LandGenData/Github-files/outputs/" #local

library(ResistanceGA)
library(raster)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(naniar)
library(tibble)
library(tictoc)


#for areasdunes----
#define area according to what is given in args
areas_v <- c("westhoek", "cabour", "doornpanne", "teryde")
#area_char <- areas_v[area_int]
#areasdunes <- c(area_char)

ascii.dir.root <- paste0(data.dir, "ascii-files/areasdunes/")
# #define category according to what is given in args
# categories_v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")
# category_char <- categories_v[cat_int]
# categories.v <- c(category_char)

#Go through all possible combinations
categories.single.list <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")#rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))


resolution <- 20
for(area_char in areas_v){
  print(area_char)
  categories.single.exist <- c()
  for(category in categories.single.list){
    #if class does not exist, not taken into account ot make combinations
    asc.category <- read.table(paste0(ascii.dir.root, "SS_SC/", area_char, "/", resolution, "m/",
                                      category, "/", category, ".asc"), header=F, skip=6)# %>% replace_with_na_all(condition = ~.x == -9999)
    if(any(asc.category==1, na.rm=TRUE)){
      categories.single.exist <- append(categories.single.exist, category)}
  }
  categories.single.exist
  
  categories.multi.all.list <- c()
  for(cat_ncomb in c(2:length(categories.single.exist))){
    categories.multi.list <- combn(categories.single.exist, cat_ncomb)
    #print(categories.multi.list)
    categories.v <- c()
    #go through all possible combinations of categories
    for(n_col in c(1:ncol(categories.multi.list))){
      name_comb <- paste(categories.multi.list[,n_col], collapse=".")
      categories.v <- append(categories.v, name_comb)
    }
    #print(categories.v)
    categories.multi.all.list <- append(categories.multi.all.list, categories.v)
  }
  #print(categories.multi.all.list)
  print(length(categories.multi.all.list))
  
  saveRDS(categories.multi.all.list, file = paste0("../data/vector-combinations/comb_", area_char, ".RDS"))
}

#for westcoast----
resolution <- 50
ascii.dir.root <- paste0(data.dir, "ascii-files/westcoast/")
# #define category according to what is given in args
# categories_v <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")
# category_char <- categories_v[cat_int]
# categories.v <- c(category_char)

#Go through all possible combinations
categories.single.list <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")#rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))


categories.single.exist <- c()
for(category in categories.single.list){
  #if class does not exist, not taken into account ot make combinations
  asc.category <- read.table(paste0(ascii.dir.root, "SS_SC/", resolution, "m/",
                                    category, "/", category, ".asc"), header=F, skip=6)# %>% replace_with_na_all(condition = ~.x == -9999)
  if(any(asc.category==1, na.rm=TRUE)){
    categories.single.exist <- append(categories.single.exist, category)}
}
categories.single.exist

categories.multi.all.list <- c()
for(cat_ncomb in c(2:length(categories.single.exist))){
  categories.multi.list <- combn(categories.single.exist, cat_ncomb)
  #print(categories.multi.list)
  categories.v <- c()
  #go through all possible combinations of categories
  for(n_col in c(1:ncol(categories.multi.list))){
    name_comb <- paste(categories.multi.list[,n_col], collapse=".")
    categories.v <- append(categories.v, name_comb)
  }
  #print(categories.v)
  categories.multi.all.list <- append(categories.multi.all.list, categories.v)
}
print(categories.multi.all.list)
length(categories.multi.all.list)
saveRDS(categories.multi.all.list, file = paste0("../data/vector-combinations/comb_westcoast.RDS"))
