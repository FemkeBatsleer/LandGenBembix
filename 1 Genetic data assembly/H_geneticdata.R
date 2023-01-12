###Aim script: create helper functions to load & select genetic data for use in resistanceGA
###including calculating genetic distances
#By: Femke Batsleer

library(adegenet)
library(dplyr)
library(tidyr)
library(ecodist)

####functions to get data from genind object####
#return a genind object
get_gendata.genind <- function(genind.dir = "../data/genetic_data_raw/wc2018_genind_pop.RDS", #directory of RDS-file which holds genind object
                               ratio.select = 1, #ratio of how many individuals should be randomly drawn
                               seed #set seed argument, if random selection needs to be reproducible
){
  #load data from directory with RDS-file
  gendata <- readRDS(file=genind.dir)
  
  #set seed
  if(exists("seed")){set.seed(seed)}
  
  #make selection of data
  random.set.v <- sample(1:nInd(gendata), round(ratio.select * nInd(gendata)), replace=F)
  gendata.select <- gendata[random.set.v,] #make selection
  
  return(gendata.select)
}

# #function to get coordinates from genind object
# get_coordinates <- function(genind){
#   ###Retrieve sample location coordinates from genind object
#   points <- gendata$other$xy
#   ### define the coordinates as spatial points
#   samples.coord.m <- as.matrix(points[,c(1,2)])
#   
#   return(samples.coord.m)
# }


####functions to calculate genetic distances####
#returns a vector of the dissimilarities
#Dps: proportion of shared alleles
calc_gendistance <- function(genind, #a genind object, including xy coordinates in at 'other'
                             type="Dps", #choose between Dps: prop shared alleles, XXXXX upcoming!
                             n.axes = 16 #if type=PCA, choose number of PC's to be retained
){
  if(type=="Dps"){ ###Proportion of shared alleles
    
    Dps.m <- propShared(genind) #matrix-object, function from pacakge adegenet
    #retrieve the upper part and the diagonal of the matrix to get only the lower part
    Dps.m[upper.tri(Dps.m, diag=TRUE)] <-NA
    #change distance matrix into vector to be used in resistanceGA, delete the NAs from the symmetric matrix
    Dps.v.na <- as.vector(Dps.m)
    Dps.v <- Dps.v.na[!is.na(Dps.v.na)]
    length(Dps.v) == nInd(genind)*(nInd(gendata)-1)/2 #check if correct #pairwise distances
    #make a dissimilarity, as resistanceGA works with dissimilarities
    distance.v <- 1-Dps.v
    
  } else if(type=="PCA"){
    pca.gendata <- dudi.pca(tab(genind, NA.method="mean"), scannf=F, nf=n.axes)
    # 
    # col <- funky(length(unique(pop(gendata))))
    # s.class(pca.gendata$li, pop(gendata), col=transp(col,.6), clabel=0.6)
    pc.gendata <- pca.gendata$li
    distance.m <- distance(pc.gendata, method="euclidean")
    distance.v <- as.vector(distance.m)
  }
  
  else{
    distance.v <- NULL
    print("No correct type of genetic distance was provided, NULL is returned")
  }
  
  return(distance.v) #vector of dissimilarities is returned
}

calc_gendistance_matrix <- function(genind, #a genind object, including xy coordinates in at 'other'
                             type="Dps", #choose between Dps: prop shared alleles, XXXXX upcoming!
                             n.axes = 16 #if type=PCA, choose number of PC's to be retained
){
  if(type=="Dps"){ ###Proportion of shared alleles
    
    Dps.m <- propShared(genind) #matrix-object, function from pacakge adegenet
    #retrieve the upper part and the diagonal of the matrix to get only the lower part
    Dps.m[upper.tri(Dps.m, diag=TRUE)] <-NA
    distance.m <- Dps.m
    #change distance matrix into vector to be used in resistanceGA, delete the NAs from the symmetric matrix
    Dps.v.na <- as.vector(Dps.m)
    Dps.v <- Dps.v.na[!is.na(Dps.v.na)]
    length(Dps.v) == nInd(genind)*(nInd(gendata)-1)/2 #check if correct #pairwise distances
    #make a dissimilarity, as resistanceGA works with dissimilarities
    distance.v <- 1-Dps.v
    
  } else if(type=="PCA"){
    pca.gendata <- dudi.pca(tab(genind, NA.method="mean"), scannf=F, nf=n.axes)
    # 
    # col <- funky(length(unique(pop(gendata))))
    # s.class(pca.gendata$li, pop(gendata), col=transp(col,.6), clabel=0.6)
    pc.gendata <- pca.gendata$li
    distance.m <- distance(pc.gendata, method="euclidean")
    distance.v <- as.vector(distance.m)
  }
  
  else{
    distance.v <- NULL
    print("No correct type of genetic distance was provided, NULL is returned")
  }
  
  return(distance.m) #vector of dissimilarities is returned
}
