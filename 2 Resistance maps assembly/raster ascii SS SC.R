###Aim script###
####convert rasters (from script 'raster raw assembly.R') to ascii-files and order them in folders to be used with resistanceGA

#By: Femke Batsleer

library(raster)
library(sf)
library(terra)

#write down base of the directory to store the ascii files; make sure it is a local folder, not synced with onedrive etc, gives problems with files to delete etc
ascii.dir.base.westcoast <- "D:/Fbatslee/LandGenData/Github-files/ascii-files/westcoast/SS_SC/"
ascii.dir.base.areasdunes <- "D:/Fbatslee/LandGenData/Github-files/ascii-files/areasdunes/SS_SC/"

categories.list <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")
#for run 2: rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend"))

#whole study area----
for(resolution in c("50m")){#define resolution, to let things end up in the correct directory"10m", "20m", 
  ascii.dir.singclass <- paste0(ascii.dir.base.westcoast, resolution, "/")
  # if (file.exists(ascii.dir.singclass)) {cat("The folder already exists")
  #   } else {dir.create(ascii.dir.singclass, recursive = T)}
  
  #load raster with all combined classes
  classes.r <- rast(paste0("../data/raster_aggregated/classes7_", resolution, ".tif"))
  #NAflag(classes.r) <- -9999 #change NA values of raster file, as advised for resistanceGA
  classes.r[is.na(classes.r)] <- -9999
  NAflag(classes.r) <- -9999
  #write as ascii file
  #writeRaster(classes.r, filename = paste0(ascii.dir.combclass,"classes10_100m.asc"), overwrite=TRUE)
  
  ##single classes in separate maps====
  #subset each raster with the single categories
  surfaces.list <- list(classes.r==1, classes.r==2, classes.r==3, classes.r==4, classes.r==5,
                        classes.r==6, classes.r==7)
  
  surface.category.list <- list(surfaces.list, categories.list)                      
  
  #create folders if they don't exist yet
  for(category in categories.list){
    dir.to.create <- paste(ascii.dir.base.westcoast, resolution, category, sep="/")
    if (file.exists(dir.to.create)) {cat("The folder already exists")
      } else {dir.create(dir.to.create, recursive=T)}
  }
  
  #write raster into each folder per resolution/cateogry_type/cateogry.asc
  for(i in 1:length(surface.category.list[[1]])){
    name_layer <- surface.category.list[[2]][i]
    raster_layer <- surface.category.list[[1]][[i]]
    print(paste0(ascii.dir.singclass, name_layer, "/", name_layer, ".asc"))
    #write raster in correct folder (either ascii-files or tif-files)
    writeRaster(raster_layer, filename = paste0(ascii.dir.singclass, name_layer, "/", name_layer, ".asc"), overwrite=T)
    #writeRaster(raster_layer, filename = paste0("../data/raster_aggregated/westcoast/single_surfaces/", resolution, "/", name_layer, ".tif"), overwrite=T)
    #delete extra files which were created 'xml' and 'prj' files
    files.to.delete <- list.files(paste0(ascii.dir.singclass, name_layer, "/"), pattern=".xml|.prj", full.names=T)
    file.remove(files.to.delete)
  }
  
}


#For dune areas----
for(dunearea in c("westhoek", "cabour", "teryde", "doornpanne")){
  for(resolution in c("20m")){#"10m", , "50m"
    #directory of the ascii-files
    ascii.dir.singclass <- paste0(ascii.dir.base.areasdunes, dunearea , "/", resolution, "/")
    
    #load raster
    classes.r <- rast(paste0("../data/raster_aggregated/classes7_", resolution, "_", dunearea, ".tif"))
    #NAflag(classes.r) <- -9999 #change NA values of raster file, as advised for resistanceGA
    classes.r[is.na(classes.r)] <- -9999
    NAflag(classes.r) <- -9999
    #write as ascii file
    #writeRaster(classes.r, filename = paste0(ascii.dir.combclass,"classes10_100m.asc"), overwrite=TRUE)
    
    ##single classes in separate maps====
    #subset each raster with the single categories
    surfaces.list <- list(classes.r==1, classes.r==2, classes.r==3, classes.r==4, classes.r==5,
                          classes.r==6, classes.r==7)
    surface.category.list <- list(surfaces.list, categories.list)                      
    
    #create folders if they don't exist yet
    for(category in categories.list){
      dir.to.create <- paste(ascii.dir.base.areasdunes, dunearea, resolution, category, sep="/") #"../data/ascii-files/areasdunes/dunearea/resolution/category"
      if (file.exists(dir.to.create)) {cat("The folder already exists")
        } else {dir.create(dir.to.create, recursive=T)}
    }
    
    #write raster into each folder per resolution/cateogry_type/cateogry.asc
    for(i in 1:length(surface.category.list[[1]])){
      name_layer <- surface.category.list[[2]][i]
      raster_layer <- surface.category.list[[1]][[i]]
      print(paste0(ascii.dir.singclass, name_layer, "/", name_layer, ".asc"))
      #write raster in correct folder
      writeRaster(raster_layer, filename = paste0(ascii.dir.singclass, name_layer, "/", name_layer, ".asc"), NAflag=-9999, overwrite=T)
      #writeRaster(raster_layer, filename = paste0("../data/raster_aggregated/areasdunes/single_surfaces/", resolution, "/", name_layer, ".tif"), overwrite=T)
      #delete extra files which were created 'xml' and 'prj' files
      files.to.delete <- list.files(paste0(ascii.dir.singclass, name_layer, "/"), pattern=".xml|.prj", full.names=T)
      file.remove(files.to.delete)
    }
  }
}

