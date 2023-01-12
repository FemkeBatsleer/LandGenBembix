####Aim script###
####make all possible combinations and put them in the right folder combinations for each dune area and the whole study area

#By: Femke Batsleer


library(raster)
library(sf)
library(terra)

#write down base of the directory to store the ascii files; make sure it is a local folder, not synced with onedrive etc, gives problems with files to delete etc
ascii.dir.SC.westcoast <-"D:/Fbatslee/LandGenData/Github-files/ascii-files/westcoast/SS_SC/"
ascii.dir.SC.areasdunes <- "D:/Fbatslee/LandGenData/Github-files/ascii-files/areasdunes/SS_SC/"
ascii.dir.MC.westcoast <- "D:/Fbatslee/LandGenData/Github-files/ascii-files/westcoast/SS_MC/"
ascii.dir.MC.areasdunes <- "D:/Fbatslee/LandGenData/Github-files/ascii-files/areasdunes/SS_MC/"

#list of all categories
categories.list <- c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")
#for run 2: rev(c("urban", "water", "agric", "beach", "scrub", "fixed", "opend")); also don't forget to create new directories for ascii file storage

#For complete study area westcoast----
##Single surface: categories are combined into one file
for(resolution in c(50)){#"10m", "20m", "50m" --> all resolutions creates a lot of data
  print(paste0("westcoast", " - ", as.character(resolution)))
  #directory of the ascii-files
  ascii.dir.root.SC <- paste0(ascii.dir.SC.westcoast, resolution, "m/") #root of current areadune and resolution files
  ascii.dir.root.MC <- paste0(ascii.dir.MC.westcoast, resolution, "m/")
  
  classes.r <- rast(paste0("../data/raster_aggregated/classes7_", resolution, "m", ".tif"))
  #NAflag(classes.r) <- -9999 #change NA values of raster file, as advised for resistanceGA
  classes.r[is.na(classes.r)] <- -9999
  NAflag(classes.r) <- -9999
  
  
  #make list of not-null classes
  categories.list.exist <- c()
  for(category in categories.list){
    #if class does not exist, not taken into account to make combinations
    asc.category <- read.table(paste0(ascii.dir.root.SC, category, "/", category, ".asc"), header=F, skip=6)# %>% replace_with_na_all(condition = ~.x == -9999)
    if(any(asc.category==1, na.rm=TRUE)){
      categories.list.exist <- append(categories.list.exist, category)}
  }
  #print(categories.list.exist)
  
  #go through all possible combinations of categories
  for(n_sample in c(2:length(categories.list.exist))){
    combs <- combn(categories.list.exist, n_sample)
    #matrix of all combinations
    for(n_col in c(1:ncol(combs))){
      
      name_comb <- paste(combs[,n_col], collapse=".")
      print(name_comb)
      #make combination of raster file and write ascii-file
      classes.sel <- deepcopy(classes.r)
      #make reclassification-vector
      becomes.v <- c() #initialize vector
      for(category in categories.list){
        value_becomes <- ifelse(category %in% combs[,n_col], which(startsWith(combs[,n_col], category)), 0) #if not in focal combs, 0, else its rownumber
        becomes.v <- append(becomes.v, value_becomes)#add to vector
      }
      #print(becomes.v)
      
      reclass.df <- data.frame(is=c(1:length(becomes.v)),
                               becomes = becomes.v)
      
      classes.sel.reclass <- classify(classes.r, reclass.df)
      #plot(classes.sel.reclass)
      dir.comb <- paste0(ascii.dir.root.MC, name_comb, "/") #"../data/ascii-files/areasdunes/dunearea/resolution/category"
      if (file.exists(dir.comb)) {cat("The folder already exists")
      } else {dir.create(dir.comb, recursive=T)}
      
      #write raster into folder
      writeRaster(classes.sel.reclass, filename = paste0(ascii.dir.root.MC, name_comb, "/", name_comb, ".asc"), overwrite=T)
      #delete extra files which were created 'xml' and 'prj' files
      files.to.delete <- list.files(paste0(ascii.dir.root.MC, name_comb, "/"), pattern=".xml|.prj", full.names=T)
      file.remove(files.to.delete)
    }
    
  }
}


#for areasdunes----
##Single surface: categories combined into one file====
for(dunearea in c("westhoek",  "cabour", "teryde", "doornpanne")){#"westhoek",  "cabour", "houtsaegernoordduinen", "teryde", "doornpanne"
  for(resolution in c(20)){#"10m", "20m", "50m" --> all resolutions creates a lot of data
    print(paste0(dunearea, " - ", as.character(resolution)))
    #directory of the ascii-files
    ascii.dir.root.SC <- paste0(ascii.dir.SC.areasdunes, dunearea , "/", resolution, "m/") #root of current areadune and resolution files
    ascii.dir.root.MC <- paste0(ascii.dir.MC.areasdunes, dunearea , "/", resolution, "m/")
    
    classes.r <- rast(paste0("../data/raster_aggregated/classes7_", resolution, "m_", dunearea, ".tif"))
    #NAflag(classes.r) <- -9999 #change NA values of raster file, as advised for resistanceGA
    classes.r[is.na(classes.r)] <- -9999
    NAflag(classes.r) <- -9999
    
    
    #make list of not-null classes
    categories.list.exist <- c()
    for(category in categories.list){
      #if class does not exist, not taken into account to make combinations
      asc.category <- read.table(paste0(ascii.dir.root.SC, category, "/", category, ".asc"), header=F, skip=6)# %>% replace_with_na_all(condition = ~.x == -9999)
      if(any(asc.category==1, na.rm=TRUE)){
        categories.list.exist <- append(categories.list.exist, category)}
    }
    #print(categories.list.exist)
    
    #go through all possible combinations of categories
    for(n_sample in c(2:length(categories.list.exist))){
      combs <- combn(categories.list.exist, n_sample)
      #matrix of all combinations
      for(n_col in c(1:ncol(combs))){
        
        name_comb <- paste(combs[,n_col], collapse=".")
        print(name_comb)
        #make combination of raster file and write ascii-file
        
        classes.sel <- deepcopy(classes.r)
        
        #make reclassification-vector
        becomes.v <- c() #initialize vector
        for(category in categories.list){
          value_becomes <- ifelse(category %in% combs[,n_col], which(startsWith(combs[,n_col], category)), 0) #if not in focal combs, 0, else its rownumber
          becomes.v <- append(becomes.v, value_becomes)#add to vector
        }
        #print(becomes.v)
        
        reclass.df <- data.frame(is=c(1:length(becomes.v)),
                                 becomes = becomes.v)
        
        classes.sel.reclass <- classify(classes.r, reclass.df)
        #plot(classes.sel.reclass)
        dir.comb <- paste0(ascii.dir.root.MC, name_comb, "/") #"../data/ascii-files/areasdunes/dunearea/resolution/category"
        if (file.exists(dir.comb)) {cat("The folder already exists")
        } else {dir.create(dir.comb, recursive=T)}
        
        setGDALconfig("GDAL_PAM_ENABLED", "FALSE")
        
        #write raster into folder
        writeRaster(classes.sel.reclass, filename = paste0(ascii.dir.root.MC, name_comb, "/", name_comb, ".asc"), overwrite=T)
        
        #delete extra files which were created 'xml' and 'prj' files
        files.to.delete <- list.files(paste0(ascii.dir.root.MC, name_comb, "/"), pattern=".xml|.prj", full.names=T)
        # print(files.to.delete)
        file.remove(files.to.delete)
      }
      
    }
  }}

