##Script to assemble raster files at different number of classes, resolutions###
###conversion to ascii-files for loading into resistanceGA is done in 'raster ascii conversion.R'###
##By: Femke Batsleer

library(raster)
library(sf)
library(terra)
rasterOptions(maxmemory=5e+10)

#needed to run this script: BBK files, dune classification files, clippers for the study (sub)areas

#Complete study area westcoast: clip and combine BBK & dunes at 1m, reclassify, aggregate at scales 10, 20, 50 -----



##load rasters to make one file for each type and clip to extent of westcoast====
#clipper
wc_clipper <- read_sf(dsn="../data/vector_raw", layer="westcoast_clipper_adjustedborders_metpolders")
#plot(st_geometry(wc_clipper))
##BBK
BBK.l <- list.files("../data/raster_raw/BBK/", "tif$", full.names=TRUE) #read raster filenames # the $ exludes aux-files
BBK.r <- sprc(lapply(BBK.l, rast)) #SpatRasterCollection (terra)
BBK <- mosaic(BBK.r) #complete merged raster
BBK.mask <- terra::mask(BBK, wc_clipper) #mask: masks values to NA
BBK.crop <- crop(BBK.mask, extent(wc_clipper)) #crop to extent as well
BBK.na <- classify(BBK.crop, cbind(-Inf, 0, NA)) #change 0 values to NA

##Dunes
dunes.l <- list.files("../data/raster_raw/Dune classifications/", "tif$", full.names=TRUE)
dunes.r <- sprc(lapply(dunes.l, rast))
dunes <- mosaic(dunes.r)
dunes.mask <- terra::mask(dunes, wc_clipper)
dunes.crop <- crop(dunes.mask, extent(wc_clipper))
dunes.na <- classify(dunes.crop, cbind(-Inf, 0, NA))

plot(BBK.na)
plot(dunes.na)
# writeRaster(BBK.na, filename="../data/raw raster/resampled/BBK_merged_R.tif", overwrite=T)
# writeRaster(dunes.na, filename="../data/raw raster/resampled/Dunes_merged_R.tif", overwrite=T)


##resize and recentre/align to each other====

#check what resolutions are
res(BBK.na)
res(dunes.na)
#align to each other
dunes.align <- resample(dunes.na, BBK.na, method='near')
plot(dunes.align)
# writeRaster(dunes.align, filename="../data/raw raster/resampled/Dunes_aligned_R.tif", overwrite=T)


##add beachsand====

#load beachsand
beachsand <- read_sf(dsn="../data/vector_raw", layer="BeachSandPolygon")
#plot(st_geometry(beachsand))
#rasterize according to dune/BBK
BBK.ex <- deepcopy(BBK.na) #deepcopy of BBK raster, to change extent, so shallow copy is not changed
ext(BBK.ex) <- ext(beachsand)#adjust extent to that of raster layer
beachsand.raster <- rasterize(vect(beachsand), BBK.ex, values=5)#4=beachsand, see below reclassifications, now 5 for 8 classes
beachsand.raster.align <- resample(beachsand.raster, BBK.na, method='near')#align to other raster files
beachsand.mask <- terra::mask(beachsand.raster.align, wc_clipper)
beachsand.crop <- crop(beachsand.mask, extent(wc_clipper))
beachsand.na <- classify(beachsand.crop, cbind(-Inf, 0, NA))
beachsand.reclass.r <- classify(beachsand.na, data.frame(from=c(1), to=c(5)))
plot(beachsand.reclass.r)
plot(st_geometry(beachsand), add=T)
# writeRaster(beachsand.reclass.r, filename="../data/raw raster/reclassified/beachsand.tif", overwrite=T)


##reclassify====
#do this before merging/aggregating, so values match when putting them together
#1=urbanized, 2=water, 3=agricultural, 4=beach, 5=scrub, 6=fixed (trees), 7=open dune
#make dataframe for each reclassification
dunes.reclass.df <- data.frame(from=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            to = c(5, 5, 5, 6, 7, 7, 7, 7, 5, 7))
BBK.reclass.df <- data.frame(from=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                          to = c(1, 1, 1, 1, 2, 1, 3, 5, 6, 3, 1, 1, 2, 2))#
#perform the reclassifications
dunes.reclass.r <- classify(dunes.align, dunes.reclass.df)
BBK.reclass.r <- classify(BBK.na, BBK.reclass.df)
# writeRaster(dunes.reclass.r, filename="../data/raster_raw/reclassified/Dunes_reclassified_v8classes.tif", overwrite=T)
# writeRaster(BBK.reclass.r, filename="../data/raster_raw/reclassified/BBK_reclassified_v8classes.tif", overwrite=T)

##merge====
stack.dunes.BBK <- merge(dunes.reclass.r, BBK.reclass.r)
stack.dunes.BBK.beachsand <- merge(beachsand.reclass.r, stack.dunes.BBK)
writeRaster(stack.dunes.BBK.beachsand, filename="../data/raster_aggregated/classes7_1m.tif", overwrite=T)

#priority rule: depending on values to combine, a certain value is combined; GIVES WEIRD RESULTS e.g. urban in mid of dunes; NA should probably be added
# priority_rule <- function(x){
#   if(1 %in% x){return(1)}#if urban present in values -> urban
#   #else if(6 %in% x){return(6)} #fixed present in values, if not urban -> fixed
#   else{return(modal(x))}
# }

##rescale  50m, 20m====
for(resolution in c(20, 50)){
  print(paste0("aggregating the raster with resolution ", as.character(resolution),"..."))
  stack.resolution <- aggregate(stack.dunes.BBK.beachsand, fact=resolution, fun="modal")
  print("saving the file...")
  writeRaster(stack.resolution, filename=paste0("../data/raster_aggregated/classes7_", as.character(resolution), "m.tif"), overwrite=T)
}

###########################################################################################################################################


#Subareas: clip and write files-----

#can be run without running previous part of script, but aggregated rasterfiles (with n_classes and resolution) should have been made and ready in the raster.path
clip_dunearea <- function(clipper, name_clipper, raster.path="../data/raster_aggregated/", n_classes=7, resolution=10){
  #load raster file
  classes.raster <- mosaic(sprc(rast(paste0(raster.path, "classes", as.character(n_classes), "_", as.character(resolution),"m.tif"))))
  classes.mask <- terra::mask(classes.raster, clipper)#mas.k: masks values to NA
  #print("cropping raster...")
  classes.crop <- crop(classes.mask, ext(clipper))#crop to extent as well
  classes.na <- classify(classes.mask, cbind(-Inf, 0, NA))#change 0 values to NA
  
  writeRaster(classes.na, filename=paste0('../data/raster_aggregated/classes', as.character(n_classes), 
                                          "_", as.character(resolution), "m", "_", name_clipper, ".tif"), overwrite=T)
  return(classes.na)
}

#make and write raster-files for each clipper.
for(clipper.name in c("westhoek", "doornpanne", "teryde", "cabour")){
  print(clipper.name)
  for(resolution in c(20, 50)){#c(10, 20, 50)
    print(resolution)
    clipper_area <- read_sf(dsn='../data/vector_raw/clippers_areasdunes', layer=paste0(clipper.name, "_adjustedborders"))
    plot(clipper_area)
    clipper.raster <- clip_dunearea(clipper = clipper_area, name_clipper=clipper.name, raster.path="../data/raster_aggregated/",
                                  n_classes=7, resolution=resolution)
  }
}
