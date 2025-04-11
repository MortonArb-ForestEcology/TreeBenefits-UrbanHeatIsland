# This script will just pull the data and run some LST models to decide which one we should move forward with
library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=T

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsRegion <- file.path(path.cities, "../city_stats_model-selection.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v4")

# Some color palettes for later
grad.temp <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
grad.elev <- c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4")
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange

grad.et <- rev(c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101")) # Ends with orange


# Lets add the ecoregion for each city; accessed 27 Oct 2022 9:30 a.m.
# SDEI shapefile: https://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013/data-download# # NOTE: REQUIRES LOGIN
# ecoregion file: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)
# plot(sdei.urb[1,])

# ecoregions <- read_sf("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
ecoregions <- read_sf("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
ecoregions$biome.name <- car::recode(ecoregions$BIOME, "'1'='tropical moist broadleaf forest'; 
                                                             '2'='tropical dry broadleaf forest'; 
                                                             '3'='tropical coniferous forest';
                                                             '4'='temperate broadleaf/mixed forest'; 
                                                             '5'='temperate coniferous forest'; 
                                                             '6'='boreal forest/taiga'; 
                                                             '7'='tropical grassland/savannas'; 
                                                             '8'='temperate grassland/savanna'; 
                                                             '9'='flooded grassland/savanna'; 
                                                            '10'='montane grassland/savanna'; 
                                                            '11'='tundra'; 
                                                            '12'='mediterranean'; 
                                                            '13'='desert/xeric shrublands'; 
                                                            '14'='mangroves'")
summary(ecoregions)

# Transform the cities and ecoregion files to what we've worked with for the MODIS data
# NOTE: This *shouldn't* be necessary anymore, but there's a weird duplicate vertex issue that causes problems; doing the spTransform seems to help
projMODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
sdei.urb <- st_transform(sdei.urb, crs(projMODIS))
summary(sdei.urb)
ecoregions <- st_transform(ecoregions, crs(projMODIS))
summary(ecoregions)

# If we don't have our summary file yet, create it and create all the column names we're going to want
if(!file.exists(file.cityStatsRegion) | overwrite){
  # cityStatsRegion <- read.csv("../sdei-global-uhi-2013.csv")
  cols.keep <-  c("ISOURBID", "ISO3", "URBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP")
  cityStatsRegion <- data.frame(sdei.urb[, cols.keep])[,1:length(cols.keep)]
  # head(cityStatsRegion)
  
  # ------------------
  # Some summary stats about the inputs at the region scale 
  # ------------------
  cityStatsRegion[,"SpatialMistmatch"] <- NA
  # - number of pixels, mean LST, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
  cityStatsRegion[,c("biome", "biome.prop", "n.pixels", "LST.mean", "LST.sd", "LST.min", "LST.max", "tree.mean", "tree.sd", "tree.min", "tree.max", "veg.mean", "veg.sd", "veg.min", "veg.max", "elev.mean", "elev.sd", "elev.min", "elev.max", "ET.mean", "ET.sd", "ET.min", "ET.max")] <- NA
  
  # Save the key info from the full model
  cityStatsRegion[,c("LST.NoVeg.model.R2adj", "LST.NoVeg.model.AIC", "LST.NoVeg.model.RMSE")] <- NA
  cityStatsRegion[,c("LSTmodel.R2adj", "LSTmodel.AIC", "LSTmodel.RMSE")] <- NA
  cityStatsRegion[,c("LSTmodelSCover.R2adj", "LSTmodelSCover.AIC", "LSTmodelSCover.RMSE")] <- NA
  cityStatsRegion[,c("LSTmodelSCoverTree.R2adj", "LSTmodelSCoverTree.AIC", "LSTmodelSCoverTree.RMSE")] <- NA
  cityStatsRegion[,c("LSTmodelS3D.R2adj", "LSTmodelS3D.AIC", "LSTmodelS3D.RMSE")] <- NA
  cityStatsRegion[,c("LSTmodelLog.R2adj", "LSTmodelLog.AIC", "LSTmodelLog.RMSE")] <- NA
  

  summary(cityStatsRegion)
  dim(cityStatsRegion)
  
  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  
  # ------------------
  
  
  
}

# Read in Summary file -----
cityStatsRegion <- read.csv(file.cityStatsRegion)
summary(cityStatsRegion); dim(cityStatsRegion)

# Get a list of the files that are done
# # Note: Some cities (2-3) seems to have >1 file, which is weird.  Can do a spot check or just roll with the last file like I think I have coded in
files.elev <- dir(path.EEout, "elevation")
files.lst <- dir(path.EEout, "LST_Day_Tmean")
files.et <- dir(path.EEout, "ETmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.mask <- dir(path.EEout, "CityMask")
files.buff <- dir(path.EEout, "Buffer-NoUrb")

length(files.elev); length(files.lst); length(files.et); length(files.tree); length(files.veg); length(files.mask); length(files.buff)
# Note: We're missing 3 cities for ET data

# Figure out which cities have all the layers needed to be analyzed
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.et <- unlist(lapply(files.et, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.mask <- unlist(lapply(files.mask, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.buff <- unlist(lapply(files.buff, FUN=function(x){strsplit(x, "_")[[1]][1]}))


# citiesDone <- unique(cities.lst)

citiesDone <- unique(cities.lst[cities.lst %in% cities.et & cities.lst %in% cities.elev & cities.lst %in% cities.tree & cities.lst %in% cities.veg & cities.lst %in% cities.mask & cities.lst %in% cities.buff])
length(citiesDone)

# Now compare the done list to what needs to be analyzed
citiesAnalyze <- citiesDone[citiesDone %in% cityStatsRegion$ISOURBID[is.na(cityStatsRegion$LSTmodel.R2adj)]]
length(citiesAnalyze)

# # Start City Loop -----
for(CITY in citiesAnalyze){
  # # Good Test Cities: Sydney (AUS66430)
  # CITY="AUS66430"; CITY="USA26687"
  row.city <- which(cityStatsRegion$ISOURBID==CITY)
  print(CITY)
  citySP <- sdei.urb[sdei.urb$ISOURBID==CITY, ]
  cityBuff <- st_buffer(citySP, dist=10e3)
  # plot(cityBuff[1]); 
  # plot(citySP[1], add=F)
  biome <- st_intersection(ecoregions[,c("BIOME", "biome.name")], cityBuff[,"ISOURBID"])
  summary(biome)
  # plot(biome)
  
  # # data.frame(biome)
  if(nrow(biome)>0){ # Some aren't quite aligning-- we'll figure those out later
    if(nrow(biome)==1){
      cityStatsRegion$biome[row.city] <- biome$biome.name
      cityStatsRegion$biome.prop[row.city] <- 1
    } else {
      biome$area <- st_area(biome)
      biome.sum <- aggregate(area ~ biome.name, data=biome, FUN=sum, na.rm=T)
      
      if(nrow(biome.sum)>1){
        rowBiome <- which(biome.sum$area==max(biome.sum$area, na.rm=T))
        cityStatsRegion$biome[row.city] <- biome.sum$biome.name[rowBiome]
        cityStatsRegion$biome.prop[row.city] <- biome.sum$area[rowBiome]/sum(biome.sum$area, na.rm=T)
        
      } else {
        cityStatsRegion$biome[row.city] <- biome.sum$biome.name
        cityStatsRegion$biome.prop[row.city] <- 1
        
      }
      
      rm(biome.sum)
    } # End if/else
  } # End skipping biomes that don't exist
  
  
  # length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fMASK <- files.mask[grep(CITY, files.mask)]
  fBUFF <- files.buff[grep(CITY, files.buff)]
  fELEV <- files.elev[grep(CITY, files.elev)]
  fLST <- files.lst[grep(CITY, files.lst)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  fVEG <- files.veg[grep(CITY, files.veg)]
  fET <- files.et[grep(CITY, files.et)]
  
  # The length statements will grab the newest file if there's more than one
  maskCity <- raster(file.path(path.EEout, fMASK[length(fMASK)]))
  buffCity <- raster(file.path(path.EEout, fBUFF[length(fBUFF)]))
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))
  

  # par(mfrow=c(1,2))
  # plot(maskCity); plot(buffCity)
  # par(mfrow=c(1,1))
  
  # par(mfrow=c(1,2))
  # plot(elevCity); plot(maskCity)
  # par(mfrow=c(1,1))
  # plot(treeCity)
  # plot(lstCity)
  # plot(etCity)
  
  # lst.mean <- mean(lstCity)
  # tree.mean <- mean(treeCity)
  # veg.mean <- mean(vegCity)
  # par(mfrow=c(2,2))
  # plot(elevCity); plot(lst.mean); plot(tree.mean); plot(veg.mean)
  
  # Elevation should be our most reliable data layer, so lets use that as our base
  coordsCity <- data.frame(coordinates(elevCity)) 
  coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
  coordsCity$elevation <- getValues(elevCity)
  coordsCity$cityBounds <- getValues(maskCity)
  coordsCity$cityBounds <- !is.na(coordsCity$cityBounds) & coordsCity$cityBounds>0 # NA = buffer = FALSE citybounds
  
  coordsBuff <- data.frame(coordinates(buffCity))
  coordsBuff$location <- paste0("x", coordsBuff$x, "y", coordsBuff$y)
  coordsBuff$bufferNoUrb <- getValues(buffCity) # NA = buffer = FALSE citybounds
  coordsBuff$bufferNoUrb <- !is.na(coordsBuff$bufferNoUrb) & coordsBuff$bufferNoUrb>0 # T = non-urban buffer
  
  if(all(coordsBuff$location == coordsCity$location)){
    coordsCity$bufferNoUrb <- coordsBuff$bufferNoUrb
    # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
    
  } else if(nrow(coordsBuff)==nrow(coordsCity)) {
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsBuff$x, y1=coordsBuff$y, x2 = coordsCity$x, y2 = coordsCity$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      coordsCity$bufferNoUrb <- coordsBuff$bufferNoUrb
    }  else {
      stop("Something's really off")
    }
    
  } else if( any(coordsVeg$location %in% coordsCity$location)) {  
    print(warning("Veg and Elev Layer Coords don't match, but at least some do"))
    # valsCity <- valsCityVeg[,]
    coordsCity <- merge(coordsCity, coordsBuff, all.x=T, all.y=F)
  } else {
    print(warning("Veg and Elev Layer doesn't match. :-( skipping for now to see how prevalent that is"))
    next
  }
  
  # ggplot(data=coordsCity, aes(x=x, y=y, fill=elevation)) +
  #   coord_equal() +
  #   geom_tile() +
  #   geom_tile(data=coordsCity[coordsCity$cityBounds,], color="orange2", alpha=0.5)+
  #   geom_tile(data=coordsCity[coordsCity$bufferNoUrb,], color="red2", alpha=0.8)
  # geom_tile(data=coordsCity[coordsCity$cityBounds,], aes(fill="core"))+
  # geom_tile(data=coordsCity[coordsCity$bufferNoUrb,], aes(fill="buffer"), alpha=0.8)
    
  # Double Checking the mask 
  coordsMask <- data.frame(coordinates(maskCity))
  coordsMask$location <- paste0("x", coordsMask$x, "y", coordsMask$y)
  
  # In case we're missing some years of LST (likely in the tropics); only pull certain layers
  layers.use <- names(treeCity)[names(treeCity) %in% names(lstCity)]
  
  coordsVeg <- data.frame(coordinates(treeCity))
  coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)
  
  
  # Land Surface Temperature 
  coordsLST <- data.frame(coordinates(lstCity))
  coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)
  
  # Adding ET --> note: This will just be a subset of years!
  layersET <- names(etCity)[names(etCity) %in% names(lstCity)]
  
  coordsET <- data.frame(coordinates(etCity))
  coordsET$location <- paste0("x", coordsET$x, "y", coordsET$y)
  
  # Checkign to see whcih coords match
  # if(!all(coordsCity$location == coordsMask$location)) { stop("Mask and elev coords don't match") } # Elev = mask --> NO!
  # if(!all(coordsVeg$location == coordsCity$location)){ stop("Veg and elev coords don't match")} # Veg = elev --> NO! # Elevation is the bad one!
  # if(!all(coordsVeg$location == coordsMask$location)){ stop("Veg and Mask coords don't match")} # Veg = mask --> YES
  # if(!all(coordsVeg$location == coordsLST$location)){ stop("Veg and LST coords don't match")} # Veg = LST --> YES
  # if(!all(coordsVeg$location == coordsET$location)){ stop("Veg and ET coords don't match")}  #  Veg = ET  --> YES
  # 
  valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
  names(valsCityVeg) <- c("cover.tree", "year")
  valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
  valsCityVeg$x <- coordsVeg$x
  valsCityVeg$y <- coordsVeg$y
  valsCityVeg$location <- coordsVeg$location
  
  # nrow(coordsCity); nrow(coordsVeg)
  
  # summary(coordsVeg$location == coordsCity$location)
  # summary(coordsVeg[coordsVeg$location==coordsCity$location,])
  # summary(coordsVeg[coordsVeg$location!=coordsCity$location,])
  if(all(coordsVeg$location == coordsCity$location)){
    cityStatsRegion$SpatialMistmatch[row.city] <- F # No problem, we're good
    
    valsCity <- valsCityVeg[,]
    valsCity$elevation <- coordsCity$elevation
    valsCity$cityBounds <- coordsCity$cityBounds
    valsCity$bufferNoUrb <- coordsCity$bufferNoUrb
    # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
    
  } else if(nrow(coordsVeg)==nrow(coordsCity)) {
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsVeg$x, y1=coordsVeg$y, x2 = coordsCity$x, y2 = coordsCity$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      
      valsCity <- valsCityVeg[,]
      valsCity$elevation <- coordsCity$elevation
      valsCity$cityBounds <- coordsCity$cityBounds
      valsCity$cityBoundsbufferNoUrb<- coordsCity$bufferNoUrb
    }  else {
      stop("Something's really off")
    }
    
  } else if( any(coordsVeg$location %in% coordsCity$location)) {  
    print(warning("Veg and Elev Layer Coords don't match, but at least some do"))
    valsCity <- valsCityVeg[,]
    
    valsCity <- merge(valsCity, coordsCity, all.x=T, all.y=F)
  } else {
    print(warning("Veg and Elev Layer doesn't match. :-( skipping for now to see how prevalent that is"))
    next
  }
  
  
  valsLST <- stack(data.frame(getValues(lstCity[[layers.use]])))
  names(valsLST) <- c("LST_Day", "year")
  valsLST$x <- coordsLST$x
  valsLST$y <- coordsLST$y
  valsLST$location <- coordsLST$location
  summary(valsLST)
  
  # locLSTAll <- unique(valsLST$location[!is.na(valsLST$LST_Day)])
  
  # nrow(coordsCity); nrow(coordsLST)
  if(all(coordsLST$location == coordsCity$location)){
    valsCity$LST_Day <- valsLST$LST_Day
    # valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
  } else if( nrow(coordsLST) == nrow (coordsCity)) {  
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsCity$x, y1=coordsCity$y, x2 = valsLST$x, y2 = coordsCity$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      valsCity$LST_Day <- valsLST$LST_Day
      
    }  else {
      stop("Something's really off")
    }
  } else if( any(coordsLST$location %in% valsCity$location)) {  
    valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
  } else {
    print(warning("LST coords do not match elev.  Need to re-implment nearest neighbor"))
  }
  
  
  valsET <- stack(data.frame(getValues(etCity[[layersET]])))
  names(valsET) <- c("ET", "year")
  valsET$x <- coordsET$x
  valsET$y <- coordsET$y
  valsET$location <- coordsET$location
  summary(valsET)
  
  # IMPORTANT: CONVERT ET from 8-day total to per day (mm/8-day to mm/day; 1 mm = 1 kg/m2)
  valsET$ET <- valsET$ET/8
  # locLSTAll <- unique(valsLST$location[!is.na(valsLST$LST_Day)])
  
  # nrow(coordsCity); nrow(coordsLST)
  if(all(coordsET$location == coordsCity$location)){
    valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
    # valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
  } else if( nrow(coordsET) == nrow (coordsCity)) {  
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsCity$x, y1=coordsCity$y, x2 = valsET$x, y2 = valsET$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsRegion$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      valsCity$ET[valsCity$year %in% layersET] <- valsET$ET
      
    }  else {
      stop("Something's really off")
    }
    
    # print(warning("Using a merge to get things together.  Slow, but it should work"))
  } else if( any(coordsET$location %in% valsCity$location)) {
    print(warning("Using a merge to get things together.  Slow, but it should work"))
    valsCity <- merge(valsCity, valsET, all.x=T, all.y=F)
  } else {
    print(warning("LST coords do not match elev.  Need to re-implment nearest neighbor"))
  }
  
  summary(valsCity)
  
  # Doing some conversion etc
  valsCity$year <- as.numeric(substr(valsCity$year, 3, 6))
  valsCity <- valsCity[!is.na(valsCity$elevation) & !is.na(valsCity$cover.tree),]
  summary(valsCity)
  
  if(length(unique(valsCity$location[!is.na(valsCity$LST_Day)]))<50){
    print(warning("LST Spatial mismatch too big; skip city"))
    print("") # Just give a clean return before moving on
    cityStatsRegion$model.R2adj[row.city] <- -9999
    next
  }
  
  
  
  # Don't bother creating a folder for a city until we'll have at least something to save!
  dir.create(file.path(path.cities, CITY), recursive = T, showWarnings = F)
  
  # # Save the full output in case we want to share it for review or other uses
  # write.csv(valsCity, file.path(path.cities, CITY, paste0(CITY, "_CityData_All.csv")), row.names=F)
  
  # Saving some summary stats of our inputs -- I know there's a more elegant way to do this, but hey, this works
  # cityStatsRegion[row.city,]
  cityStatsRegion$n.pixels[row.city] <- length(unique(valsCity$location))
  cityStatsRegion$LST.mean[row.city] <- mean(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$LST.sd[row.city] <- sd(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$LST.min[row.city] <- min(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$LST.max[row.city] <- max(valsCity$LST_Day, na.rm=T)
  cityStatsRegion$tree.mean[row.city] <- mean(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$tree.sd[row.city] <- sd(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$tree.min[row.city] <- min(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$tree.max[row.city] <- max(valsCity$cover.tree, na.rm=T)
  cityStatsRegion$veg.mean[row.city] <- mean(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$veg.sd[row.city] <- sd(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$veg.min[row.city] <- min(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$veg.max[row.city] <- max(valsCity$cover.veg, na.rm=T)
  cityStatsRegion$elev.mean[row.city] <- mean(valsCity$elevation, na.rm=T)
  cityStatsRegion$elev.sd[row.city] <- sd(valsCity$elevation, na.rm=T)
  cityStatsRegion$elev.min[row.city] <- min(valsCity$elevation, na.rm=T)
  cityStatsRegion$elev.max[row.city] <- max(valsCity$elevation, na.rm=T)
  
  # Saving ET params values, but bear in mind they're only a subset of the area!
  cityStatsRegion$ET.mean[row.city] <- mean(valsCity$ET, na.rm=T)
  cityStatsRegion$ET.sd[row.city] <- sd(valsCity$ET, na.rm=T)
  cityStatsRegion$ET.min[row.city] <- min(valsCity$ET, na.rm=T)
  cityStatsRegion$ET.max[row.city] <- max(valsCity$ET, na.rm=T)
  # cityStatsRegion[row.city,]
  
  
  # Running a null LST model to better get a sense for how much is actually explained by the veg & not just spatial stuff
  modLSTnull <- gam(LST_Day ~ elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modLSTnull <- summary(modLSTnull)
  
  valsCity$LST.NoVeg.gam.pred <- predict(modLSTnull, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LST.NoVeg.gam.resid <- valsCity$LST_Day - valsCity$LST.NoVeg.gam.pred # Hand-calculating te residuals... gives the same thing
  
  cityStatsRegion$LST.NoVeg.model.R2adj[row.city] <- sum.modLSTnull$r.sq
  cityStatsRegion$LST.NoVeg.model.AIC[row.city] <- AIC(modLSTnull)
  cityStatsRegion$LST.NoVeg.model.RMSE[row.city] <- sqrt(mean(valsCity$LST.NoVeg.gam.resid^2, na.rm=T))
  saveRDS(modLSTnull, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST-NoVegNull_gam.RDS")))
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST-NoVegNull_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(modLSTnull)
  hist(valsCity$LST.NoVeg.gam.resid)
  plot(LST.NoVeg.gam.resid ~ LST.NoVeg.gam.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LST.NoVeg.gam.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # plot(modLSTnull)
  
  # Running the LST actual model! Woot Woot
  modLSTCity <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modLSTCity <- summary(modLSTCity)
  valsCity$LSTgam.pred <- predict(modLSTCity, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LSTgam.resid <- valsCity$LST_Day - valsCity$LSTgam.pred # Hand-calculating te residuals... gives the same thing
  saveRDS(modLSTCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam.RDS")))
  # par(mfrow=c(1,1)); plot(modLSTCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(modLSTCity)
  hist(valsCity$LSTgam.resid)
  plot(LSTgam.resid ~ LSTgam.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LSTgam.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big LST model
  cityStatsRegion$LSTmodel.R2adj[row.city] <- sum.modLSTCity$r.sq
  cityStatsRegion$LSTmodel.AIC[row.city] <- AIC(modLSTCity)
  cityStatsRegion$LSTmodel.RMSE[row.city] <- sqrt(mean(valsCity$LSTgam.resid^2, na.rm=T))
  
  # -----------------------
  # Adding in two alternate non-linear models to satisfy reviewers --> just doing the summary stats for now to make life easier
  # -----------------------
  # --------------
  # Same smoothing spline formulation as we use for ET
  # --------------
  modLSTCitySCover <- gam(LST_Day ~ s(cover.tree) + s(cover.veg) + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modLSTCitySCover <- summary(modLSTCitySCover)
  valsCity$LSTgamSCover.pred <- predict(modLSTCitySCover, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LSTgamSCover.resid <- valsCity$LST_Day - valsCity$LSTgamSCover.pred # Hand-calculating te residuals... gives the same thing
  saveRDS(modLSTCitySCover, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam-SCover.RDS")))
  # par(mfrow=c(1,1)); plot(modLSTCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST_GAM-SCover_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(3,2))
  plot(modLSTCitySCover)
  hist(valsCity$LSTgamSCover.resid)
  plot(LSTgamSCover.resid ~ LSTgamSCover.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LSTgamSCover.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big LST model
  cityStatsRegion$LSTmodelSCover.R2adj[row.city] <- sum.modLSTCitySCover$r.sq
  cityStatsRegion$LSTmodelSCover.AIC[row.city] <- AIC(modLSTCitySCover)
  cityStatsRegion$LSTmodelSCover.RMSE[row.city] <- sqrt(mean(valsCity$LSTgamSCover.resid^2, na.rm=T))
  # --------------

  # --------------
  # Same smoothing spline formulation as we use for ET
  # --------------
  modLSTCitySCoverTree <- gam(LST_Day ~ s(cover.tree) + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modLSTCitySCoverTree <- summary(modLSTCitySCoverTree)
  valsCity$LSTgamSCoverTree.pred <- predict(modLSTCitySCoverTree, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LSTgamSCoverTree.resid <- valsCity$LST_Day - valsCity$LSTgamSCoverTree.pred # Hand-calculating te residuals... gives the same thing
  saveRDS(modLSTCitySCoverTree, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam-SCoverTree.RDS")))
  # par(mfrow=c(1,1)); plot(modLSTCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST_GAM-SCoverTreeOnly_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(3,2))
  plot(modLSTCitySCoverTree)
  hist(valsCity$LSTgamSCoverTree.resid)
  plot(LSTgamSCoverTree.resid ~ LSTgamSCoverTree.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LSTgamSCoverTree.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big LST model
  cityStatsRegion$LSTmodelSCoverTree.R2adj[row.city] <- sum.modLSTCitySCoverTree$r.sq
  cityStatsRegion$LSTmodelSCoverTree.AIC[row.city] <- AIC(modLSTCitySCoverTree)
  cityStatsRegion$LSTmodelSCoverTree.RMSE[row.city] <- sqrt(mean(valsCity$LSTgamSCoverTree.resid^2, na.rm=T))
  # --------------
  
  # --------------
  # Moving elevation to the smoothing spline as suggested by a reviewer
  # --------------
  modLSTCityS3D <- gam(LST_Day ~ cover.tree + s(x,y, elevation) + as.factor(year)-1, data=valsCity)
  sum.modLSTCityS3D <- summary(modLSTCityS3D)
  valsCity$LSTgamS3D.pred <- predict(modLSTCityS3D, newdata=valsCity) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LSTgamS3D.resid <- valsCity$LST_Day - valsCity$LSTgamS3D.pred # Hand-calculating te residuals... gives the same thing
  saveRDS(modLSTCityS3D, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam-S3D.RDS")))
  # par(mfrow=c(1,1)); plot(modLSTCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST_GAM-S3D_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(3,2))
  plot(modLSTCityS3D)
  hist(valsCity$LSTgamS3D.resid)
  plot(LSTgamS3D.resid ~ LSTgamS3D.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LSTgamS3D.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big LST model
  cityStatsRegion$LSTmodelS3D.R2adj[row.city] <- sum.modLSTCityS3D$r.sq
  cityStatsRegion$LSTmodelS3D.AIC[row.city] <- AIC(modLSTCityS3D)
  cityStatsRegion$LSTmodelS3D.RMSE[row.city] <- sqrt(mean(valsCity$LSTgamS3D.resid^2, na.rm=T))
  # --------------
  
  # --------------
  # Using a log function like we originally explored back in 2019; 
  # Note: This requires having no 0 values
  # --------------
  valsCity2 <- valsCity
  valsCity2$cover.tree[valsCity$cover.tree==0] <- 0.1
  valsCity2$cover.veg[valsCity$cover.veg==0] <- 0.1
  modLSTCityLog <- gam(LST_Day ~ log(cover.tree) + log(cover.veg) + elevation + s(x,y) + as.factor(year)-1, data=valsCity2)
  sum.modLSTCityLog <- summary(modLSTCityLog)
  valsCity$LSTgamLog.pred <- predict(modLSTCityLog, newdata=valsCity2) # Shifting to the newdata version to predict for where we have missing data
  valsCity$LSTgamLog.resid <- valsCity$LST_Day - valsCity$LSTgamLog.pred # Hand-calculating te residuals... gives the same thing
  saveRDS(modLSTCityLog, file=file.path(path.cities, CITY, paste0(CITY, "_Model-LST_gam-Log.RDS")))
  # par(mfrow=c(1,1)); plot(modLSTCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "LST_GAM-Log_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(modLSTCityLog)
  hist(valsCity$LSTgamLog.resid)
  plot(LSTgamLog.resid ~ LSTgamLog.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ LSTgamLog.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big LST model
  cityStatsRegion$LSTmodelLog.R2adj[row.city] <- sum.modLSTCityLog$r.sq
  cityStatsRegion$LSTmodelLog.AIC[row.city] <- AIC(modLSTCityLog)
  cityStatsRegion$LSTmodelLog.RMSE[row.city] <- sqrt(mean(valsCity$LSTgamLog.resid^2, na.rm=T))
  
  # --------------
  # -----------------------
  
  
  write.csv(valsCity, file.path(path.cities, CITY, paste0(CITY, "_values-All.csv")), row.names=F)
  
  # ------------
  

  # Calculating pixel-based summary stats to do some trend correlations
  # For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
  summaryCity <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation, ET) ~ x+y+location + cityBounds + bufferNoUrb, data=valsCity, FUN=mean)
  names(summaryCity)[names(summaryCity) %in% c("LST_Day", "cover.tree", "cover.veg", "ET")] <- c("LST.mean", "tree.mean", "veg.mean", "ET.mean")
  summary(summaryCity)
  

  # Making some plots with the summary data
  # proj.elev <- projection(elevCity)
  # sp.city2 <- terra::project(x=sp.city, proj.elev)
  # test <- sf::st_as_sf(sp.city)
  # sp.city3 <- st_transform(test, proj.elev)
  plot.lst <- ggplot(data=summaryCity[!is.na(summaryCity$LST.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=LST.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_sf(data=sp.city3, fill=NA) +
    scale_fill_gradientn(name="Summer\nTemp\n(deg. C)", colors=grad.temp) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.elev <- ggplot(data=summaryCity[!is.na(summaryCity$elevation),]) +
    coord_equal() +
    # geom_tile(aes(x=x2, y=y2, fill=temp.summer)) +
    geom_tile(aes(x=x, y=y, fill=elevation)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Elevation\n(m)", colors=grad.elev) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.tree <- ggplot(data=summaryCity[!is.na(summaryCity$tree.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=tree.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Tree\nCover\n(%)", colors=grad.tree, limits=c(0,100)) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.veg <- ggplot(data=summaryCity[!is.na(summaryCity$veg.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=veg.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Other Veg\nCover (%)", colors=grad.other, limits=c(0,100)) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  plot.et <- ggplot(data=summaryCity[!is.na(summaryCity$ET.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=ET.mean)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black", color=NA) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="ET (kg/m2)", colors=grad.et) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text=element_blank())
  
  
  png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Means.png")), height=10, width=8, units="in", res=120)
  print(
    cowplot::plot_grid(plot.lst, plot.et, plot.tree, plot.veg, plot.elev, ncol=2)
  )
  dev.off()  

  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  # Write our city stats file each time in case it bonks
  
  print("") # Just give a clean return before moving on
  
  # Remove a bunch of stuff for our own sanity
  # rm(elevCity, treeCity, vegCity, lstCity, modLSTCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.LST.Tree, plot.corr.LST.Veg, plot.corr.Tree.Veg, plot.lst.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.lst, plot.tree, plot.veg, veg.lst, veg.tree, tree.lst, veg.out, tree.out, sum.corrTreeLST, sum.corrVegLST, sum.corrVegTree, sum.modLSTCity)
  
}	

