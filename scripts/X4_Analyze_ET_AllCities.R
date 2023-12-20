# After having done our preliminary ET model exploration, lets go through the full data for all cities.  I'll probably save the top 1-2

library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=F

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3/ET_models")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsET <- file.path(path.cities, "../city_stats_all_ET.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v3")

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



# If we don't have our summary file yet, create it and create all the column names we're going to want
if(!file.exists(file.cityStatsET) | overwrite){
  # cityStatsET <- read.csv("../sdei-global-uhi-2013.csv")
  cols.keep <-  c("ISOURBID", "ISO3", "URBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP")
  cityStatsET <- data.frame(sdei.urb[, cols.keep])[,1:length(cols.keep)]
  # head(cityStatsET)
  
  # ------------------
  # Some summary stats about the inputs at the region scale 
  # ------------------
  cityStatsET[,"SpatialMistmatch"] <- NA
  # - number of pixels, mean LST, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
  cityStatsET[,c("n.pixels", "n.pixels.ET", "LST.ET.mean", "LST.ET.sd", "LST.ET.min", "LST.ET.max", "tree.ET.mean", "tree.ET.sd", "tree.ET.min", "tree.ET.max", "veg.ET.mean", "veg.ET.sd", "veg.ET.min", "veg.ET.max", "ETobs.mean", "ETobs.sd", "ETobs.min", "ETobs.max")] <- NA
  
  # Save the key info from the full model
  cityStatsET[,c("ETmodel.R2adj", "ETmodel.RMSE")] <- NA
  cityStatsET[,c("ETpred.mean", "ETpred.sd", "ETpred.min", "ETpred.max")] <- NA
  

  summary(cityStatsET)
  dim(cityStatsET)
  
  write.csv(cityStatsET, file.cityStatsET, row.names=F)  
  # ------------------
}

# Read in Summary file -----
cityStatsET <- read.csv(file.cityStatsET)
summary(cityStatsET); dim(cityStatsET)

# Get a list of the files that are done
# # Note: Some cities (2-3) seems to have >1 file, which is weird.  Can do a spot check or just roll with the last file like I think I have coded in
files.elev <- dir(path.EEout, "elevation")
files.lst <- dir(path.EEout, "LST_Day_Tmean")
files.et <- dir(path.EEout, "ETmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
files.mask <- dir(path.EEout, "CityMask")
length(files.lst); length(files.et); length(files.tree); length(files.veg); length(files.elev); length(files.mask)
# Note: We're missing 3 cities for ET data

# Figure out which cities have all the layers needed to be analyzed
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.et <- unlist(lapply(files.et, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.mask <- unlist(lapply(files.mask, FUN=function(x){strsplit(x, "_")[[1]][1]}))


# citiesDone <- unique(cities.lst)

citiesDone <- unique(cities.lst[cities.lst %in% cities.et & cities.lst %in% cities.elev & cities.lst %in% cities.tree & cities.lst %in% cities.veg & cities.lst %in% cities.mask])
length(citiesDone)

# Now compare the done list to what needs to be analyzed
citiesAnalyze <- citiesDone[citiesDone %in% cityStatsET$ISOURBID[is.na(cityStatsET$ETmodel.R2adj)]]
length(citiesAnalyze)

# # Start City Loop -----
for(CITY in citiesAnalyze){
  # # Good Test Cities: Sydney (AUS66430)
  # CITY="AUS66430"; CITY="USA26687"
  row.city <- which(cityStatsET$ISOURBID==CITY)
  print(CITY)
  # citySP <- sdei.urb[sdei.urb$ISOURBID==CITY, ]
  # cityBuff <- st_buffer(citySP, dist=10e3)
  
  # length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fMASK <- files.mask[grep(CITY, files.mask)]
  fELEV <- files.elev[grep(CITY, files.elev)]
  fLST <- files.lst[grep(CITY, files.lst)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  fVEG <- files.veg[grep(CITY, files.veg)]
  fET <- files.et[grep(CITY, files.et)]
  
  # The length statements will grab the newest file if there's more than one
  maskCity <- raster(file.path(path.EEout, fMASK[length(fMASK)]))
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))/8
  
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
  coordsCity$cityBounds <- !is.na(coordsCity$cityBounds) # NA = buffer = FALSE citybounds
  
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
  
  valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
  names(valsCityVeg) <- c("cover.tree", "year")
  valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
  valsCityVeg$x <- coordsVeg$x
  valsCityVeg$y <- coordsVeg$y
  valsCityVeg$location <- coordsVeg$location
  
  if(all(coordsVeg$location == coordsCity$location)){
    cityStatsET$SpatialMistmatch[row.city] <- F # No problem, we're good
    
    valsCity <- valsCityVeg[,]
    valsCity$elevation <- coordsCity$elevation
    valsCity$cityBounds <- coordsCity$cityBounds
    # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
    
  } else if(nrow(coordsVeg)==nrow(coordsCity)) {
    # Checking to make sure the offset is minimal
    datComb <- data.frame(x1=coordsVeg$x, y1=coordsVeg$y, x2 = coordsCity$x, y2 = coordsCity$y)
    datComb$x.diff <- datComb$x1 - datComb$x2
    datComb$y.diff <- datComb$y1 - datComb$y2
    # summary(datComb) # For this example, it's a stupid tiny offset
    
    if(max(abs(datComb$x.diff), abs(datComb$y.diff))<1){
      # print(warning("Veg and Elev Layer Coords don't match, but right number pixels. Proceeding as if fine"))
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
      
      valsCity <- valsCityVeg[,]
      valsCity$elevation <- coordsCity$elevation
      valsCity$cityBounds <- coordsCity$cityBounds
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
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
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
      # cityStatsET$SpatialMistmatch[row.city] <- T # Something's off, but hopefully okay
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
  
  if(length(unique(valsCity$location[!is.na(valsCity$ET)]))<50){
    print(warning("Not enough ET pixels to robustly model; skip city"))
    print("") # Just give a clean return before moving on
    cityStatsET$ETmodel.R2adj[row.city] <- -9999
    next
  }
  
  if(max(valsCity$ET, na.rm=T)<0.1){
    print(warning("ET too low to model; skip city"))
    print("") # Just give a clean return before moving on
    cityStatsET$ETmodel.R2adj[row.city] <- -9999
    next
  }
  
  
  # Don't bother creating a folder for a city until we'll have at least something to save!
  dir.create(file.path(path.cities, CITY), recursive = T, showWarnings = F)
  
  # # Save the full output in case we want to share it for review or other uses
  # write.csv(valsCity, file.path(path.cities, CITY, paste0(CITY, "_CityData_All.csv")), row.names=F)
  
  # Saving some summary stats of our inputs -- I know there's a more elegant way to do this, but hey, this works
  # cityStatsET[row.city,]
  # NOTE: In contrast to the big analy script, these are the ranges used to model ET
  cityStatsET$n.pixels[row.city] <- length(unique(valsCity$location))
  cityStatsET$n.pixels.ET[row.city] <- length(unique(valsCity$location[!is.na(valsCity$ET)]))
  cityStatsET$LST.ET.mean[row.city] <- mean(valsCity$LST_Day[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$LST.ET.sd[row.city] <- sd(valsCity$LST_Day[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$LST.ET.min[row.city] <- min(valsCity$LST_Day[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$LST.ET.max[row.city] <- max(valsCity$LST_Day[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.mean[row.city] <- mean(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.sd[row.city] <- sd(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.min[row.city] <- min(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$tree.ET.max[row.city] <- max(valsCity$cover.tree[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.mean[row.city] <- mean(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.sd[row.city] <- sd(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.min[row.city] <- min(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)
  cityStatsET$veg.ET.max[row.city] <- max(valsCity$cover.veg[!is.na(valsCity$ET)], na.rm=T)

  # Saving ET params values, but bear in mind they're only a subset of the area!
  cityStatsET$ETobs.mean[row.city] <- mean(valsCity$ET, na.rm=T)
  cityStatsET$ETobs.sd[row.city] <- sd(valsCity$ET, na.rm=T)
  cityStatsET$ETobs.min[row.city] <- min(valsCity$ET, na.rm=T)
  cityStatsET$ETobs.max[row.city] <- max(valsCity$ET, na.rm=T)
  # cityStatsET[row.city,]
  
  
  # having the year factor means we can't predict ET without knowing a regional mean --> 
  modETCity <- gam(sqrt(ET) ~ s(cover.tree) + s(cover.veg) + s(LST_Day) + s(x,y), data=valsCity)
  sum.modETCity <- summary(modETCity)

  valsCity$ET.pred <- predict(modETCity, newdata=valsCity)^2 # Shifting to the newdata version to predict for where we have missing data
  valsCity$ET.resid <- valsCity$ET - valsCity$ET.pred # Hand-calculating te residuals... gives the same thing, but hopefully a bit faster
  
  # Saving predicted ET params values, this shoudl generally be lower than our "observed"
  # cityStatsET[,c("ETpred.mean", "ETpred.sd", "ETpred.min", "ETpred.max")] <- NA
  cityStatsET$ETpred.mean[row.city] <- mean(valsCity$ET.pred, na.rm=T)
  cityStatsET$ETpred.sd[row.city] <- sd(valsCity$ET.pred, na.rm=T)
  cityStatsET$ETpred.min[row.city] <- min(valsCity$ET.pred, na.rm=T)
  cityStatsET$ETpred.max[row.city] <- max(valsCity$ET.pred, na.rm=T)
  
  
  # Save the key stats from the big LST model
  cityStatsET$ETmodel.R2adj[row.city] <- sum.modETCity$r.sq
  cityStatsET$ETmodel.RMSE[row.city] <- sqrt(mean(valsCity$ET.resid^2, na.rm=T))
  
  
  save(modETCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model-ET_gam.RData")))
  # par(mfrow=c(1,1)); plot(modETCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "ET_GAM_qaqc.png")), height=6, width=9, units="in", res=120)
  par(mfrow=c(2,4))
  plot(modETCity)
  hist(valsCity$ET.resid)
  plot(ET.resid ~ ET.pred, data=valsCity); abline(h=0, col="red")
  plot(ET ~ ET.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  # plot(ET.resid ~ LST_Day, data=valsCity); abline(h=0, col="red")
  # plot(ET.resid ~ cover.tree, data=valsCity); abline(h=0, col="red")
  # plot(ET.resid ~ cover.veg, data=valsCity); abline(h=0, col="red")
  # 
  # plot(ET.pred ~ LST_Day, data=valsCity)
  # plot(ET.pred ~ cover.tree, data=valsCity)
  # plot(ET.pred ~ cover.veg, data=valsCity)
  # plot(ET ~ LST_Day, data=valsCity)
  # plot(ET ~ cover.tree, data=valsCity)
  # plot(ET ~ cover.veg, data=valsCity)
  
  # cityStatsET[row.city,]
  
  
  write.csv(cityStatsET, file.cityStatsET, row.names=F)  # Write our city stats file each time in case it bonks
  
  print("") # Just give a clean return before moving on
  
  # Remove a bunch of stuff for our own sanity
  # rm(elevCity, treeCity, vegCity, lstCity, modETCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.LST.Tree, plot.corr.LST.Veg, plot.corr.Tree.Veg, plot.lst.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.lst, plot.tree, plot.veg, veg.lst, veg.tree, tree.lst, veg.out, tree.out, sum.corrTreeLST, sum.corrVegLST, sum.corrVegTree, sum.modETCity)
  
}	

