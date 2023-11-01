# Setting up a script to check projections as they come off the line
library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)

path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3/data_processed_final")

origMismatch <- c("CAN18062", "CHN30337", "CHN20860", "CHN31996", "JPN36657", "JPN36805", "JPN7372", "NOR2617", "PRK90005", "PRK90006", "RUS5257" , "RUS5447" , "RUS5759", "RUS6025", "RUS8724")
# "CAN18062" -- 94%
# "CHN30337" -- 97%
# "CHN20860" -- 96%
# "CHN31996" -- 100%
# "JPN36657" - 100% match
# "JPN36805" -- 98% 
# "JPN7372" -- 98% match
# NOR2617 -- 99% match
# "PRK90005" -- 100% match
# "PRK90006" -- 100% match
# "RUS5257" -- 100%
# "RUS5447" -- 91% match
# "RUS5759" - 98% match
# "RUS6025" -- 100% match
# "RUS8724" -- 100% match
# # # BROKEN: SWE3477 --> oddly truncated



if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsRegion <- file.path(path.cities, "../city_stats_all.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v3")
path.EEout2 <- file.path(path.google, "My Drive", "UHI_Analysis_Output_Final_v2")


sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)


# Get a list of the files that are done
# # Note: Some cities (2-3) seems to have >1 file, which is weird.  Can do a spot check or just roll with the last file like I think I have coded in
files.lst <- dir(path.EEout, "LST_Day_Tmean")
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.lst)


# LST vs. Mask ----
files.mask <- dir(path.EEout, "CityMask")
cities.mask <- unlist(lapply(files.mask, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.mask)


citiesLSTMask <- unique(cities.lst[cities.lst %in% cities.mask])
length(citiesLSTMask)

# 
for(CITY in citiesLSTMask){
  print(paste0(CITY, " - ", sdei.urb$NAME[sdei.urb$ISOURBID==CITY]))

  # Circuitous coding, but it will be more resilient to multiple versions
  fLST <- files.lst[grep(CITY, files.lst)]
  fMASK <- files.mask[grep(CITY, files.mask)]
 
  # The length statements will grab the newest file if there's more than one
  maskCity <- raster(file.path(path.EEout, fMASK[length(fMASK)])) 
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15

  # Double Checking the mask 
  coordsMask <- data.frame(coordinates(maskCity))
  coordsMask$location <- paste0("x", coordsMask$x, "y", coordsMask$y)
  
  # Land Surface Temperature 
  coordsLST <- data.frame(coordinates(lstCity))
  coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)
  
  if(!all(coordsMask$location == coordsLST$location)){ 
    if(any(coordsMask$location %in% coordsLST$location))({
      nMask <- nrow(coordsMask)
      nLST <- nrow(coordsLST)
      lMatch <- length(which(coordsMask$location %in% coordsLST$location))/nMask
      print(paste("only some locations match:", nMask, "mask points, ", nLST, "LST points;", round(lMatch, 2)*100, "% match"))
    }) else {
      stop("Mask and LST coords don't match")
    }
    
  } 
}


# CAN18062 -- 94%
# CHN30337 -- 97%
# CHN20860 -- 96%
# CHN31996 -- 100%
# JPN36657 - 100% match
# JPN36805 -- 98% 
# JPN7372 -- 98% match
# NOR2617 -- 99% match
# PRK90005 -- 100% match
# PRK90006 -- 100% match
# RUS5257 -- 100%
# RUS5447 -- 91% match
# RUS5759 - 98% match
# RUS6025 -- 100% match
# RUS8724 -- 100% match
# # # BROKEN: SWE3477 --> oddly truncated
#
#
#

# LST vs ET ---- # Shoudl be fine because same script!
files.et <- dir(path.EEout, "ETmean")
cities.et <- unlist(lapply(files.et, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.et)

citiesLSTET <- unique(cities.lst[cities.lst %in% cities.et])
length(citiesLSTET)
# # Now compare the done list to what needs to be analyzed
# citiesAnalyze <- citiesDone[citiesDone %in% cityStatsRegion$ISOURBID[is.na(cityStatsRegion$LSTmodel.R2adj)]]
# length(citiesAnalyze)

# # Start City Loop -----
for(CITY in citiesLSTET){
  print(paste0(CITY, " - ", sdei.urb$NAME[sdei.urb$ISOURBID==CITY]))
  # length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fLST <- files.lst[grep(CITY, files.lst)]
  fET <- files.et[grep(CITY, files.et)]
  # 
  # The length statements will grab the newest file if there's more than one
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15
  etCity <- brick(file.path(path.EEout, fET[length(fET)]))
  
  # Double Checking the mask 
  coordsET <- data.frame(coordinates(etCity))
  coordsET$location <- paste0("x", coordsET$x, "y", coordsET$y)
  
  # Land Surface Temperature 
  coordsLST <- data.frame(coordinates(lstCity))
  coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)
  

  if(!all(coordsET$location == coordsLST$location)){ 
    if(any(coordsET$location %in% coordsLST$location))({
      nET <- nrow(coordsET)
      nLST <- nrow(coordsLST)
      lMatch <- length(which(coordsET$location %in% coordsLST$location))/nET
      print(paste("only some locations match:", nET, "ET points, ", nLST, "LST points;", round(lMatch, 2)*100, "% match"))
    }) else {
      stop("ET and LST coords don't match")
    }
    
  } 
}


# LST vs elevation ----
files.lst <- dir(path.EEout, "ET")
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.lst)

files.elev <- dir(path.EEout, "elevation-TEST")
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.elev); 

# files.elev <- dir(path.EEout, "elevation-TEST")
files.elevOLD <- dir(path.EEout2, "elevation")
cities.elevOLD <- unlist(lapply(files.elevOLD, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.elevOLD); 

citiesLSTelev <- unique(cities.lst[cities.lst %in% cities.elev])
length(citiesLSTelev)

# # Start City Loop -----
for(CITY in citiesLSTelev[2:length(citiesLSTelev)]){
  print(paste0(CITY, " - ", sdei.urb$NAME[sdei.urb$ISOURBID==CITY]))
  # length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fLST <- files.lst[grep(CITY, files.lst)]
  fELEV <- files.elev[grep(CITY, files.elev)]
  fELEVold <- files.elevOLD[grep(CITY, files.elevOLD)]
  
  # The length statements will grab the newest file if there's more than one
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15
  elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
  elevCityOLD <- raster(file.path(path.EEout2, fELEVold[length(fELEVold)]))
  lstCity
  elevCity
  elevCityOLD
  
  plot(lstCity[[1]])
  plot(elevCityOLD[[1]])
  plot(elevCity[[1]])

  
  # Land Surface Temperature 
  coordsLST <- data.frame(coordinates(lstCity))
  coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)
  
  # Double Checking the Elev 
  coordsElev <- data.frame(coordinates(elevCity))
  coordsElev$location <- paste0("x", coordsElev$x, "y", coordsElev$y)

  coordsElevOLD <- data.frame(coordinates(elevCityOLD))
  coordsElevOLD$location <- paste0("x", coordsElevOLD$x, "y", coordsElevOLD$y)
  
  dim(coordsLST)
  dim(coordsElev)
  dim(coordsElevOLD)
  # Checkign to see whcih coords match
  # if(!all(coordsCity$location == coordsMask$location)) { stop("Mask and elev coords don't match") } # Elev = mask --> NO!
  # if(!all(coordsVeg$location == coordsCity$location)){ stop("Veg and elev coords don't match")} # Veg = elev --> NO! # Elevation is the bad one!
  summary(coordsElev$location == coordsLST$location)
  summary(coordsElev$location == coordsElevOLD$location)
  summary(coordsLST$location == coordsElevOLD$location)
  
  if(!all(coordsElev$location == coordsLST$location)){ 
    if(any(coordsElev$location %in% coordsLST$location))({
      nElev <- nrow(coordsElev)
      nLST <- nrow(coordsLST)
      lMatch <- length(which(coordsElev$location %in% coordsLST$location))/nElev
      stop(paste("only some locations match:", nElev, "Elev points, ", nLST, "LST points;", round(lMatch, 2)*100, "% match"))
    }) else {
      stop("Elev and LST coords don't match")
    }
    
  } 
}





files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
length(files.lst); length(files.et); length(files.tree); length(files.veg); length(files.mask)

# Figure out which cities have all the layers needed to be analyzed
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))


# LST vs Tree ----
files.lst <- dir(path.EEout, "LST_Day_Tmean-TEST")
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.lst)

files.tree <- dir(path.EEout, "PercentTree-TEST")
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
length(files.tree); 

citiesLSTtree <- unique(cities.lst[cities.lst %in% cities.tree])
length(citiesLSTtree)

# 
for(CITY in citiesLSTtree){
  print(paste0(CITY, " - ", sdei.urb$NAME[sdei.urb$ISOURBID==CITY]))
  # length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
  # Circuitous coding, but it will be more resilient to multiple versions
  fLST <- files.lst[grep(CITY, files.lst)]
  fTREE <- files.tree[grep(CITY, files.tree)]
  
  # The length statements will grab the newest file if there's more than one
  lstCity <- brick(file.path(path.EEout, fLST[length(fLST)]))-273.15
  treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
  
  plot(lstCity[[1]])
  plot(treeCity[[1]])
  
  
  # Land Surface Temperature 
  coordsLST <- data.frame(coordinates(lstCity))
  coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)
  
  # Double Checking the Elev 
  coordsTree <- data.frame(coordinates(treeCity))
  coordsTree$location <- paste0("x", coordsTree$x, "y", coordsTree$y)
  
  # Checkign to see whcih coords match
  # if(!all(coordsCity$location == coordsMask$location)) { stop("Mask and elev coords don't match") } # Elev = mask --> NO!
  # if(!all(coordsVeg$location == coordsCity$location)){ stop("Veg and elev coords don't match")} # Veg = elev --> NO! # Elevation is the bad one!
  
  if(!all(coordsTree$location == coordsLST$location)){ 
    if(any(coordsTree$location %in% coordsLST$location))({
      nTree <- nrow(coordsTree)
      nLST <- nrow(coordsLST)
      lMatch <- length(which(coordsTree$location %in% coordsLST$location))/nElev
      print(paste("only some locations match:", nTree, "Tree points, ", nLST, "LST points;", round(lMatch, 2)*100, "% match"))
    }) else {
      stop("Tree and LST coords don't match")
    }
    
  } 
}

