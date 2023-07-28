library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

overwrite=F

# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
path.google <- "~/Google Drive/My Drive/"
path.cities <- file.path(path.google, "UHI_Analysis_v3Test_CHICAGO_analysis")

if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityStatsRegion <- file.path(path.cities, "city_stats_all.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path(path.google, "UHI_Analysis_v3Test_CHICAGO")

# Some color palettes for later
grad.temp <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
grad.elev <- c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4")
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange



# Lets add the ecoregion for each city; accessed 27 Oct 2022 9:30 a.m.
# SDEI shapefile: https://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013/data-download# # NOTE: REQUIRES LOGIN
# ecoregion file: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
sdei.urb <- read_sf("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$NAME=="Chicago" & !is.na(sdei.urb$NAME),]
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
  # - number of pixels, mean LST, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
  cityStatsRegion[,c("biome", "biome.prop", "n.pixels", "LST.mean", "LST.sd", "LST.min", "LST.max", "tree.mean", "tree.sd", "tree.min", "tree.max", "veg.mean", "veg.sd", "veg.min", "veg.max", "elev.mean", "elev.sd", "elev.min", "elev.max")] <- NA
  
  # Save the key info from the full model
  cityStatsRegion[,c("model.R2adj", "model.tree.slope", "model.veg.slope", "model.elev.slope", "model.tree.p", "model.veg.p", "model.elev.p")] <- NA
  
  # For each variable calculate the overall trends --> this will need to be separate from the pixel-by-pixel analysis, although we'll do and save that as well
  cityStatsRegion[,c("trend.LST.slope", "trend.LST.slope.sd", "trend.LST.p")] <- NA
  cityStatsRegion[,c("trend.tree.slope", "trend.tree.slope.sd", "trend.tree.p")] <- NA
  cityStatsRegion[,c("trend.veg.slope", "trend.veg.slope.sd", "trend.veg.p")] <- NA
  
  # Also look at the correlation between warming and change in tree & veg cover
  cityStatsRegion[,c("corr.LST.tree.slope", "corr.LST.tree.p", "corr.LST.tree.Rsq")] <- NA
  cityStatsRegion[,c("corr.LST.veg.slope", "corr.LST.veg.p", "corr.LST.veg.Rsq")] <- NA
  cityStatsRegion[,c("corr.tree.veg.slope", "corr.tree.veg.p", "corr.tree.veg.Rsq")] <- NA
  
  summary(cityStatsRegion)
  dim(cityStatsRegion)
  
  write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  
  # ------------------
 
  

}

cityStatsRegion <- read.csv(file.cityStatsRegion)
summary(cityStatsRegion); dim(cityStatsRegion)


# length(files.elev); length(files.lst); length(files.tree); length(files.veg); length(files.mask)
# Circuitous coding, but it will be more resilient to multiple versions
dir(path.EEout)

fMASK <- dir(path.EEout, "CityMask")
fELEV <- dir(path.EEout, "elevation")
fTREE <- dir(path.EEout, "PercentTree")
fVEG <- dir(path.EEout, "PercentOtherVeg")
fLST8 <- dir(path.EEout, "Landsat8_LST")
fLST7 <- dir(path.EEout, "Landsat7_LST")
fLST5 <- dir(path.EEout, "Landsat5_LST")
fLSTall <- dir(path.EEout, "Landsat-AllCombined_LST")

# The length statements will grab the newest file if there's more than one
maskCity <- brick(file.path(path.EEout, fMASK[length(fMASK)]))
elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))

lst8City <- brick(file.path(path.EEout, fLST8[length(fLST8)]))-273.15
lst7City <- brick(file.path(path.EEout, fLST7[length(fLST7)]))-273.15
lst5City <- brick(file.path(path.EEout, fLST5[length(fLST5)]))-273.15
lstAllCity <- brick(file.path(path.EEout, fLSTall[length(fLSTall)]))-273.15
lst8City; lst7City; lst5City; lstAllCity


# par(mfrow=c(1,2))
# plot(elevCity); plot(maskCity)
# par(mfrow=c(1,1))
# plot(treeCity)

plot(lstAllCity[["YR2020"]])
plot(lst8City[["YR2020"]])
plot(lst7City[["YR2020"]])

plot(lstAllCity[["YR2014"]])
plot(lst8City[["YR2014"]])
plot(lst7City[["YR2014"]])

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

# In case we're missing some years of LST (likely in the tropics); only pull certain layers
tempNames <- unique(c(names(lst8City), names(lst7City), names(lst5City)))
layers.use <- names(treeCity)[names(treeCity) %in% tempNames]

coordsVeg <- data.frame(coordinates(treeCity))
coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)

valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
names(valsCityVeg) <- c("cover.tree", "year")
valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
valsCityVeg$x <- coordsVeg$x
valsCityVeg$y <- coordsVeg$y
valsCityVeg$location <- coordsVeg$location

# nrow(coordsCity); nrow(coordsVeg)
if(all(coordsVeg$location == coordsCity$location)){
  valsCity <- valsCityVeg[,]
  valsCity$elevation <- coordsCity$elevation
  # valsCity$cityBounds <- coordsCity$cityBounds
  # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
} else {
  stop("Veg and Elev Layer doesn't match. :-( gotta figure it out")
}


valsCity <- valsCity[!is.na(valsCity$elevation),]
summary(valsCity)
dim(valsCity)


# -------
# Landsat 8 LST ----
# -------
coordsLST8 <- data.frame(coordinates(lst8City))
coordsLST8$location <- paste0("x", coordsLST8$x, "y", coordsLST8$y)
dim(coordsLST8)

# Landsat 8
valsLST8 <- stack(data.frame(getValues(lst8City[[layers.use]])))
names(valsLST8) <- c("LST_Day", "year")
valsLST8$x <- coordsLST8$x
valsLST8$y <- coordsLST8$y
valsLST8$location <- coordsLST8$location
valsLST8$satellite <- "landsat8"
valsLST8 <- valsLST8[!is.na(valsLST8$LST_Day),]
summary(valsLST8)

dim(valsLST8); dim(valsCity)

# Landsat 7
coordsLST7 <- data.frame(coordinates(lst7City))
coordsLST7$location <- paste0("x", coordsLST7$x, "y", coordsLST7$y)

valsLST7 <- stack(data.frame(getValues(lst7City[[layers.use]])))
names(valsLST7) <- c("LST_Day", "year")
valsLST7$x <- coordsLST7$x
valsLST7$y <- coordsLST7$y
valsLST7$location <- coordsLST7$location
valsLST7$satellite <- "landsat7"
valsLST7 <- valsLST7[!is.na(valsLST7$LST_Day),]
summary(valsLST7)


# Landsat 5
coordsLST5 <- data.frame(coordinates(lst5City))
coordsLST5$location <- paste0("x", coordsLST5$x, "y", coordsLST5$y)

valsLST5 <- stack(data.frame(getValues(lst5City[[layers.use]])))
names(valsLST5) <- c("LST_Day", "year")
valsLST5$x <- coordsLST7$x
valsLST5$y <- coordsLST7$y
valsLST5$location <- coordsLST7$location
valsLST5$satellite <- "landsat5"
valsLST5 <- valsLST5[!is.na(valsLST5$LST_Day),]

summary(valsLST5)
dim(valsLST8); dim(valsLST7); dim(valsLST5);




# nrow(coordsCity); nrow(coordsLST)
# Try merging just the temperature data together for the moment
temp875 <- rbind(valsLST8, valsLST7, valsLST5)
temp875 <- temp875[!is.na(temp875$year),]
summary(temp875)


# Iteratively Remove 6-sigma outliers for LST
lst875.mean <- mean(temp875$LST_Day, na.rm=T)
lst875.sd <- sd(temp875$LST_Day, na.rm=T)

sussLST <- which(temp875$LST_Day>(lst875.mean+6*lst875.sd) | temp875$LST_Day<(lst875.mean-6*lst875.sd))

while(length(sussLST)>0){
  temp875$LST_Day[sussLST] <- NA 
  
  # Update our outlier threshold
  lst875.mean <- mean(temp875$LST_Day, na.rm=T)
  lst875.sd <- sd(temp875$LST_Day, na.rm=T)
  sussLST <- which(temp875$LST_Day>(lst875.mean+6*lst875.sd) | temp875$LST_Day<(lst875.mean-6*lst875.sd))
}
summary(temp875)


# Landsat All combined
coordsLSTall <- data.frame(coordinates(lstAllCity))
coordsLSTall$location <- paste0("x", coordsLSTall$x, "y", coordsLSTall$y)

valsLSTall <- stack(data.frame(getValues(lstAllCity[[layers.use]])))
names(valsLSTall) <- c("LST_Day", "year")
valsLSTall$x <- coordsLSTall$x
valsLSTall$y <- coordsLSTall$y
valsLSTall$location <- coordsLSTall$location
valsLSTall$satellite <- "landsat-combined"
valsLSTall <- valsLSTall[!is.na(valsLSTall$LST_Day),]

summary(valsLSTall)
dim(valsLSTall)
dim(valsLST8); dim(valsLST7); dim(valsLST5);


# Iteratively Remove 6-sigma outliers for LST
lstAll.mean <- mean(valsLSTall$LST_Day, na.rm=T)
lstAll.sd <- sd(valsLSTall$LST_Day, na.rm=T)

sussLST <- which(valsLSTall$LST_Day>(lstAll.mean+6*lstAll.sd) | valsLSTall$LST_Day<(lstAll.mean-6*lstAll.sd))

while(length(sussLST)>0){
  valsLSTall$LST_Day[sussLST] <- NA 
  
  # Update our outlier threshold
  lstAll.mean <- mean(valsLSTall$LST_Day, na.rm=T)
  lstAll.sd <- sd(valsLSTall$LST_Day, na.rm=T)
  sussLST <- which(valsLSTall$LST_Day>(lstAll.mean+6*lstAll.sd) | valsLSTall$LST_Day<(lstAll.mean-6*lstAll.sd))
}
summary(valsLSTall)


tempAll <- rbind(temp875, valsLSTall)
# if( any(coordsLST8$location %in% valsCity$location)) {  
#   valsCity <- merge(valsCity, valsLST8, all.x=T, all.y=T)
# } else {
#   print(warning("LST coords do not match elev. Reprocess your data."))
# }

for(YR in unique(tempAll$year)){
  print(YR)
  
  png(file.path(path.cities, paste0("Chicago_LST_Comparison_", YR, ".png")), height=8, width=10, units="in", res=220)
  print(
    ggplot(data=tempAll[tempAll$year==YR,]) +
      coord_equal() +
      ggtitle(YR) +
      facet_wrap(~satellite, ncol=2) +
      geom_tile(aes(x=x, y=y, fill=LST_Day)) +
      scale_fill_gradientn(name="Summer\nTemp\n(deg. C)", colors=grad.temp, limits=range(tempAll$LST_Day, na.rm=T)) +
      theme_bw()
  )
  dev.off()
  
}

# -------

# -------
# Merging datasets together
# -------
valsCityAllL <- merge(valsCity, valsLSTall, all.x=T, all.y=F)
summary(valsCityAllL)

write.csv(valsCityAllL, file.path(path.cities, paste0("Chicago_data_raw_LandsatCombined.csv")), row.names=F)

valsCityL875 <- merge(valsCity, temp875, all.x=T, all.y=F)
summary(valsCityL875)

write.csv(valsCityL875, file.path(path.cities, paste0("Chicago_data_raw_Landsat875.csv")), row.names=F)


# -------


# -------
# Running the Gams to test performance
# -------
modCityComb <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=valsCityAllL)
save(modCityComb, file=file.path(path.cities, paste0("Chicago_Model_gam_LandsatCombined.RData")))

sum.modCityComb <- summary(modCityComb)
sum.modCityComb

valsCityAllL$gam.pred[!is.na(valsCityAllL$LST_Day)] <- predict(modCityComb)
valsCityAllL$gam.resid[!is.na(valsCityAllL$LST_Day)] <- resid(modCityComb)
summary(valsCityAllL)

write.csv(valsCityAllL, file.path(path.cities, paste0("Chicago_data_raw_LandsatCombined.csv")), row.names=F)


modCityL875 <- gamm(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, random=list(satellite=~1), data=valsCityL875)
save(modCityL875, file=file.path(path.cities, paste0("Chicago_Model_gam_Landsat875.RData")))

sum.modCityL875 <- summary(modCityL875$gam)
sum.modCityL875


valsCityL875$gam.pred[!is.na(valsCityL875$LST_Day)] <- predict(modCityComb)
valsCityL875$gam.resid[!is.na(valsCityL875$LST_Day)] <- resid(modCityComb)
summary(valsCityL875)
write.csv(valsCityL875, file.path(path.cities, paste0("Chicago_data_raw_Landsat875.csv")), row.names=F)

# -------


