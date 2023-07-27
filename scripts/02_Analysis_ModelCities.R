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

# The length statements will grab the newest file if there's more than one
maskCity <- raster(file.path(path.EEout, fMASK[length(fMASK)]))
elevCity <- raster(file.path(path.EEout, fELEV[length(fELEV)]))
treeCity <- brick(file.path(path.EEout, fTREE[length(fTREE)]))
vegCity <- brick(file.path(path.EEout, fVEG[length(fVEG)]))

lst8City <- brick(file.path(path.EEout, fLST8[length(fLST8)]))-273.15
lst7City <- brick(file.path(path.EEout, fLST7[length(fLST7)]))-273.15
lst5City <- brick(file.path(path.EEout, fLST5[length(fLST5)]))-273.15
lst8City; lst7City; lst5City


# par(mfrow=c(1,2))
# plot(elevCity); plot(maskCity)
# par(mfrow=c(1,1))
# plot(treeCity)

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
layers.use <- names(treeCity)[names(treeCity) %in% names(lstCity)]

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
  valsCity$cityBounds <- coordsCity$cityBounds
  # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
} else {
  stop("Veg and Elev Layer doesn't match. :-( gotta figure it out")
}

# Land Surface Temperature is mismatched with 
coordsLST <- data.frame(coordinates(lstCity))
coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)

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
} else if( any(coordsLST$location %in% valsCity$location)) {  
  valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
} else {
  print(warning("LST coords do not match elev.  Doing nearest neighbor"))
  
  valsCity$LST_Day <- NA
  valsCity$LST_Offset <- NA
  
  for(i in 1:nrow(coordsLST)){
    locLST <- coordsLST$location[i]
    xLST <- coordsLST$x[i]
    yLST <- coordsLST$y[i]
    lstNow <- which(valsLST$location==locLST)
    
    # Check to see if this is a blank spot; if so, move on
    if(all(is.na(valsLST$LST_Day[lstNow]))) next 
    
    # Find the nearest pixel from the cityCoords
    distLocX <- coordsCity$x - xLST
    distLocY <- coordsCity$y - yLST
    distLocCity <- sqrt(distLocX^2 + distLocY^2)
    # summary(distLocCity)
    
    minDist <- min(distLocCity)
    locCity <- coordsCity$location[which(distLocCity==minDist)]
    valsCity$LST_Offset[valsCity$location==locCity] <- minDist
    
    # If the closest cell is more than half a pixel away, skip it
    if(minDist > 927/2) next
    # if(minDist > 1000/2) next  # Adjusting to our nominal scale
    
    valsCity$LST_Day[valsCity$location==locCity] <- valsLST$LST_Day[lstNow]
  }
  
}

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
# cityStatsRegion[row.city,]


# Running the actual model! Woot Woot
modCity <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
sum.modCity <- summary(modCity)
valsCity$gam.pred[!is.na(valsCity$LST_Day)] <- predict(modCity)
valsCity$gam.resid[!is.na(valsCity$LST_Day)] <- resid(modCity)
save(modCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model_gam.RData")))
# par(mfrow=c(1,1)); plot(modCity)

png(file.path(path.cities, CITY, paste0(CITY, "_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
par(mfrow=c(2,2))
plot(modCity)
hist(valsCity$gam.resid)
plot(gam.resid ~ gam.pred, data=valsCity); abline(h=0, col="red")
plot(LST_Day ~ gam.pred, data=valsCity); abline(a=0, b=1, col="red")
par(mfrow=c(1,1))
dev.off()


# Save the key stats from the big model
cityStatsRegion$model.R2adj[row.city] <- sum.modCity$r.sq
cityStatsRegion[row.city,c("model.tree.slope", "model.veg.slope", "model.elev.slope")] <- sum.modCity$p.coeff[c("cover.tree", "cover.veg", "elevation")]
cityStatsRegion[row.city,c("model.tree.p", "model.veg.p", "model.elev.p")] <- sum.modCity$p.pv[c("cover.tree", "cover.veg", "elevation")]
# cityStatsRegion[row.city,]

# Calculating pixel-based summary stats to do some trend correlations
# For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
summaryCity <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation) ~ x+y+location + cityBounds, data=valsCity, FUN=mean)
names(summaryCity)[names(summaryCity) %in% c("LST_Day", "cover.tree", "cover.veg")] <- c("LST.mean", "tree.mean", "veg.mean")
summary(summaryCity)

summaryCity[,c("LST.trend", "LST.p", "LST.R2")] <- NA
summaryCity[,c("tree.trend", "tree.p", "tree.R2")] <- NA
summaryCity[,c("veg.trend", "veg.p", "veg.R2")] <- NA

pb.lms <- txtProgressBar(min=0, max=nrow(summaryCity), style=3)
for(i in 1:nrow(summaryCity)){
	rowsCity <- which(valsCity$location==summaryCity$location[i])
	setTxtProgressBar(pb.lms, i)
	
	# Skip any analysis if there's less than 10 years of data or our trend doesn't go to the last 5 years of our record
	if(length(rowsCity)<10 | max(valsCity$year[rowsCity])<=2015) next
	
	# The LST Data is going to be noisier, so we'll want to skip over anything without robust data
	lstGood <- which(valsCity$location==summaryCity$location[i] & !is.na(valsCity$LST_Day))
	if(length(lstGood)>10 & max(valsCity$year[lstGood])>2015){
  	trend.LST <- lm(LST_Day ~ year, data=valsCity[rowsCity,])
  	sum.LST <- summary(trend.LST)
  	summaryCity[i,c("LST.trend", "LST.p")] <- sum.LST$coefficients["year",c(1,4)]
  	summaryCity[i,"LST.R2"] <- sum.LST $r.squared
	}
	
	trend.tree <- lm(cover.tree ~ year, data=valsCity[rowsCity,])
	sum.tree <- summary(trend.tree)
	summaryCity[i,c("tree.trend", "tree.p")] <- sum.tree$coefficients["year",c(1,4)]
	summaryCity[i,"tree.R2"] <- sum.tree$r.squared

	trend.veg <- lm(cover.veg ~ year, data=valsCity[rowsCity,])
	sum.veg <- summary(trend.veg)
	summaryCity[i,c("veg.trend", "veg.p")] <- sum.veg$coefficients["year",c(1,4)]
	summaryCity[i,"veg.R2"] <- sum.veg$r.squared
	
}
summary(summaryCity)
write.csv(summaryCity, file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")), row.names=F)

# rm(trend.LST, trend.tree, trend.veg, sum.LST, sum.tree, sum.veg)

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
        axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
        axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))

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
        axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
        axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))

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
        axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
        axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))

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
        axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
        axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))


png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Means.png")), height=8, width=8, units="in", res=120)
print(
  cowplot::plot_grid(plot.lst, plot.elev, plot.tree, plot.veg)
)
dev.off()  

if(length(which(!is.na(summaryCity$LST.trend)))>50){
  # cityStatsRegion[row.city,]
  # Calculate the stats for the trends in LST and veg cover
  cityStatsRegion$trend.LST.slope[row.city] <- mean(summaryCity$LST.trend, na.rm=T)
  cityStatsRegion$trend.LST.slope.sd[row.city] <- sd(summaryCity$LST.trend, na.rm=T)
  LST.out <- t.test(summaryCity$LST.trend)
  cityStatsRegion$trend.LST.p[row.city] <- LST.out$p.value
  
  cityStatsRegion$trend.tree.slope[row.city] <- mean(summaryCity$tree.trend, na.rm=T)
  cityStatsRegion$trend.tree.slope.sd[row.city] <- sd(summaryCity$tree.trend, na.rm=T)
  tree.out <- t.test(summaryCity$tree.trend)
  cityStatsRegion$trend.tree.p[row.city] <- tree.out$p.value
  
  cityStatsRegion$trend.veg.slope[row.city] <- mean(summaryCity$veg.trend, na.rm=T)
  cityStatsRegion$trend.veg.slope.sd[row.city] <- sd(summaryCity$veg.trend, na.rm=T)
  veg.out <- t.test(summaryCity$veg.trend)
  cityStatsRegion$trend.veg.p[row.city] <- veg.out$p.value
  
  
  # Creating and saving some maps of those trends
  plot.lst.trend <- ggplot(data=summaryCity[!is.na(summaryCity$LST.trend),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=LST.trend)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
    # geom_sf(data=sp.city3, fill=NA) +
    scale_fill_gradientn(name="Summer\nTemp\n(deg. C/yr)", colors=grad.temp) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.tree.trend <- ggplot(data=summaryCity[!is.na(summaryCity$tree.trend),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=tree.trend)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Tree\nCover\n(%/yr)", colors=grad.tree) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.veg.trend <- ggplot(data=summaryCity[!is.na(summaryCity$veg.trend),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=veg.trend)) +
    geom_tile(data=summaryCity[!summaryCity$cityBounds,], aes(x=x, y=y), alpha=0.2, fill="black") +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Other Veg\nCover (%/yr)", colors=grad.other) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  
  png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Trends.png")), height=8, width=8, units="in", res=120)
  print(
    cowplot::plot_grid(plot.lst.trend, NULL, plot.tree.trend, plot.veg.trend)
  )
  dev.off()  
  
  
  # cityStatsRegion[row.city,]
  # Now calculating the correlations among variables
  tree.lst <- lm(LST.trend ~ tree.trend, data=summaryCity)
  sum.corrTreeLST <- summary(tree.lst)
  cityStatsRegion$corr.LST.tree.slope[row.city] <- sum.corrTreeLST$coefficients["tree.trend",1]
  cityStatsRegion$corr.LST.tree.p[row.city] <- sum.corrTreeLST$coefficients["tree.trend",4]
  cityStatsRegion$corr.LST.tree.Rsq[row.city]  <- sum.corrTreeLST$r.squared
  
  veg.lst <- lm(LST.trend ~ veg.trend, data=summaryCity)
  sum.corrVegLST <- summary(veg.lst)
  cityStatsRegion$corr.LST.veg.slope[row.city] <- sum.corrVegLST$coefficients["veg.trend",1]
  cityStatsRegion$corr.LST.veg.p[row.city] <- sum.corrVegLST$coefficients["veg.trend",4]
  cityStatsRegion$corr.LST.veg.Rsq[row.city]  <- sum.corrVegLST$r.squared

  veg.tree <- lm(tree.trend ~ veg.trend, data=summaryCity)
  sum.corrVegTree <- summary(veg.tree)
  cityStatsRegion$corr.tree.veg.slope[row.city] <- sum.corrVegTree$coefficients["veg.trend",1]
  cityStatsRegion$corr.tree.veg.p[row.city] <- sum.corrVegTree$coefficients["veg.trend",4]
  cityStatsRegion$corr.tree.veg.Rsq[row.city]  <- sum.corrVegTree$r.squared
  
  plot.corr.LST.Tree <- ggplot(data=summaryCity, aes(x=tree.trend, y=LST.trend)) +
    geom_point() +
    stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
    labs(x="Tree Trend (%/yr)", y="LST Trend (deg. C/yr)") +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_text(face="bold"),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.corr.LST.Veg <- ggplot(data=summaryCity, aes(x=veg.trend, y=LST.trend)) +
    geom_point() +
    stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
    labs(x="Other Veg Trend (%/yr)", y="LST Trend (deg. C/yr)") +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_text(face="bold"),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))

  plot.corr.Tree.Veg <- ggplot(data=summaryCity, aes(x=veg.trend, y=tree.trend)) +
    geom_point() +
    stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
    labs(x="Other Veg Trend (%/yr)", y="Tree Trend (%/yr)") +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_text(face="bold"),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))

  png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Correlations_Trends.png")), height=8, width=8, units="in", res=120)
  print(
    cowplot::plot_grid(plot.corr.LST.Tree, plot.corr.LST.Veg, NULL, plot.corr.Tree.Veg)
  )
  dev.off()  
}
write.csv(cityStatsRegion, file.cityStatsRegion, row.names=F)  # Write our city stats file each time in case it bonks

print("") # Just give a clean return before moving on

# Remove a bunch of stuff for our own sanity
# rm(elevCity, treeCity, vegCity, lstCity, modCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.LST.Tree, plot.corr.LST.Veg, plot.corr.Tree.Veg, plot.lst.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.lst, plot.tree, plot.veg, veg.lst, veg.tree, tree.lst, veg.out, tree.out, sum.corrTreeLST, sum.corrVegLST, sum.corrVegTree, sum.modCity)

	
