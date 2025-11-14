# # Script to go through the pixel-level data and calculate the core vs buffer stats
library(ggplot2); library(RColorBrewer); library(cowplot)
library(raster); library(sp); library(terra); library(sf) 


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v5")
path.cities <- file.path(path.google, "data_processed_final")
# Path to where Earth Engine is saving the spatial extractions
path.EEout <- file.path("~/Google Drive/My Drive", "UHI_Analysis_Output_Final_v5")

path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)
###########################################

###########################################
# Work with the cityAll.stats file to start re-building the core vs. buffer stats
###########################################
# Read in metro shapefile so we can update the buffer

cityAll.stats <- read.csv(file.path(path.cities, "..", "city_stats_model.csv"))
head(cityAll.stats)

# Chicago: USA26687; Vancouver: CAN16375; Berlin: DEU10109; Atlanta: USA40447; Sydney: AUS66430; Santiago (Chile): CHL66311; Cairo (AlQahirah): EGY44702; Beijing: CHN31890; Johannesburg (South Africa): ZAF64524; Rio de Janeiro: BRA63739
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Chicago" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Vancouver" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Berlin" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Atlanta" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Sydney" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Santiago" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="AlQahirah" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="BEJING" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="JOHANNESBURG" & !is.na(cityAll.stats$NAME)]
# cityAll.stats$ISOURBID[cityAll.stats$NAME=="Rio de Janeiro" & !is.na(cityAll.stats$NAME)]
# 
# unique(cityAll.stats$NAME[cityAll.stats$ISO3=="ZAF"])

CityBuff.stats <- data.frame(ISOURBID = rep(unique(cityAll.stats$ISOURBID), each=3), 
                             factor=c("LST", "tree", "other veg"), 
                             value.mean.core = NA, value.mean.buffer = NA, 
                             value.sd.core=NA, value.sd.buffer=NA, 
                             value.mean.diff=NA, value.mean.diff.sig=NA)
head(CityBuff.stats)

pb <- txtProgressBar(min=0, max=nrow(cityAll.stats), style=3)
for(i in 1:nrow(cityAll.stats)){
  setTxtProgressBar(pb, i)
  # URBID <- cityAll.stats$ISOURBID[cityAll.stats$NAME=="Chicago" & !is.na(cityAll.stats$NAME)]
  # URBID <- cityAll.stats$ISOURBID[cityAll.stats$NAME=="Sydney" & !is.na(cityAll.stats$NAME)]
  # i=which(cityAll.stats$ISOURBID==URBID)
  URBID <- cityAll.stats$ISOURBID[i]
  
  row.urbid <- which(CityBuff.stats$ISOURBID==URBID)
  row.lst <- which(CityBuff.stats$ISOURBID==URBID & CityBuff.stats$factor=="LST")
  row.tree <- which(CityBuff.stats$ISOURBID==URBID & CityBuff.stats$factor=="tree")
  row.veg <- which(CityBuff.stats$ISOURBID==URBID & CityBuff.stats$factor=="other veg")
  
  # Read in the data
  if(!file.exists(file.path(path.cities, URBID, paste0(URBID, "_CityStats_Pixels.csv")))) next
  datAll <- read.csv(file.path(path.cities, URBID, paste0(URBID, "_CityStats_Pixels.csv")))
  summary(datAll)
  

  if(nrow(datAll)<50 | length(unique(datAll$cityBounds))<2) next # For some reason we have some cities with only 2 pixels?
  # Calculate some summary stats
  # datMean <- aggregate(cbind(LST.mean, tree.mean, veg.mean, elevation, LST.trend, tree.trend, veg.trend) ~ cityBounds, data=datAll, FUN=mean)
  # datSD <- aggregate(cbind(LST.mean, tree.mean, veg.mean, elevation, LST.trend, tree.trend, veg.trend) ~ cityBounds, data=datAll, FUN=sd)
  
  # Summarizing LST
  CityBuff.stats$value.mean.core[row.lst] <- mean(datAll$LST_Day[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.buffer[row.lst] <- mean(datAll$LST_Day[datAll$bufferNoUrb], na.rm=T)
  CityBuff.stats$value.sd.core[row.lst] <- sd(datAll$LST_Day[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.buffer[row.lst] <- sd(datAll$LST_Day[datAll$bufferNoUrb], na.rm=T)
  CityBuff.stats$value.mean.diff[row.lst] <- CityBuff.stats$value.mean.core[row.lst] - CityBuff.stats$value.mean.buffer[row.lst]

  
  nCore <- length(which(datAll$cityBounds))
  nBuff <- length(which(datAll$bufferNoUrb))
  diffLST <- vector(length=1000)
  set.seed(1554)
  for(i in 1:length(diffLST)){
    datCore <- sample(datAll$LST_Day[datAll$cityBounds], min(1000, nCore, nBuff))
    datBuff <- sample(datAll$LST_Day[datAll$bufferNoUrb], min(1000, nCore, nBuff))
    
    diffLST[i] <- mean(datCore) - mean(datBuff)
  }
  # head(mean_diff)
  # mean(mean_diff)
  qDiffLST <- quantile(diffLST, c(0.025, 0.975))
  CityBuff.stats$value.mean.diff.sig[row.lst] <- ifelse(all(qDiffLST>0) | all(qDiffLST<0), T, F)

  
  # Summarizing trees
  CityBuff.stats$value.mean.core[row.tree] <- mean(datAll$cover.tree[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.buffer[row.tree] <- mean(datAll$cover.tree[datAll$bufferNoUrb], na.rm=T)
  CityBuff.stats$value.sd.core[row.tree] <- sd(datAll$cover.tree[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.buffer[row.tree] <- sd(datAll$cover.tree[datAll$bufferNoUrb], na.rm=T)
  CityBuff.stats$value.mean.diff[row.tree] <- CityBuff.stats$value.mean.core[row.tree] - CityBuff.stats$value.mean.buffer[row.tree]
  
  diffTree <- vector(length=1000)
  set.seed(1554)
  for(i in 1:length(diffTree)){
    datCore <- sample(datAll$cover.tree[datAll$cityBounds], min(1000, nCore, nBuff))
    datBuff <- sample(datAll$cover.tree[datAll$bufferNoUrb], min(1000, nCore, nBuff))
    
    diffTree[i] <- mean(datCore) - mean(datBuff)
  }
  # head(diffTree)
  # mean(diffTree)
  qDiffTree <- quantile(diffTree, c(0.025, 0.975))
  CityBuff.stats$value.mean.diff.sig[row.tree] <- ifelse(all(qDiffTree>0) | all(qDiffTree<0), T, F)
  

  # Summarizing vegs
  CityBuff.stats$value.mean.core[row.veg] <- mean(datAll$cover.veg[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.buffer[row.veg] <- mean(datAll$cover.veg[datAll$bufferNoUrb], na.rm=T)
  CityBuff.stats$value.sd.core[row.veg] <- sd(datAll$cover.veg[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.buffer[row.veg] <- sd(datAll$cover.veg[datAll$bufferNoUrb], na.rm=T)
  CityBuff.stats$value.mean.diff[row.veg] <- CityBuff.stats$value.mean.core[row.veg] - CityBuff.stats$value.mean.buffer[row.veg]
  
  diffVeg <- vector(length=1000)
  set.seed(1554)
  for(i in 1:length(diffVeg)){
    datCore <- sample(datAll$cover.veg[datAll$cityBounds], min(1000, nCore, nBuff))
    datBuff <- sample(datAll$cover.veg[datAll$bufferNoUrb], min(1000, nCore, nBuff))
    
    diffVeg[i] <- mean(datCore) - mean(datBuff)
  }
  # head(diffVeg)
  # mean(diffVeg)
  qDiffVeg <- quantile(diffVeg, c(0.025, 0.975))
  CityBuff.stats$value.mean.diff.sig[row.veg] <- ifelse(all(qDiffVeg>0) | all(qDiffVeg<0), T, F)
  

}
summary(CityBuff.stats)
write.csv(CityBuff.stats, file.path(path.google, "city_stats_core-buffer.csv"), row.names=F)


summary(CityBuff.stats[CityBuff.stats$factor=="LST" & CityBuff.stats$value.mean.diff>0 & CityBuff.stats$value.mean.diff.sig,])


summary(CityBuff.stats[CityBuff.stats$factor=="tree" & CityBuff.stats$value.mean.diff<0 & CityBuff.stats$value.mean.diff.sig,])
###########################################
