# # Script to go through the pixel-level data and calculate the core vs buffer stats
library(ggplot2); library(RColorBrewer); library(cowplot)


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4")
path.cities <- file.path(path.google, "data_processed_final")

path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)
###########################################

###########################################
# Work with the cityAll.stats file to start re-building the core vs. buffer stats
###########################################
cityAll.stats <- read.csv(file.path(path.cities, "..", "city_stats_all.csv"))
head(cityAll.stats)

# Chicago: USA26687; Vancouver: CAN16375; Berlin: DEU10109; Atlanta: USA40447; Sydney: AUS66430; Santiago (Chile): CHL66311; Cairo (AlQahirah): EGY44702; Beijing: CHN31890; Johannesburg (South Africa): ZAF64524; Rio de Janeiro: BRA63739
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Chicago" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Vancouver" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Berlin" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Atlanta" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Sydney" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Santiago" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="AlQahirah" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="BEJING" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="JOHANNESBURG" & !is.na(cityAll.stats$NAME)]
cityAll.stats$ISOURBID[cityAll.stats$NAME=="Rio de Janeiro" & !is.na(cityAll.stats$NAME)]

# unique(cityAll.stats$NAME[cityAll.stats$ISO3=="ZAF"])

CityBuff.stats <- data.frame(ISOURBID = rep(unique(cityAll.stats$ISOURBID), each=3), 
                             factor=c("LST", "tree", "other veg"), 
                             value.mean.core = NA, value.mean.buffer = NA, 
                             value.sd.core=NA, value.sd.buffer=NA, 
                             value.mean.diff=NA, value.mean.diff.p=NA, 
                             trend.mean.core=NA, trend.mean.buffer=NA, 
                             trend.sd.core=NA, trend.sd.buffer=NA, 
                             trend.p.core=NA, trend.p.buffer=NA, 
                             trend.mean.diff=NA, trend.mean.diff.p=NA)
head(CityBuff.stats)

pb <- txtProgressBar(min=0, max=nrow(cityAll.stats), style=3)
for(i in 1:nrow(cityAll.stats)){
  setTxtProgressBar(pb, i)
  # URBID <- cityAll.stats$ISOURBID[cityAll.stats$NAME=="Chicago" & !is.na(cityAll.stats$NAME)]
  # URBID <- cityAll.stats$ISOURBID[cityAll.stats$NAME=="Sydney" & !is.na(cityAll.stats$NAME)]
  URBID <- cityAll.stats$ISOURBID[i]
  
  row.urbid <- which(CityBuff.stats$ISOURBID==URBID)
  row.lst <- which(CityBuff.stats$ISOURBID==URBID & CityBuff.stats$factor=="LST")
  row.tree <- which(CityBuff.stats$ISOURBID==URBID & CityBuff.stats$factor=="tree")
  row.veg <- which(CityBuff.stats$ISOURBID==URBID & CityBuff.stats$factor=="other veg")
  
  # Read in the data
  if(!file.exists(file.path(path.cities, URBID, paste0(URBID, "_CityStats_Pixels.csv")))) next
  datAll <- read.csv(file.path(path.cities, URBID, paste0(URBID, "_CityStats_Pixels.csv")))
  summary(datAll)
  
  # Calculate some summary stats
  # datMean <- aggregate(cbind(LST.mean, tree.mean, veg.mean, elevation, LST.trend, tree.trend, veg.trend) ~ cityBounds, data=datAll, FUN=mean)
  # datSD <- aggregate(cbind(LST.mean, tree.mean, veg.mean, elevation, LST.trend, tree.trend, veg.trend) ~ cityBounds, data=datAll, FUN=sd)
  
  # Summarizing LST
  CityBuff.stats$value.mean.core[row.lst] <- mean(datAll$LST.mean[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.buffer[row.lst] <- mean(datAll$LST.mean[!datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.core[row.lst] <- sd(datAll$LST.mean[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.buffer[row.lst] <- sd(datAll$LST.mean[!datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.diff[row.lst] <- CityBuff.stats$value.mean.core[row.lst] - CityBuff.stats$value.mean.buffer[row.lst]
  
  LSTMean.test <- t.test(LST.mean ~ cityBounds, data=datAll)
  CityBuff.stats$value.mean.diff.p[row.lst] <- LSTMean.test$p.value
  
  # Summarizing trees
  CityBuff.stats$value.mean.core[row.tree] <- mean(datAll$tree.mean[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.buffer[row.tree] <- mean(datAll$tree.mean[!datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.core[row.tree] <- sd(datAll$tree.mean[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.buffer[row.tree] <- sd(datAll$tree.mean[!datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.diff[row.tree] <- CityBuff.stats$value.mean.core[row.tree] - CityBuff.stats$value.mean.buffer[row.tree]
  
  treeMean.test <- t.test(tree.mean ~ cityBounds, data=datAll)
  CityBuff.stats$value.mean.diff.p[row.tree] <- treeMean.test$p.value
  

  # Summarizing vegs
  CityBuff.stats$value.mean.core[row.veg] <- mean(datAll$veg.mean[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.buffer[row.veg] <- mean(datAll$veg.mean[!datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.core[row.veg] <- sd(datAll$veg.mean[datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.sd.buffer[row.veg] <- sd(datAll$veg.mean[!datAll$cityBounds], na.rm=T)
  CityBuff.stats$value.mean.diff[row.veg] <- CityBuff.stats$value.mean.core[row.veg] - CityBuff.stats$value.mean.buffer[row.veg]
  
  vegMean.test <- t.test(veg.mean ~ cityBounds, data=datAll)
  CityBuff.stats$value.mean.diff.p[row.veg] <- vegMean.test$p.value
  
  
  # Analyze the trends in each zone (city vs buffer)
  if(!all(is.na(datAll$LST.trend))){
    # LST Trend
    CityBuff.stats$trend.mean.core[row.lst] <- mean(datAll$LST.trend[datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.mean.buffer[row.lst] <- mean(datAll$LST.trend[!datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.sd.core[row.lst] <- sd(datAll$LST.trend[datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.sd.buffer[row.lst] <- sd(datAll$LST.trend[!datAll$cityBounds], na.rm=T)
    
    LSTTrend.lm <- lm(LST.trend~cityBounds-1, data=datAll)
    sum.LSTTrend.lm <- summary(LSTTrend.lm)
    CityBuff.stats$trend.p.core[row.lst] <- sum.LSTTrend.lm$coefficients["cityBoundsTRUE","Pr(>|t|)"]
    CityBuff.stats$trend.p.buffer[row.lst] <- sum.LSTTrend.lm$coefficients["cityBoundsFALSE","Pr(>|t|)"]
    
    CityBuff.stats$trend.mean.diff[row.lst] <- CityBuff.stats$trend.mean.core[row.lst] - CityBuff.stats$trend.mean.buffer[row.lst]
    
    LSTTrend.test <- t.test(LST.trend ~ cityBounds, data=datAll)
    CityBuff.stats$trend.mean.diff.p[row.lst] <- LSTTrend.test$p.value
    
    # tree Trend
    CityBuff.stats$trend.mean.core[row.tree] <- mean(datAll$tree.trend[datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.mean.buffer[row.tree] <- mean(datAll$tree.trend[!datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.sd.core[row.tree] <- sd(datAll$tree.trend[datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.sd.buffer[row.tree] <- sd(datAll$tree.trend[!datAll$cityBounds], na.rm=T)
    
    treeTrend.lm <- lm(tree.trend~cityBounds-1, data=datAll)
    sum.treeTrend.lm <- summary(treeTrend.lm)
    
    CityBuff.stats$trend.p.core[row.tree] <- sum.treeTrend.lm$coefficients["cityBoundsTRUE","Pr(>|t|)"]
    CityBuff.stats$trend.p.buffer[row.tree] <- sum.treeTrend.lm$coefficients["cityBoundsFALSE","Pr(>|t|)"]
    
    CityBuff.stats$trend.mean.diff[row.tree] <- CityBuff.stats$trend.mean.core[row.tree] - CityBuff.stats$trend.mean.buffer[row.tree]
    
    treeTrend.test <- t.test(tree.trend ~ cityBounds, data=datAll)
    CityBuff.stats$trend.mean.diff.p[row.tree] <- treeTrend.test$p.value
    
    # veg Trend
    CityBuff.stats$trend.mean.core[row.veg] <- mean(datAll$veg.trend[datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.mean.buffer[row.veg] <- mean(datAll$veg.trend[!datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.sd.core[row.veg] <- sd(datAll$veg.trend[datAll$cityBounds], na.rm=T)
    CityBuff.stats$trend.sd.buffer[row.veg] <- sd(datAll$veg.trend[!datAll$cityBounds], na.rm=T)
    
    vegTrend.lm <- lm(veg.trend~cityBounds-1, data=datAll)
    sum.vegTrend.lm <- summary(vegTrend.lm)
    
    CityBuff.stats$trend.p.core[row.veg] <- sum.vegTrend.lm$coefficients["cityBoundsTRUE","Pr(>|t|)"]
    CityBuff.stats$trend.p.buffer[row.veg] <- sum.vegTrend.lm$coefficients["cityBoundsFALSE","Pr(>|t|)"]
    
    CityBuff.stats$trend.mean.diff[row.veg] <- CityBuff.stats$trend.mean.core[row.veg] - CityBuff.stats$trend.mean.buffer[row.veg]
    
    vegTrend.test <- t.test(veg.trend ~ cityBounds, data=datAll)
    CityBuff.stats$trend.mean.diff.p[row.veg] <- vegTrend.test$p.value
  }
  
  # CityBuff.stats[row.urbid,]

}

write.csv(CityBuff.stats, file.path(path.google, "city_stats_core-buffer.csv"), row.names=F)

###########################################
