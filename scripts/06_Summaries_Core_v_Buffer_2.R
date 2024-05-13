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

grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with red
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green


biome.pall.all = c("Taiga"= "#2c5c74", 
                   "Tundra"="#6d8e9d",
                   "Temperate Broadleaf Forest" = "#7f310f",
                   "Temperate Conifer Forest" = "#4d1e10",
                   "Temperate Grassland/Savanna" = "#b09c41",
                   "Montane Grassland/Savanna" = "#a0b8c7",
                   "Mediterranean" = "#bf772e",
                   "Desert" = "#c89948",
                   "Flooded Grassland/Savanna" = "#e0dfa1",
                   "Tropical Grassland/Savanna" = "#a6b39e",
                   "Tropical Dry Broadleaf Forest" = "#7a9c64",
                   "Tropical Conifer Forest" = "#488458",
                   "Tropical Moist Broadleaf Forest"= "#266240",
                   "Mangroves" = "#9c8c94")

world <- map_data("world")
# ##########################################


# ##########################################
# Read in base datasets ----
# ##########################################
# Regional Sumary Stuff ----
cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])

cityAll.stats$biome <- gsub("flodded", "flooded", cityAll.stats$biome) # Whoops, had a typo!  Not going to reprocess now.
summary(as.factor(cityAll.stats$biome))

cityAll.stats$biomeName <- car::recode(cityAll.stats$biome, 
                                       "'boreal forest/taiga'='Taiga';
                                       'tundra'='Tundra';
                                       'montane grassland/savanna'='Montane Grassland/Savanna';
                                       'temperate broadleaf/mixed forest'='Temperate Broadleaf Forest';
                                       'temperate coniferous forest'='Temperate Conifer Forest';
                                       'temperate grassland/savanna'='Temperate Grassland/Savanna';
                                       'mediterranean'='Mediterranean';
                                       'desert/xeric shrublands'='Desert';
                                       'flooded grassland/savanna'='Flooded Grassland/Savanna';
                                       'tropical grassland/savannas'='Tropical Grassland/Savanna';
                                       'tropical dry broadleaf forest'='Tropical Dry Broadleaf Forest';
                                       'tropical coniferous forest'='Tropical Conifer Forest';
                                       'tropical moist broadleaf forest'='Tropical Moist Broadleaf Forest';
                                       'mangroves'='Mangroves'")

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)
# ##########################################


# ##########################################
# Exploring Core vs. Buffer data -----
# ##########################################
CityBuffStats <- read.csv(file.path(path.google, "city_stats_core-buffer.csv"))
summary(CityBuffStats)

# need
# library(tidyr)
# spread(dat1, key = numbers, value = value)
valMean <- reshape(CityBuffStats[,c("ISOURBID", "factor", "value.mean.core")], idvar="ISOURBID", timevar="factor", direction="wide")
valMean2 <- reshape(CityBuffStats[,c("ISOURBID", "factor", "value.mean.buffer")], idvar="ISOURBID", timevar="factor", direction="wide")
head(valMean2)
valMean[,names(valMean2)[2:4]] <- valMean2[,2:4]

# Merge in metadata
valMean <- merge(valMean, cityAll.stats[,c("ISOURBID", "NAME",  "LATITUDE", "LONGITUDE", "ES00POP", "biomeName", "LSTmodel.tree.slope", "LSTmodel.veg.slope")], all=T)
summary(valMean)

# Looking at the difference in tree cover between the metro core & buffer
valMean$LST.diff <- valMean$value.mean.core.LST-valMean$value.mean.buffer.LST

valMean$Tree.diff <- valMean$value.mean.core.tree-valMean$value.mean.buffer.tree
valMean$Tree.diff.degC <- valMean$Tree.diff*valMean$LSTmodel.tree.slope

valMean$Veg.diff <- valMean$`value.mean.core.other veg`-valMean$`value.mean.buffer.other veg`
valMean$Veg.diff.degC <- valMean$Veg.diff*valMean$LSTmodel.veg.slope

summary(valMean)
mean(valMean$Tree.diff, na.rm=T); sd(valMean$Tree.diff, na.rm=T)
mean(valMean$Tree.diff.degC, na.rm=T); sd(valMean$Tree.diff.degC, na.rm=T)

# Proportion of the LST diff attributable to Tree Cover ----
valMean$percentTreeCool <- valMean$Tree.diff.degC/valMean$LST.diff

mean(valMean$percentTreeCool, na.rm=T); sd(valMean$percentTreeCool, na.rm=T)

#### Exploring statements from our outline ----
# https://docs.google.com/document/d/1jzRpgNR8pWvmhS0bCULx4iAG5QjX5b0GcW8YPAs01VI/edit?usp=sharing

# Of the cities with showing UHI effects, XX% have decreased tree canopy cover, with a mean differences  XX% between the core metropolitan area and surrounding landscape.
hist(valMean$Tree.diff[valMean$LST.diff>0])
mean(valMean$Tree.diff[valMean$LST.diff>0], na.rm=T); sd(valMean$Tree.diff[valMean$LST.diff>0], na.rm=T) 
quantile(valMean$Tree.diff[valMean$LST.diff>0], c(0.05, 0.5, 0.95), na.rm=T)

hist(valMean$Tree.diff.degC[valMean$LST.diff>0])
mean(valMean$Tree.diff.degC[valMean$LST.diff>0], na.rm=T); sd(valMean$Tree.diff.degC[valMean$LST.diff>0], na.rm=T) 
t.test(valMean$Tree.diff[valMean$LST.diff>0])
t.test(valMean$Tree.diff.degC[valMean$LST.diff>0])

summary(valMean$Tree.diff.degC[valMean$LST.diff>0]/valMean$LST.diff[valMean$LST.diff>0])
mean(valMean$Tree.diff.degC[valMean$LST.diff>0]/valMean$LST.diff[valMean$LST.diff>0], na.rm=T); sd(valMean$Tree.diff.degC[valMean$LST.diff>0]/valMean$LST.diff[valMean$LST.diff>0], na.rm=T)

mean(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0]/valMean$LST.diff[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T)

length(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0])
mean(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T); sd(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T)
# t.test(valMean$value.mean.core.tree[valMean$LST.diff>0], valMean$value.mean.buffer.tree[valMean$LST.diff>0], paired=T)

hist(valMean$Veg.diff[valMean$LST.diff>0])
mean(valMean$Veg.diff[valMean$LST.diff>0], na.rm=T); sd(valMean$Veg.diff[valMean$LST.diff>0], na.rm=T) 
mean(valMean$Veg.diff.degC[valMean$LST.diff>0], na.rm=T); sd(valMean$Veg.diff.degC[valMean$LST.diff>0], na.rm=T) 
t.test(valMean$Veg.diff[valMean$LST.diff>0])


nrow(valMean[valMean$LST.diff>0 & valMean$Veg.diff<0,])/nrow(valMean[valMean$LST.diff>0,])
nrow(valMean[valMean$LST.diff>0 & valMean$Veg.diff>0,])/nrow(valMean[valMean$LST.diff>0,])

mean(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T); sd(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T)

# Conversely -- looking at the cities with negative LST diff
hist(valMean$Tree.diff[valMean$LST.diff<0])
mean(valMean$Tree.diff[valMean$LST.diff<0], na.rm=T); sd(valMean$Tree.diff[valMean$LST.diff<0], na.rm=T) 
quantile(valMean$Tree.diff[valMean$LST.diff<0], c(0.025, 0.5, 0.975), na.rm=T)
t.test(valMean$Tree.diff[valMean$LST.diff<0])

nrow(valMean[valMean$LST.diff<0 & valMean$Tree.diff>0,])/nrow(valMean[valMean$LST.diff<0,])

mean(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T); sd(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0], na.rm=T)




### # Looking at temporal trends in the urban core ----
valTrendCore <- reshape(CityBuffStats[,c("ISOURBID", "factor", "trend.mean.core")], idvar="ISOURBID", timevar="factor", direction="wide")
valTrendCore <- merge(valTrendCore, cityAll.stats[,c("ISOURBID", "NAME",  "LATITUDE", "LONGITUDE", "ES00POP", "biomeName", "LSTmodel.tree.slope", "LSTmodel.veg.slope")], all=T)

# Converting the tree trend to deg. C per year ----
valTrendCore$LST.change.Tree <- valTrendCore$trend.mean.core.tree*valTrendCore$LSTmodel.tree.slope
valTrendCore$LST.change.NoTree <- valTrendCore$trend.mean.core.LST - valTrendCore$LST.change.Tree
summary(valTrendCore)

summary(valTrendCore[ ,c("trend.mean.core.LST", "LST.change.Tree", "LST.change.NoTree")]*20)
summary(valTrendCore$LST.change.NoTree/valTrendCore$LST.change.Tree)

# hist(valTrendCore$LST.change.Tree/valTrendCore$LST.change.NoTree, xlim=-)


