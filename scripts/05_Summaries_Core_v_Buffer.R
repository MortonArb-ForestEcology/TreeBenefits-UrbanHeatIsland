# # Script to go through the pixel-level data and calculate the core vs buffer stats
library(ggplot2); library(RColorBrewer); library(cowplot)

###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
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
cityAll.stats <- read.csv(file.path(path.google, "city_stats_model.csv"))
summary(cityAll.stats[!is.na(cityAll.stats$model.R2adj),])

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

cityAll.stats$biomeClim[grepl("tropical", cityAll.stats$biome) | grepl("flooded", cityAll.stats$biome) | grepl("mangroves", cityAll.stats$biome)] <- "Tropical/Subtropical"
cityAll.stats$biomeClim[grepl("temperate", cityAll.stats$biome)] <- "Temperate"
cityAll.stats$biomeClim[grepl("xeric", cityAll.stats$biome) | grepl("mediterranean", cityAll.stats$biome)] <- "Dry"
cityAll.stats$biomeClim[grepl("taiga", cityAll.stats$biome) | grepl("tundra", cityAll.stats$biome) | grepl("montane", cityAll.stats$biome)] <- "Polar/Montane"
summary(as.factor(cityAll.stats$biomeClim))

cityAll.stats$biomeVeg[grepl("forest", cityAll.stats$biome) | grepl("mangrove", cityAll.stats$biome)] <- "Forest"
cityAll.stats$biomeVeg[grepl("grassland", cityAll.stats$biome)] <- "Grassland/Savanna"
cityAll.stats$biomeVeg[grepl("shrub", cityAll.stats$biome) | grepl("tundra", cityAll.stats$biome) | grepl("mediterranean", cityAll.stats$biome)] <- "Shrubland"
summary(as.factor(cityAll.stats$biomeVeg))
# unique(cityAll.stats$ISO3)

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)
# ##########################################



# ##########################################
# Exploring Core vs. Buffer data -----
# ##########################################
CityBuffStats <- read.csv(file.path(path.google, "city_stats_core-buffer.csv"))
CityBuffStats <- merge(CityBuffStats, cityAll.stats[,c("ISOURBID", "NAME",  "LATITUDE", "LONGITUDE", "ES00POP", "biomeName")], all=T)
summary(CityBuffStats)

head(CityBuffStats[is.na(CityBuffStats$value.mean.core),])
length(unique(CityBuffStats$ISOURBID[is.na(CityBuffStats$value.mean.core)]))

# Get rid of missing cities that got introduced somehow
CityBuffStats <- CityBuffStats[!is.na(CityBuffStats$value.mean.core),]

nrow(CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig & !is.na(CityBuffStats$value.mean.diff),])/ nrow(CityBuffStats[CityBuffStats$factor=="LST"& !is.na(CityBuffStats$value.mean.diff),])

mean(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig], na.rm=T); sd(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig], na.rm=T)

summary(CityBuffStats$biomeName[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig])/summary(CityBuffStats$biomeName[CityBuffStats$factor=="LST"])



mean(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST"], na.rm=T); sd(CityBuffStats$value.mean.diff[CityBuffStats$factor=="LST"], na.rm=T)


nrow(CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig,])
nrow(CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig & (grepl("Grassland", CityBuffStats$biomeName) | CityBuffStats$biomeName %in% c("Mediterranean", "Desert")),])

nrow(CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig,])/nrow(CityBuffStats[CityBuffStats$factor=="LST",])


summary(CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig,])
summary(CityBuffStats$biomeName[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig])


# Comparing mean tree cover ----
nrow(CityBuffStats[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig,])/nrow(CityBuffStats[CityBuffStats$factor=="tree",])
summary(CityBuffStats[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig,])

  
nrow(CityBuffStats[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig,])
summary(CityBuffStats[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig,])

CityBuffStats[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig & grepl("USA", CityBuffStats$ISOURBID) & grepl("Forest", CityBuffStats$biomeName),]

CityBuffStats[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig & grepl("USA", CityBuffStats$ISOURBID) & grepl("Grassland", CityBuffStats$biomeName),]


mean(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="tree"]); sd(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="tree"])
mean(CityBuffStats$value.mean.core[CityBuffStats$factor=="tree"]); sd(CityBuffStats$value.mean.core[CityBuffStats$factor=="tree"])

mean(CityBuffStats$value.mean.diff[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig]); sd(CityBuffStats$value.mean.diff[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig])

mean(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig]); sd(CityBuffStats$value.mean.buffer[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig])
# 
mean(CityBuffStats$value.mean.core[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig]); sd(CityBuffStats$value.mean.core[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig])

mean(CityBuffStats$value.mean.diff[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig]); sd(CityBuffStats$value.mean.diff[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig])

summary(CityBuffStats$biomeName[CityBuffStats$factor=="tree" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig])

nrow(CityBuffStats[CityBuffStats$factor=="other veg" & CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.sig,])/nrow(CityBuffStats[CityBuffStats$factor=="other veg",])
nrow(CityBuffStats[CityBuffStats$factor=="other veg" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.sig,])/nrow(CityBuffStats[CityBuffStats$factor=="other veg",])

# CityBuffStats.Summary <- aggregate(cbind(vale.mean.diff, ))


png(file.path(path.figs, "MetroCore_v_Buffer_Means_Histograms_Quick.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats) +
  facet_wrap(~factor, ncol=1, scales="free") +
  geom_histogram(aes(x=value.mean.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


CityBuffStats[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff< -5,]
# Outliers for city colder than buffer: TKM33404; US City with cooler city: USA21290

CityBuffStats[CityBuffStats$ISOURBID=="TKM33404",]
CityBuffStats[CityBuffStats$ISOURBID=="USA21290",] # This is Yakima Washington




# ##########################################



