# Looking at changes in water demand & availability for vegetation
# 1. [other scripts] Here’s what tree cover you need to offset urban heat; benchmark against current cover & trajectories (compare to Konijnendijk’s 3-30-300 rule) [done]
# 2. Here’s how much more water would needed to sustain that canopy; compared to current precipitation [have estimated ET; just need calcs]
# 3. In a warming climate, the amount of water needed to sustain vegetation will increase – how much will that be??
  

library(ggplot2)

# file paths for where to put the processed data ----
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")

# which(dir(file.path(path.cities, "data_processed_final"))=="JPN28893")

# file.cityClim <- file.path(path.cities, "city_climatology.csv")
# file.cityAll.stats <- file.path(path.cities, "city_stats_all.csv")

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

# -------------------
# Reading in base data form our original analyses ----
# -------------------
cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
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


# StatsCombined <- read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
# summary(StatsCombined)


biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]
biome.order$biomeCode <- car::recode(biome.order$biomeName, 
                                     " 'Taiga'='Tai';
                                       'Tundra'='Tun';
                                       'Montane Grassland/Savanna'='MGS';
                                       'Temperate Broadleaf Forest'='TeBF';
                                       'Temperate Conifer Forest'='TeCF';
                                       'Temperate Grassland/Savanna'='TeGS';
                                       'Mediterranean'='Med';
                                       'Desert'='Des';
                                       'Flooded Grassland/Savanna'='FGS';
                                       'Tropical Grassland/Savanna'='TrGS';
                                       'Tropical Dry Broadleaf Forest'='TrDBF';
                                       'Tropical Conifer Forest'='TrCF';
                                       'Tropical Moist Broadleaf Forest'='TrMBF';
                                       'Mangroves'='Man'")
biome.order
# -------------------

# -------------------
# Reading in the climate change data from TerraClim ----
# Units:
#  - temperature (tmax, tmin) - C (month mean)
#  - preciptiation (ppt) - mm/month (total)
#  - evapotranspriation (aet, pet) - mm/month (total)
#  - vapor pressure deficit (def) - kPA (month mean)
#  - soil moisture (soil) - mm (total column; end of month)
# -------------------
cityClim <- read.csv(file.path(path.google, "city_climatology.csv"))
cityClim$TIME <- as.factor(cityClim$TIME)
summary(cityClim)

# Convert monthly totals to daily means
cityClim[cityClim$LATITUDE<0,c("ppt", "aet", "pet")] <- cityClim[cityClim$LATITUDE<0,c("ppt", "aet", "pet")]/sum(lubridate::days_in_month(1:2))
cityClim[cityClim$LATITUDE>0,c("ppt", "aet", "pet")] <- cityClim[cityClim$LATITUDE>0,c("ppt", "aet", "pet")]/sum(lubridate::days_in_month(7:8))
summary(cityClim)

summary(cityClim[cityClim$ppt==0,])
summary(cityClim[cityClim$soil==0,])
cityClim[cityClim$soil==0,]
summary(cityClim[cityClim$aet==0,])

# Doing some quick looks at how ET compares to precip
summary(cityClim$ppt[cityClim$TIME=="current"]>cityClim$aet[cityClim$TIME=="current"]) # precip exceeds actual ET; ~33%
summary(cityClim$ppt[cityClim$TIME=="current"]>cityClim$pet[cityClim$TIME=="current"]) # precip exceets PET

summary(cityClim$ppt[cityClim$TIME=="+4C"]>cityClim$aet[cityClim$TIME=="+4C"]) # precip exceeds actual ET; ~50%
summary(cityClim$ppt[cityClim$TIME=="+4C"]>cityClim$pet[cityClim$TIME=="+4C"])

# ggplot(data=cityClim) +
#   facet_grid(TIME~.) +
#   geom_histogram(aes(x=def))

for(i in 1:nrow(cityAll.stats)){
  URBID <- cityAll.stats$ISOURBID[i]
  rowRef <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="current")
  rowFut2 <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="+2C")
  rowFut4 <- which(cityClim$ISOURBID==URBID & cityClim$TIME=="+4C")
  
  # rowDiff <- which(diffClim$ISOURBID==URBID)
  cityAll.stats[i,c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")] <- cityClim[rowRef, c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")]
  cityAll.stats[i,c("tmax2", "tmin2", "ppt2", "aet2", "pet2", "def2", "soil2")] <- cityClim[rowFut2, c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")]
  cityAll.stats[i,c("tmax4", "tmin4", "ppt4", "aet4", "pet4", "def4", "soil4")] <- cityClim[rowFut4, c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")]
  
}

summary(cityAll.stats)
# -------------------

# -------------------
# Quick look at MODIS vs. terraclim aet
# -------------------
# ggplot(data=cityAll.stats) +
#   geom_point(aes(x=aet, y=ET.mean))
ggplot(data=cityAll.stats) +
  facet_wrap(~biomeName) +
  geom_point(aes(x=ET.mean, y=aet, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  scale_color_manual(values=biome.pall.all) +
  guides(color="none") +
  theme_bw()

etmod <- lm(ET.mean ~ aet, data=cityAll.stats)
summary(etmod)

cityAll.stats$ET.est <- cityAll.stats$ETmodel.tree.slope*cityAll.stats$tree.mean + cityAll.stats$ETmodel.veg.slope*cityAll.stats$veg.mean
summary(cityAll.stats)

ggplot(data=cityAll.stats) +
  facet_wrap(~biomeName) +
  geom_point(aes(x=ET.mean, y=ET.est, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  scale_color_manual(values=biome.pall.all) +
  guides(color="none") +
  theme_bw()


etmod2 <- lm(ET.mean ~ ET.est-1, data=cityAll.stats)
summary(etmod2)

ggplot(data=cityAll.stats) +
  facet_wrap(~biomeName) +
  geom_point(aes(x=aet, y=ET.est, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  scale_color_manual(values=biome.pall.all) +
  guides(color="none") +
  theme_bw()
  
etmod2 <- lm(ET.aet ~ ET.est-1, data=cityAll.stats)
summary(etmod2)

# -------------------


# -------------------
# Quick calculation of trees needed to offset the UHI: LST diff / tree cooling slope ----
# -------------------
# Subsetting to just cities with  tree cooling AND  UHI
statsAnaly <- cityAll.stats[cityAll.stats$LSTmodel.tree.slope<0 & cityAll.stats$value.LST.diff>0 & cityAll.stats$ETmodel.tree.slope>0,]

# How much more trees would there need to be to offset UHI?
statsAnaly$TreeCoverUHINeed <- -statsAnaly$value.LST.diff/statsAnaly$LSTmodel.tree.slope
statsAnaly$OtherCoverUHINeed <- -statsAnaly$value.LST.diff/statsAnaly$LSTmodel.veg.slope

# 
statsAnaly$TreeCoverTargetUHI <- statsAnaly$TreeCoverUHINeed + statsAnaly$value.tree.core
statsAnaly$OtherCoverTargetUHI <- statsAnaly$OtherCoverUHINeed + statsAnaly$value.other.core
summary(statsAnaly)

ggplot(data=statsAnaly[,], aes(x=biomeCode, y=TreeCoverTargetUHI, fill="Biome Target", color="Biome Target")) +  
  geom_bar(stat="summary", fun="median") +
  # geom_segment(yend=0, aes(xend=biomeCode), stat="summary", fun="median", size=2) +
  geom_violin(aes(x=biomeCode, y=value.tree.core, fill="Current", color="Current"), scale="width") +
  geom_hline(yintercept=30, linetype="dashed") +
  annotate("text", x=1, y=35, label="30%\nTarget") +
  # geom_point(stat="summary", fun="median", size=5) +
  scale_fill_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4])) +
  scale_color_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4])) +
  labs(x="Biome", y="Tree Cover (%)") +
  scale_y_continuous(limits=c(0,70), expand=c(0,0)) +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face='bold'),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

# -------------------



# -------------------
# Water Analysis 1: how much water does the current canopy need; would be needed to increase tree cover to target ----
# -------------------
# How much water the current canopy needs: current % tree cover x ET Tree slope --> mm water/summer
# Sanity check on ET estimates
# statsAnaly$ETest <- statsAnaly$value.tree.core*statsAnaly$ETmodel.tree.slope+statsAnaly$value.other.core*statsAnaly$ETmodel.veg.slope
summary(statsAnaly)

statsAnaly$CurrentCanopyWater <- statsAnaly$value.tree.core*statsAnaly$ETmodel.tree.slope
statsAnaly$CurrentVegWater <- statsAnaly$value.other.core*statsAnaly$ETmodel.veg.slope
statsAnaly$CurrentTotalWater <- statsAnaly$CurrentCanopyWater+statsAnaly$CurrentVegWater
statsAnaly$TargetCanopyWater <- statsAnaly$TreeCoverTargetUHI*statsAnaly$ETmodel.tree.slope
statsAnaly$TargetTotalWater <- statsAnaly$TargetCanopyWater + statsAnaly$CurrentVegWater
statsAnaly$TargetTotalWater.diff <- statsAnaly$TargetTotalWater - statsAnaly$CurrentTotalWater
statsAnaly$PrecipDiff.CurrentAET <- statsAnaly$ppt - statsAnaly$aet
statsAnaly$PrecipDiff.CurrentTotal <- statsAnaly$ppt - statsAnaly$CurrentTotalWater
statsAnaly$PrecipDiff.TargetTotal <- statsAnaly$ppt - statsAnaly$CurrentTotalWater
summary(statsAnaly)
summary(statsAnaly$TargetTotalWater/statsAnaly$CurrentTotalWater) # Looking at target water use of funciton of current
summary(statsAnaly$CurrentTotalWater/statsAnaly$ppt) # Current water use relative to precip inputs
summary(statsAnaly$TargetTotalWater/statsAnaly$ppt) # Target water use relative to precip inputs


statsAnalyStack <- stack(statsAnaly[,c("ppt", "CurrentTotalWater", "TargetTotalWater")])
statsAnalyStack[,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode")] <- statsAnaly[,c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode")]
statsAnalyStack$level <- ifelse(grepl("ppt", statsAnalyStack$ind), "precip (current)", ifelse(grepl("Current", statsAnalyStack$ind), "Water Use - Current", "Water Use - Target"))
summary(statsAnalyStack)

ggplot(data=statsAnalyStack) +
  facet_wrap(~biomeCode, scales="free_x") +
  coord_cartesian(ylim=c(0,10)) +
  geom_boxplot(aes(x=biomeName, y=values, fill=level))

summary(as.factor(statsAnaly$biomeName[statsAnaly$ETmodel.R2adj>=0.5]))
summary(as.factor(statsAnaly$biomeName[statsAnaly$ETmodel.R2adj<0.5]))

ggplot(data=statsAnaly[statsAnaly$ETmodel.R2adj>=0.5,]) +
  geom_point(aes(x=aet, y=CurrentTotalWater))
# -------------------







# -------------------
# -------------------
# -------------------