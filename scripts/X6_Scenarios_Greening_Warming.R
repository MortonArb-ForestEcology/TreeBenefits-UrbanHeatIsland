# Doing some warming and greening projections on cities
# Vegetation: 1) As is; 2) Green Blob (even at target) 3) Uniform increase 4) bottom-up
# -- Vegetation goal: How much water will a city need to meet its goals?  How does canopy equality affect water needs?
# -- Vegetation Methods: apply scenario to each pixel; for 4) ???
# -- Save along the way: percent of each city in 5% bins so we can graph the changes in distribution
# Climate: 1) current; 2) mid-century: ssp 245, 585; 3) end-century: ssp 245, 585
# -- Climate Goal: How will the water needs of the canopy change with climate change; how does that compare to changes in precip 
# -- Climate Method: look at the delta in temperature; add uniformly to area (acknowledge we're dealing with air vs. lst temps here, but it's a start!); compare to delta in precip; all deltas calculated as a change from weighted-average temp & precip for each GCM x scenario; for precip consider both absolute and % change
# -- Save along the way: climate change projection stats for each city: average warming, drying/wetting per city 
#        

###########################################
# Load Packages, set paths ----
###########################################
library(tidyr)

library(ggplot2); library(RColorBrewer); library(cowplot)
library(ggalt); library(sf)
library(mapproj)

path.google <- file.path("~/Google Drive/Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google, "data_processed_final")

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

###########################################


###########################################
# Read in city data & summarizing baseline ----
###########################################
# cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
# cityAll.stats <- cityAll.stats[,!grepl("ETmodel", names(cityAll.stats))]
# cityAll.stats <- cityAll.stats[,!grepl("corr.", names(cityAll.stats))]
# cityAll.stats <- cityAll.stats[,!grepl("trend", names(cityAll.stats))]
# summary(cityAll.stats)
# 
# summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])

StatsCombined <- read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
StatsCombined <- StatsCombined[,!(grepl("ETmodel", names(StatsCombined)))]
summary(StatsCombined)


cityAll.ET <- read.csv(file.path(path.google, "city_stats_all_ET-GLDAS.csv"))
cityAll.ET$ETpixels.prop <- cityAll.ET$n.pixels.ET/cityAll.ET$n.pixels


biome.order <- aggregate(Tmean.GLDAS ~ biomeName, data=cityAll.ET, FUN=mean)
biome.order <- biome.order[order(biome.order$Tmean.GLDAS),]

StatsCombined$biomeName <- factor(StatsCombined$biomeName, levels=biome.order$biomeName)
cityAll.ET$biomeName <- factor(cityAll.ET$biomeName, levels=biome.order$biomeName)


length(which(cityAll.ET$Precip.GLDAS<0.01))
length(which(cityAll.ET$ETpixels.prop<0.75))

cityAll.ET <- cityAll.ET[cityAll.ET$ISOURBID %in% StatsCombined$ISOURBID & cityAll.ET$Precip.GLDAS>=0.01 & cityAll.ET$ETpixels.prop>=0.75,]
summary(cityAll.ET)
nrow(cityAll.ET); nrow(StatsCombined)

hist(cityAll.ET$ETpixels.prop)


length(which(cityAll.ET$ETpred.Precip<2))
nrow(cityAll.ET)
hist(cityAll.ET$ETpred.Precip[cityAll.ET$ETpred.Precip<2])

etR2 <- ggplot(data=cityAll.ET[,]) +
  geom_histogram(aes(x=ETmodel.R2adj, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all) +
  guides(fill="none") +
  theme_bw()

etR2

etRMSE <- ggplot(data=cityAll.ET[,]) +
  geom_histogram(aes(x=ETmodel.RMSE, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all) +
  guides(fill="none") +
  theme_bw() 
etRMSE

# ############## 
# Trying to better contextualize our RMSE 
# https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
# ############## 
# THis *might* be called a "scatter index" --> metric of RMSE relative to mean value
# https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
etRMSEmean <- ggplot(data=cityAll.ET[,]) +
  geom_histogram(aes(x=ETmodel.RMSE/ETobs.mean, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all) +
  guides(fill="none") +
  theme_bw() 
etRMSEmean

# If the NRMSE is further categorized into let’s say low, medium or high performance, using the standard deviation to normalize could be a good option for the following reason: The sd-based NRMSE represent the ratio between the variation not explained by the regression vs the overall variation in Y. If the regression explains all of the variation in Y, nothing gets unexplained and the RMSE, and consequently NRMSE is zero. **If the regression explains some part and leaves some other unexplained, which is at a similar scale than the overall variation, the ratio will be around 1.** Anything beyond will indicate a much greater variation or noise than in the variable itself and consequently a low predictability.
etRMSEsd <- ggplot(data=cityAll.ET[,]) +
  geom_histogram(aes(x=ETmodel.RMSE/ETobs.sd, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all) +
  guides(fill="none") +
  theme_bw() 
etRMSEsd
# ############## 
png(file.path(path.figs, "ETmodel_PerformanceSummaries.png"), height=8, width=8, units="in", res=320)
cowplot::plot_grid(etR2, etRMSE, etRMSEmean, etRMSEsd, ncol=2)
dev.off()

modisET <- ggplot(data=cityAll.ET) +
  # coord_equal() +
  geom_point(aes(x=ETpred.mean, y=ETobs.mean, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  annotate(geom="text", x=c(0.25, 4.75), y=c(4.5, 0.5), label=c("Observed (MODIS) higher", "Predicted higher"), hjust=c(0, 1)) +
  scale_color_manual(values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="top")

gldasET <- ggplot(data=cityAll.ET) +
  # coord_equal() +
  geom_point(aes(x=ETpred.mean, y=ET.GLDAS, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  annotate(geom="text", x=c(0.25, 4.75), y=c(4.5, 0.5), label=c("GLDAS (27 km) higher", "Predicted (1 km) higher"), hjust=c(0, 1)) +
  scale_color_manual(values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="top")

png(file.path(path.figs, "ETmodel_ValidationSummaries.png"), height=8, width=8, units="in", res=320)
cowplot::plot_grid(modisET, gldasET, ncol=1)
dev.off()



plotET <- ggplot(data=cityAll.ET[,]) +
  geom_violin(aes(x=biomeName, y=ETpred.mean, fill=biomeName), scale="width") +
  scale_fill_manual(values=biome.pall.all) +
  labs(y="Estimated ET (kg/m2/day)", x="Biome") +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_blank())

plotPrecip <-  ggplot(data=cityAll.ET[,]) +
  geom_violin(aes(x=biomeName, y=Precip.GLDAS, fill=biomeName), scale="width") +
  scale_fill_manual(values=biome.pall.all) +
  labs(y="GLDAS Precip (kg/m2/day)", x="Biome") +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_blank())

plotRatio <- ggplot(data=cityAll.ET[,]) +
  coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(x=biomeName, y=ETpred.Precip, fill=biomeName), scale="width") +
  geom_hline(yintercept=1, linetype="dashed") +
  annotate(geom="text", x=1.25, y=c(0.1, 1.9), label=c("Precip Surplus", "Precip Deficit"), hjust=0) +
  scale_fill_manual(values=biome.pall.all) +
  labs(y="ET/Precip", x="Biome") +
  theme_bw() +
  theme(axis.text.x = element_blank())

plotInputs <- cowplot::plot_grid(plotET, plotPrecip, ncol=1)

png(file.path(path.figs, "ETmodel_ET_vs_Precip_Current.png"), height=8, width=10, units="in", res=320)
cowplot::plot_grid(plotInputs, plotRatio, ncol=2, rel_widths=c(0.25, 0.75))
dev.off()
###########################################




###########################################
# Bringing in climate change and running some scenarios ----
###########################################

# Read in CMIP6 metadata 
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_deviations.csv"))
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
summary(cmip6)

###########################################


###########################################
# Iterating through climate change scenarios
###########################################
###########################################
