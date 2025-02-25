# Looking at the ET Models and doing some predictions of water use and greening scenarios
library(ggplot2)

# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
library(raster); library(tidyr); library(scales)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
# path.google <- file.path(path.google)
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v4/")


# Adding info for the previous version so we can double check
# path.google2 <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v2")
# path.google2 <- file.path(path.google2)
# file.cityAll.stats2 <- file.path(path.google2, "city_stats_all.csv")


path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)


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
# Read in Data; do some cleanup ----
# ##########################################
# Reading in our old, full dataset
cityAll.stats <- read.csv(file.path(path.google, "city_stats_all.csv"))
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
# Get rid of anything that involves the ETmodel stuff
cityAll.stats <- cityAll.stats[,!grepl("ETmodel", names(cityAll.stats))]
cityAll.stats <- cityAll.stats[,!grepl("corr.", names(cityAll.stats))]
cityAll.stats <- cityAll.stats[,!grepl("trend", names(cityAll.stats))]
summary(cityAll.stats)


# Reading in our ET dataset
cityAll.ET <-  read.csv(file.path(path.google, "city_stats_all_ET.csv"))
cityAll.ET$ETmodel.R2adj[cityAll.ET$ETmodel.R2adj<0] <- NA
cityAll.ET <- cityAll.ET[!is.na(cityAll.ET$ETmodel.R2adj) & cityAll.ET$ETobs.max>1,] # get rid of anything we didn't model or that has a very low range of ET
summary(cityAll.ET)

cityAll.stats <- cityAll.stats[cityAll.stats$ISOURBID %in% cityAll.ET$ISOURBID[!is.na(cityAll.ET$ETmodel.R2adj)],]
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),9:25])
summary(cityAll.stats)

cityAll.ET <- merge(cityAll.ET, cityAll.stats[,c("ISOURBID", "biome", "biomeName")], all.x=T, all.y=F)
summary(cityAll.ET)

# Reading in the climate datasets -- GLDAS
# Units:
#  - temperature (Tair_f_inst_mean) - K  --> save as C to jive with MODIS data; note: is mean daily air temp; not day surface temp
#  - precipitation (Rainf_f_tavg_meanEvap_tavg_mean) - kg/m2/s (= mm/s) --> save as mm/day to jive with MODIS data
#  - evapotranspriation (Evap_tavg_mean) - kg/m2/s (= mm/s) --> save as mm/day to jive with MODIS data

#  Doing this as a loop because i cut out the straight climatology option
pb <- txtProgressBar(min=0, max=nrow(cityAll.ET), style=3)
for(i in 1:nrow(cityAll.ET)){
  CITY <- cityAll.ET$ISOURBID[i]
  cityGLDAS <- read.csv(file.path(path.raw, paste0(CITY, "_GLDAS21_annualMeans.csv")))
  cityAll.ET[i,c("ET.GLDAS", "Precip.GLDAS", "Tmean.GLDAS")] <- colMeans(cityGLDAS[,c("Evap_tavg_mean", "Rainf_f_tavg_mean", "Tair_f_inst_mean")], na.rm=T)
  
  cityAll.ET$ET.GLDAS[i] <- cityAll.ET$ET.GLDAS[i]*60*60*24
  cityAll.ET$Precip.GLDAS[i] <- cityAll.ET$Precip.GLDAS[i]*60*60*24
  cityAll.ET$Tmean.GLDAS[i] <- cityAll.ET$Tmean.GLDAS[i]-273.15
  
  setTxtProgressBar(pb, i)
}

summary(cityAll.ET)

cityAll.ET$ETpred.Precip <- cityAll.ET$ETpred.mean/cityAll.ET$Precip.GLDAS # less than 1 means more precip than used by veg
cityAll.ET$ETgldas.Precip <- cityAll.ET$ET.GLDAS/cityAll.ET$Precip.GLDAS # less than 1 means more precip than used by veg
summary(cityAll.ET)


write.csv(cityAll.ET, file.path(path.google, "city_stats_all_ET-GLDAS.csv"), row.names=F)
# ##########################################


# ##########################################
# Do some data exploration ----
# ##########################################
cityAll.ET <- read.csv(file.path(path.google, "city_stats_all_ET-GLDAS.csv"))
cityAll.ET$ETpixels.prop <- cityAll.ET$n.pixels.ET/cityAll.ET$n.pixels
cityAll.ET$biomeName <- factor(cityAll.ET$biomeName, levels=biome.order$biomeName)

summary(cityAll.ET)

length(which(cityAll.ET$Precip.GLDAS<0.01))
length(which(cityAll.ET$ETpixels.prop<0.75))

# cityAll.ET <- cityAll.ET[cityAll.ET$ISOURBID %in% StatsCombined$ISOURBID & cityAll.ET$Precip.GLDAS>=0.01 & cityAll.ET$ETpixels.prop>=0.75,]
summary(cityAll.ET)
nrow(cityAll.ET); 

hist(cityAll.ET$ETpixels.prop)


summary(cityAll.ET[cityAll.ET$ETmodel.R2adj<0.2,])
summary(as.factor(cityAll.ET$ISO3[cityAll.ET$ETmodel.R2adj<0.2]))
summary(as.factor(cityAll.ET$biomeName[cityAll.ET$ETmodel.R2adj<0.2]))
# head(cityAll.ET[cityAll.ET$ETmodel.R2adj<0.2 & cityAll.ET$ISO3=="USA",])
# head(cityAll.ET[cityAll.ET$ETmodel.R2adj<0.2 & cityAll.ET$ISO3=="IND",])
# tail(cityAll.ET[cityAll.ET$ETmodel.R2adj<0.2 & cityAll.ET$ISO3=="IND",])

length(which(cityAll.ET$ETpred.Precip<2))
nrow(cityAll.ET)
length(which(cityAll.ET$ETpred.Precip<2))/nrow(cityAll.ET)
hist(cityAll.ET$ETpred.Precip[cityAll.ET$ETpred.Precip<2])


world <- map_data("world"); 
world <- world[!world$long>180,]
grad.modfit <- c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177", "#49006a")

etR2.map <- ggplot(data=cityAll.ET[,]) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=ETmodel.R2adj), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="ET model\nR2-adj", colors=grad.modfit, n.breaks=13, oob=squish) +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(4, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.5, 0.5, "lines"))
etR2.map

png(file.path(path.figs, "ModelFitET_R2adj_Map.png"), height=6, width=12, units="in", res=220)
etR2.map
dev.off() 

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

plotRatioLog <- ggplot(data=cityAll.ET[,]) +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(x=biomeName, y=log(ETpred.Precip), fill=biomeName), scale="width") +
  geom_hline(yintercept=log(1), linetype="dashed") +
  annotate(geom="text", x=1.25, y=c(-2.5, 3), label=c("Precip Surplus", "Precip Deficit"), hjust=0) +
  scale_fill_manual(values=biome.pall.all) +
  labs(y="log(ET/Precip)", x="Biome") +
  theme_bw() +
  theme(axis.text.x = element_blank())

plotInputs <- cowplot::plot_grid(plotET, plotPrecip, ncol=1)

png(file.path(path.figs, "ETmodel_ET_vs_Precip_Current.png"), height=8, width=10, units="in", res=320)
cowplot::plot_grid(plotInputs, plotRatio, ncol=2, rel_widths=c(0.25, 0.75))
dev.off()

png(file.path(path.figs, "ETmodel_ET_vs_Precip_Current_Log.png"), height=8, width=10, units="in", res=320)
cowplot::plot_grid(plotInputs, plotRatioLog, ncol=2, rel_widths=c(0.25, 0.75))
dev.off()

 
plot(ETobs.mean ~ ETpred.mean, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(ETobs.mean ~ ET.GLDAS, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(ETpred.mean ~ ET.GLDAS, data=cityAll.ET); abline(a=0, b=1, col="red")
plot(Temp.ET.mean ~ Tmean.GLDAS, data=cityAll.ET); abline(a=0, b=1, col="red")

ggplot(data=cityAll.ET) +
  # coord_equal() +
  geom_point(aes(x=ETpred.mean, y=ET.GLDAS, color=biomeName)) +
  geom_abline(slope=1, intercept=0) +
  scale_color_manual(values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="top")
  
  
# Proportion of cities where precip exceeds estimated ET in our data
length(which(cityAll.ET$ETpred.Precip<1))/length(which(!is.na(cityAll.ET$ETpred.Precip)))
length(which(cityAll.ET$ETgldas.Precip<1))/length(which(!is.na(cityAll.ET$ETgldas.Precip)))

