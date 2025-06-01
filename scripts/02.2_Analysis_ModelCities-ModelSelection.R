# This script will just pull the data and run some LST models to decide which one we should move forward with
library(ggplot2)

overwrite=T


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


# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/data_processed_final")

file.cityStatsRegion <- file.path(path.cities, "../city_stats_model-selection.csv")

cityStatsRegion <- read.csv(file.cityStatsRegion)

cityStatsRegion$biomeName <- car::recode(cityStatsRegion$biome, 
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

# Looking at dR2adj --> Pos is alternate does better
cityStatsRegion$dR2adj.Log <- cityStatsRegion$LSTmodelLog.R2adj - cityStatsRegion$LSTmodel.R2adj 
cityStatsRegion$dR2adj.SCover <- cityStatsRegion$LSTmodelSCover.R2adj - cityStatsRegion$LSTmodel.R2adj 
cityStatsRegion$dR2adj.SCoverTree <- cityStatsRegion$LSTmodelSCoverTree.R2adj - cityStatsRegion$LSTmodel.R2adj 
cityStatsRegion$dR2adj.S3D <- cityStatsRegion$LSTmodelS3D.R2adj - cityStatsRegion$LSTmodel.R2adj 

# change in RMSE; negative means alternate does better
cityStatsRegion$dRMSE.Log <-cityStatsRegion$LSTmodelLog.RMSE -  cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSE.SCover <- cityStatsRegion$LSTmodelSCover.RMSE - cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSE.SCoverTree <- cityStatsRegion$LSTmodelSCoverTree.RMSE - cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSE.S3D <- cityStatsRegion$LSTmodelS3D.RMSE - cityStatsRegion$LSTmodel.RMSE

# Change in AIC; negative means reduction
cityStatsRegion$dAIC.Log <- cityStatsRegion$LSTmodelLog.AIC - cityStatsRegion$LSTmodel.AIC
cityStatsRegion$dAIC.SCover <-  cityStatsRegion$LSTmodelSCover.AIC - cityStatsRegion$LSTmodel.AIC
cityStatsRegion$dAIC.SCoverTree <-  cityStatsRegion$LSTmodelSCoverTree.AIC - cityStatsRegion$LSTmodel.AIC
# cityStatsRegion$dAIC.SCoverTree2 <-  cityStatsRegion$LSTmodelSCoverTree.AIC - cityStatsRegion$LSTmodelSCover.AIC
cityStatsRegion$dAIC.S3D <-  cityStatsRegion$LSTmodelS3D.AIC - cityStatsRegion$LSTmodel.AIC

# Looking at dRMSE as a percent reduction
cityStatsRegion$dRMSEper.Log <- cityStatsRegion$dRMSE.Log/cityStatsRegion$LSTmodel.RMSE

cityStatsRegion$dRMSEper.SCover <- cityStatsRegion$dRMSE.SCover/cityStatsRegion$LSTmodel.RMSE
cityStatsRegion$dRMSEper.SCoverTree <- cityStatsRegion$dRMSE.SCoverTree/cityStatsRegion$LSTmodel.RMSE
# cityStatsRegion$dRMSEper.SCoverTree2 <- cityStatsRegion$dRMSE.SCoverTree/cityStatsRegion$LSTmodelSCover.RMSE
cityStatsRegion$dRMSEper.S3D <- cityStatsRegion$dRMSE.S3D/cityStatsRegion$LSTmodel.RMSE


mean(cityStatsRegion$LSTmodel.R2adj, na.rm=T); sd(cityStatsRegion$LSTmodel.R2adj, na.rm=T)
mean(cityStatsRegion$dR2adj.SCover, na.rm=T); sd(cityStatsRegion$dR2adj.SCover, na.rm=T)
mean(cityStatsRegion$dR2adj.S3D, na.rm=T); sd(cityStatsRegion$dR2adj.S3D, na.rm=T)


mean(cityStatsRegion$LSTmodel.RMSE, na.rm=T); sd(cityStatsRegion$LSTmodel.RMSE, na.rm=T)
mean(cityStatsRegion$dRMSE.SCover, na.rm=T); sd(cityStatsRegion$dRMSE.SCover, na.rm=T)
mean(cityStatsRegion$dRMSE.S3D, na.rm=T); sd(cityStatsRegion$dRMSE.S3D, na.rm=T)

mean(cityStatsRegion$dRMSEper.SCover, na.rm=T); sd(cityStatsRegion$dRMSEper.SCover, na.rm=T)

mean(cityStatsRegion$dRMSEper.S3D, na.rm=T); sd(cityStatsRegion$dRMSEper.S3D, na.rm=T)
mean(cityStatsRegion$dAIC.S3D, na.rm=T); sd(cityStatsRegion$dAIC.S3D, na.rm=T)


summary(cityStatsRegion)
cityStatsRegion[cityStatsRegion$dRMSE.SCover< -0.5 & !is.na(cityStatsRegion$dR2adj.SCover),]

statsBiome <- aggregate(cbind(LSTmodel.AIC, LSTmodel.R2adj, LSTmodel.RMSE, dAIC.SCover, dR2adj.SCover, dRMSE.SCover, dRMSEper.SCover, dAIC.S3D, dR2adj.S3D, dRMSE.S3D, dRMSEper.S3D) ~ biome, data=cityStatsRegion, FUN=mean, na.rm=T)
statsBiome

write.csv(statsBiome, file.path(path.cities, "../Biome_stats_model-selection.csv"), row.names=F)



drmse.histo.biome <- ggplot(data=cityStatsRegion[!is.na(cityStatsRegion$biome),])+
  geom_histogram(aes(x=dRMSEper.S3D, fill=biomeName)) +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(size=rel(0.8), color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
drmse.histo.biome

dr2.histo.biome <- ggplot(data=cityStatsRegion[!is.na(cityStatsRegion$biome),])+
  geom_histogram(aes(x=dR2adj.S3D, fill=biomeName)) +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(size=rel(0.8), color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dr2.histo.biome

