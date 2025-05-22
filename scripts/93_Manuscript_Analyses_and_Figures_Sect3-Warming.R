# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
# Key Results: (What Christy needs to get numbers for)
#  3.0. SUPPLEMENT Figure: The amount of warming & precipitation varies across the globe; this is CMIP6, not us, so no need to put in main MS
#. 3.1. Warming will cause an X% increase in ET; X% of cities will see precip not keep pace with this, resulting in XX% of cities 
#  3.2. Although we estimate XX biomes to have the greatest proportion of cities in a canopy water deficit, the biggest shift in the distribution is in temperate forest biomes, particularly cities in Europe and the US

library(ggplot2); library(cowplot); library(scales)

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
path.cities <- file.path(path.google, "data_processed_final")

path.figsMS <- file.path(path.google, "figures_manuscript")
path.figsExplore <- file.path(path.google, "figures_exploratory")
dir.create(path.figsMS, recursive=T, showWarnings=F)

path.MS <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Submission 5 - Nature Climate Change/")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Read in some base datasets etc.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
source("0_color_palettes_etc.R")

biome.order <-  read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis_BiomeOrder.csv"))
biome.order

StatsCombined <-  read.csv(file.path(path.google, "UHIs-FinalCityDataForAnalysis.csv"))
StatsCombined$biomeName <- factor(StatsCombined$biomeName, levels=biome.order$biomeName)
StatsCombined$biomeCode <- factor(StatsCombined$biomeCode, levels=biome.order$biomeCode)
StatsCombined$biomeNameRev <- factor(StatsCombined$biomeName, levels=rev(levels(StatsCombined$biomeName)))
StatsCombined$biomeCodeRev <- factor(StatsCombined$biomeCode, levels=rev(levels(StatsCombined$biomeCode)))
summary(StatsCombined)

# Read in the ET base data
cityETStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
cityETStats[,c("dET.ssp245.2050", "dET.ssp245.2100", "dET.ssp585.2050", "dET.ssp585.2100")] <- cityETStats[,c("modET.ssp245.2050", "modET.ssp245.2100", "modET.ssp585.2050", "modET.ssp585.2100")] - cityETStats$modET.Base
summary(cityETStats)

StatsCombined <- merge(StatsCombined, cityETStats[,c("ISOURBID", "modET.Base")], all.x=T, all.y=F)
StatsCombined$modET.Precip <- StatsCombined$modET.Base/StatsCombined$Precip.GLDAS
StatsCombined$Scenario <- c("Present")
StatsCombined$Time <- c("2020")
summary(StatsCombined)

# Doing the new precip deficit stats
StatsCombined$modET.MegaLiters <- StatsCombined$modET.Base*StatsCombined$SQKM_FINAL
StatsCombined$ET.Precip.diff <- StatsCombined$Precip.GLDAS - StatsCombined$modET.Base
StatsCombined$ET.Precip.diff.per <- StatsCombined$ET.Precip.diff/StatsCombined$Precip.GLDAS
StatsCombined$DeficitError.RMSExValid <- ifelse(abs(StatsCombined$ET.Precip.diff)<StatsCombined$ETxValid.spatRMSE.mean, T, F) # Using the RMSE from the spatial cross-validation since
# kg/m2 = ?kg/km2; 1km2 = 10^3*10^3 = 10^6; 1^6 = mega
StatsCombined$PrecipDiff.MegaLiters <- StatsCombined$ET.Precip.diff*StatsCombined$SQKM_FINAL
StatsCombined$Risk.per <- ifelse(StatsCombined$modET.Precip>1 & !StatsCombined$DeficitError.RMSExValid, 100, 0)
StatsCombined$Uncert.per <- ifelse(StatsCombined$DeficitError.RMSExValid, 100, 0)

# Creating a combined ET data frame
# NOTE that for the cmip6 scenarios, we added the change in temp to the gldas air temp; so lets create a precip correction
cmip6 <- read.csv(file.path(path.google, "city_stats_all_CMIP6_ET.csv"))
cmip6 <- cmip6[cmip6$ISOURBID %in% StatsCombined$ISOURBID,]
cmip6$Scenario <- car::recode(cmip6$Scenario, "'ssp245'='SSP2-4.5'; 'ssp585'='SSP5-8.5'")
cmip6$Scenario <- as.factor(cmip6$Scenario)
cmip6$Time <- as.factor(cmip6$Time)
cmip6 <- merge(cmip6, StatsCombined[,c("ISOURBID", "SQKM_FINAL", "biomeName", "biomeCode", "Tmean.GLDAS", "Precip.GLDAS", "ET.GLDAS", "modET.Base", "ETxValid.timeError", "ETxValid.timeRMSE")], all.x=T, all.y=F)
cmip6$modET.diff <- cmip6$modET - cmip6$modET.Base
cmip6$modET.perChange <- cmip6$modET/cmip6$modET.Base
cmip6$modET.MegaLiters <- cmip6$modET * cmip6$SQKM_FINAL
summary(cmip6)

# summary(cmip6)


# Looking at the ET vs Precip Ratio --> NOTE: Using adjusted PR because we adjusted ET
cmip6$pr.adj <- cmip6$Precip.GLDAS*cmip6$pr.per # Creating an adjusted daily precip by looking at the % change in pr for each run
cmip6$modET.Precip <- cmip6$modET/cmip6$pr.adj
cmip6$modET.Precip[cmip6$modET.Precip==Inf] <- NA
cmip6$ET.Precip.diff <- cmip6$pr.adj - cmip6$modET
cmip6$ET.Precip.diff.per <- cmip6$ET.Precip.diff/cmip6$pr.adj
cmip6$ET.Precip.diff.per[cmip6$ET.Precip.diff.per==Inf] <- NA
cmip6$PrecipDiff.MegaLiters <- cmip6$ET.Precip.diff*cmip6$SQKM_FINAL
cmip6$DeficitError.RMSExValid <- ifelse(abs(cmip6$ET.Precip.diff)<cmip6$ETxValid.timeRMSE, T, F) # Using the RMSE from from the *temporal* cross-validation

summary(cmip6[cmip6$modET.Precip<1,])
summary(cmip6[cmip6$modET.Precip>1,])
# cmip6$modET.PrecipLog <- log(cmip6$modET.Precip)
summary(cmip6)

write.csv(cmip6[cmip6$Time==2100,], file.path(path.MS, "SupplementalData-3_city_stats_all_CMIP6_ET.csv"), row.names=F)

# Looking at the distribution of cities by model & scenario based on comments from R3
png(file.path(path.figsExplore, "CMIP6-EnsembleMembers_ET_vs_Precip_Log.png"), height=8, width=10, units="in", res=320)
ggplot(data=cmip6[cmip6$Time=="2100" & !is.na(cmip6$modET.Precip) , ]) +
  # facet_grid(Scenario~.) +
  facet_wrap(~GCM) +
  geom_violin(aes(x=Scenario, y=log(modET.Precip), fill=biomeCode), scale="width", linewidth=0.1) +
  geom_hline(yintercept=0, linewidth=0.2, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.ShortCB) +
  # scale_color_manual(values=biomeCode.pall.ShortCB) +
  theme_bw()
dev.off()

png(file.path(path.figsExplore, "CMIP6-EnsembleMembers_PrecipDeficit_mmDay.png"), height=8, width=10, units="in", res=320)
ggplot(data=cmip6[cmip6$Time=="2100" & !is.na(cmip6$modET.Precip) , ]) +
  # facet_grid(Scenario~.) +
  facet_wrap(~GCM) +
  geom_violin(aes(x=Scenario, y=ET.Precip.diff, fill=biomeCode), scale="width", linewidth=0.1) +
  geom_hline(yintercept=0, linewidth=0.2, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.ShortCB) +
  # scale_color_manual(values=biomeCode.pall.ShortCB) +
  theme_bw()
dev.off()

png(file.path(path.figsExplore, "CMIP6-EnsembleMembers_TempChange.png"), height=8, width=10, units="in", res=320)
ggplot(data=cmip6[cmip6$Time=="2100"  , ]) +
  # facet_grid(Scenario~.) +
  facet_wrap(~GCM) +
  geom_violin(aes(x=Scenario, y=tas.diff, fill=biomeCode), scale="width", linewidth=0.1) +
  geom_hline(yintercept=0, linewidth=0.2, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.ShortCB) +
  scale_y_continuous(name="Temp change (˚C)") +
  # scale_color_manual(values=biomeCode.pall.ShortCB) +
  theme_bw()
dev.off()


png(file.path(path.figsExplore, "CMIP6-EnsembleMembers_PrecipChange.png"), height=8, width=10, units="in", res=320)
ggplot(data=cmip6[cmip6$Time=="2100"  , ]) +
  # facet_grid(Scenario~.) +
  facet_wrap(~GCM) +
  coord_cartesian(ylim=c(0,2.5)) +
  # geom_violin(aes(x=Scenario, y=log(pr.per), fill=biomeCode), scale="width", linewidth=0.1) +
  geom_violin(aes(x=Scenario, y=pr.per, fill=biomeCode), scale="width", linewidth=0.1) +
  geom_hline(yintercept=1, linewidth=0.2, linetype="dashed") +
  scale_fill_manual(values=biomeCode.pall.ShortCB) +
  scale_y_continuous(name="Precip. change (%)") +
  # scale_color_manual(values=biomeCode.pall.ShortCB) +
  theme_bw()
dev.off()


ggplot(data=cmip6[cmip6$Time=="2100" & !is.na(cmip6$modET.Precip) , ]) +
  # facet_grid(Scenario~.) +
  facet_wrap(~GCM) +
  geom_violin(aes(x=Scenario, y=log(modET.Precip), fill=biomeCode), scale="width", linewidth=0.1) +
  scale_fill_manual(values=biomeCode.pall.ShortCB) +
  # scale_color_manual(values=biomeCode.pall.ShortCB) +
  theme_bw()

# geom_violin(aes(x=biomeCode, y=log(modET.Precip), fill=biomeCode, color=biomeCode), binaxis="y", stackdir="center", alpha=0.5, dotsize=0.2)

# Checking the distribution of values of the ET/Precip ratio to figure out whether we should rely on mean or median
# This doesn't look bad!
ggplot(data=cmip6) +
  facet_grid(Scenario~.) +
  geom_histogram(aes(x=log(modET.Precip)))

cmip6AggMean <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.MegaLiters, modET.diff, modET.perChange, modET.Precip, ET.Precip.diff, ET.Precip.diff.per, PrecipDiff.MegaLiters)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=mean, na.rm=T)
cmip6AggMean$biomeName <- factor(cmip6AggMean$biomeName, levels=biome.order$biomeName)
summary(cmip6AggMean)

cmip6AggSD <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.perChange, modET.Precip, ET.Precip.diff, ET.Precip.diff.per, PrecipDiff.MegaLiters)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=sd, na.rm=T)
cmip6AggSD$biomeName <- factor(cmip6AggSD$biomeName, levels=biome.order$biomeName)
summary(cmip6AggSD)

# Want to look at the proportion of GCMs with ET risk and uncertain
cmip6AggN <- aggregate(cbind(DeficitError.RMSExValid)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6[!is.na(cmip6$DeficitError.RMSExValid),], FUN=length)
cmip6AggN$biomeName <- factor(cmip6AggMean$biomeName, levels=biome.order$biomeName)
names(cmip6AggN)[names(cmip6AggN)=="DeficitError.RMSExValid"] <- "N.GCM"
summary(cmip6AggN)

cmip6AggRisk <- aggregate(cbind(DeficitError.RMSExValid)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6[!is.na(cmip6$DeficitError.RMSExValid) & cmip6$modET.Precip>1 & !(cmip6$DeficitError.RMSExValid),], FUN=length)
names(cmip6AggRisk)[names(cmip6AggRisk)=="DeficitError.RMSExValid"] <- "N.Risk"
summary(cmip6AggRisk)

cmip6AggUncert <- aggregate(cbind(DeficitError.RMSExValid)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6[!is.na(cmip6$DeficitError.RMSExValid) & (cmip6$DeficitError.RMSExValid),], FUN=length)
names(cmip6AggUncert)[names(cmip6AggUncert)=="DeficitError.RMSExValid"] <- "N.Uncertain"
summary(cmip6AggUncert)

cmip6AggN <- merge(cmip6AggN, cmip6AggRisk, all=T)
cmip6AggN <- merge(cmip6AggN, cmip6AggUncert, all=T)
cmip6AggN$N.Risk[is.na(cmip6AggN$N.Risk)] <- 0
cmip6AggN$N.Uncertain[is.na(cmip6AggN$N.Uncertain)] <- 0

cmip6AggN$Risk.per <- round(cmip6AggN$N.Risk/cmip6AggN$N.GCM*100, 0)
cmip6AggN$Uncert.per <- round(cmip6AggN$N.Uncertain/cmip6AggN$N.GCM*100, 0)
summary(cmip6AggN)

cmip6AggMean <- merge(cmip6AggMean, cmip6AggN, all=T)
summary(cmip6AggMean)


cmip6AggMean$Risk.Level <- ifelse(cmip6AggMean$Risk.per>=66, "High", ifelse(cmip6AggMean$Risk.per<=33, "Low", "Medium"))
cmip6AggMean$Uncert.Level <- ifelse(cmip6AggMean$Uncert.per>=66, "High", ifelse(cmip6AggMean$Uncert.per<=33, "Low", "Medium"))
cmip6AggMean$Risk.Level <- factor(cmip6AggMean$Risk.Level, levels=c("High", "Medium", "Low"))
cmip6AggMean$Uncert.Level <- factor(cmip6AggMean$Uncert.Level, levels=c("High", "Medium", "Low"))


ClimSummary <- merge(StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "Tmean.GLDAS", "Precip.GLDAS", "ET.GLDAS")], cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP2-4.5",c("ISOURBID", "tas.diff", "pr.per")], all.x=T)
names(ClimSummary)[names(ClimSummary) %in% c("tas.diff", "pr.per")] <- c("tas.diff.SSP245", "pr.per.SSP245")
ClimSummary <- merge(ClimSummary, cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP5-8.5",c("ISOURBID", "tas.diff", "pr.per")], all.x=T)
names(ClimSummary)[names(ClimSummary) %in% c("tas.diff", "pr.per")] <- c("tas.diff.SSP585", "pr.per.SSP585")
summary(ClimSummary)

ClimSummary$pr.SSP245 <- ClimSummary$Precip.GLDAS*ClimSummary$pr.per.SSP245
ClimSummary$pr.SSP585 <- ClimSummary$Precip.GLDAS*ClimSummary$pr.per.SSP585
ClimSummary$pr.diff.SSP245 <- ClimSummary$Precip.GLDAS - ClimSummary$pr.SSP245
summary(ClimSummary)
median(ClimSummary$Precip.GLDAS[ClimSummary$biomeCode=="Med"], na.rm=T)
median(ClimSummary$pr.diff.SSP245[ClimSummary$biomeCode=="Med"], na.rm=T)
1-median(ClimSummary$pr.per.SSP245[ClimSummary$biomeCode=="Med"], na.rm=T)

# Creating a supplemental table with the climate change stats for each biome
climCurrentMean <- aggregate(cbind(Tmean.GLDAS, Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN = mean)
climCurrentMean[,names(climCurrentMean)[!names(climCurrentMean) %in% c("biomeName", "biomeCode")]] <- round(climCurrentMean[,names(climCurrentMean)[!names(climCurrentMean) %in% c("biomeName", "biomeCode")]], 1)
climCurrentMean

climCurrentSD <- aggregate(cbind(Tmean.GLDAS, Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN = sd)
climCurrentSD[,names(climCurrentSD)[!names(climCurrentSD) %in% c("biomeName", "biomeCode")]] <- round(climCurrentSD[,names(climCurrentSD)[!names(climCurrentSD) %in% c("biomeName", "biomeCode")]], 1)
climCurrentSD

cmip6BiomeMean245 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP2-4.5",], FUN = mean)
cmip6BiomeMean245$tas.diff <- round(cmip6BiomeMean245$tas.diff, 1)
cmip6BiomeMean245$pr.per <- round((cmip6BiomeMean245$pr.per-1)*100, 0)

cmip6BiomeSD245 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP2-4.5",], FUN = sd)
cmip6BiomeSD245$tas.diff <- round(cmip6BiomeSD245$tas.diff, 1)
cmip6BiomeSD245$pr.per <- round((cmip6BiomeSD245$pr.per)*100, 0)

cmip6BiomeMean585 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP5-8.5",], FUN = mean)
cmip6BiomeMean585$tas.diff <- round(cmip6BiomeMean585$tas.diff, 1)
cmip6BiomeMean585$pr.per <- round((cmip6BiomeMean585$pr.per-1)*100, 0)

cmip6BiomeSD585 <- aggregate(cbind(tas.diff, pr.per) ~ biomeName + biomeCode, data=cmip6AggMean[cmip6AggMean$Time=="2100" & cmip6AggMean$Scenario=="SSP5-8.5",], FUN = sd)
cmip6BiomeSD585$tas.diff <- round(cmip6BiomeSD585$tas.diff, 1)
cmip6BiomeSD585$pr.per <- round((cmip6BiomeSD585$pr.per)*100, 0)

TableS2 <- data.frame(Biome=pasteMeanSD(climCurrentMean$biomeName, climCurrentMean$biomeCode),
                      Tmean.GLDAS = pasteMeanSD(climCurrentMean$Tmean.GLDAS, climCurrentSD$Tmean.GLDAS),
                      Precip.GLDAS = pasteMeanSD(climCurrentMean$Precip.GLDAS, climCurrentSD$Precip.GLDAS),
                      Tmean.diff.245 = pasteMeanSD(cmip6BiomeMean245$tas.diff, cmip6BiomeSD245$tas.diff),
                      Pr.diff.245 = pasteMeanSD(cmip6BiomeMean245$pr.per, cmip6BiomeSD245$pr.per),
                      Tmean.diff.585 = pasteMeanSD(cmip6BiomeMean585$tas.diff, cmip6BiomeSD585$tas.diff),
                      Pr.diff.585 = pasteMeanSD(cmip6BiomeMean585$pr.per, cmip6BiomeSD585$pr.per))
                      
TableS2                      
write.csv(TableS2, file.path(path.figsMS, "TableS2_ClimateStats_CMIP6.csv"), row.names=F)

# "#E64B35FF" "#4DBBD5FF" "#00A087FF" "#3C5488FF" "#F39B7FFF" "#8491B4FF"
# "#91D1C2FF" "#DC0000FF" "#7E6148FF" "#B09C85FF"
# # Not a huge difference with median, so lets just roll with mean
# cmip6AggMed <- aggregate(cbind(tas.diff, pr.diff, pr.per, modET, modET.Base, modET.diff, modET.Precip)~ISOURBID + LATITUDE + LONGITUDE + biomeName + biomeCode + Scenario + Time, data=cmip6, FUN=median, na.rm=T)
# cmip6AggMed$biomeName <- factor(cmip6AggMed$biomeName, levels=biome.order$biomeName)
# summary(cmip6AggMed)
StatsCombined$modET <- StatsCombined$modET.Base
# cmip6AggMean$modET <- cmip6AggMean$modET

# Need to add the percentage of scenarios with Risk or uncertain
etSummary <- rbind(StatsCombined[,c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET", "modET.MegaLiters", "modET.Precip", "ET.Precip.diff", "ET.Precip.diff.per" , "PrecipDiff.MegaLiters", "Risk.per", "Uncert.per")],
                   cmip6AggMean[cmip6AggMean$Time=="2100",c("ISOURBID", "LATITUDE", "LONGITUDE", "biomeName", "biomeCode", "Scenario", "modET", "modET.MegaLiters", "modET.Precip", "ET.Precip.diff", "ET.Precip.diff.per" , "PrecipDiff.MegaLiters", "Risk.per", "Uncert.per")])
etSummary$biomeName <- factor(etSummary$biomeName, levels=biome.order$biomeName)
etSummary$biomeCode <- factor(etSummary$biomeCode, levels=rev(biome.order$biomeCode))
etSummary$Scenario <- as.factor(etSummary$Scenario)
etSummary$Risk.Level <- ifelse(etSummary$Risk.per>=66, "High", ifelse(etSummary$Risk.per<=33, "Low", "Medium"))
etSummary$Uncert.Level <- ifelse(etSummary$Uncert.per>=66, "High", ifelse(etSummary$Uncert.per<=33, "Low", "Medium"))
etSummary$Risk.Level <- factor(etSummary$Risk.Level, levels=c("High", "Medium", "Low"))
etSummary$Uncert.Level <- factor(etSummary$Uncert.Level, levels=c("High", "Medium", "Low"))
summary(etSummary)

ggplot(data=etSummary) +
  facet_grid(Scenario~.) +
  geom_histogram(aes(x=Risk.per, fill=biomeCode)) +
  scale_fill_manual(values=biomeCode.pall.ShortCB)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 3.1 Main Figure ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Do analyses here!

map.ETratio.All <- ggplot(data=etSummary[,]) +
  facet_grid(Scenario~. ) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=log(modET.Precip)), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Precip.\nDeficit", colors=rev(grad.prcp), limits=c(-3, 3), n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
  theme(legend.position="left",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.width = unit(2, "lines"),
        legend.key.height = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 2, 0.5, "lines"))
map.ETratio.All

plotRatioLog <- ggplot(data=etSummary[!is.na(etSummary$biomeName),]) +
  facet_grid(Scenario~.) +
  # coord_flip() +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(y=biomeCode, x=log(modET.Precip), fill=biomeName), scale="width") +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate(geom="text", y=10, x=-3, label=c("Precip Surplus"), hjust=0, size=3) +
  annotate(geom="text", y=10, x=3, label=c("Precip Deficit"), hjust=0, size=3) +
  # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
  scale_fill_manual(values=biome.pall.ShortCB) +
  labs(x="Precipitation Deficit", y="Biome") +
  guides(fill="none") +
  theme_bw()
plotRatioLog

# png(file.path(path.figsMS, "Figure5_ET_vs_Precip_Now-CMIP6_Log_Combined.png"), height=8, width=14, units="in", res=320)
# cowplot::plot_grid(map.ETratio.All, plotRatioLog, ncol=2, rel_widths = c(0.55, 0.45), labels=c("A", "B"))
# dev.off()
# 
# pdf(file.path(path.MS, "Figure5_ET_vs_Precip_Now-CMIP6_Log_Combined.pdf"), height=8, width=14)
# cowplot::plot_grid(map.ETratio.All, plotRatioLog, ncol=2, rel_widths = c(0.55, 0.45), labels=c("A", "B"))
# dev.off()
# 




map.Risk <- ggplot(data=etSummary[!is.na(etSummary$Risk.Level),]) +
  facet_grid(Scenario~. ) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=Risk.Level), size=0.1, alpha=0.8) +
  scale_color_manual(name="Deficit Risk", values=c("High"="#bd0026", "Medium"="#fd8d3c", "Low"="#ffffb2")) +
  guides(color=guide_legend(override.aes=list(size=5))) +
  theme(legend.position="left",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.size = unit(2, "lines"),
        # legend.key.height = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 2, 0.5, "lines"))
# map.Risk

map.Uncert <- ggplot(data=etSummary[!is.na(etSummary$Uncert.Level),]) +
  facet_grid(Scenario~. , switch="y") +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=Uncert.Level), size=0.1, alpha=0.8) +
  scale_color_manual(name="Uncertain\nDeficit", values=c("High"="#810f7c", "Medium"="#8c96c6", "Low"="#edf8fb")) +
  guides(color=guide_legend(override.aes=list(size=5))) +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        legend.key.size = unit(2, "lines"),
        # legend.key.height = unit(2, "lines"),
        # legend.key.height = unit(1.5, "lines"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 2, 0.5, "lines"))
# map.Uncert

png(file.path(path.figsMS, "FigureED2_RiskUncertainty_Map.png"), height=8, width=14, units="in", res=320)
cowplot::plot_grid(map.Risk, map.Uncert, ncol=2, labels=c("A", "B"))
dev.off()

pdf(file.path(path.MS, "FigureED2_RiskUncertainty_Map.pdf"), height=8, width=14)
cowplot::plot_grid(map.Risk, map.Uncert, ncol=2, labels=c("A", "B"))
dev.off()

plotDeficit <- ggplot(data=etSummary[!is.na(etSummary$biomeName),]) +
  facet_grid(Scenario~.) +
  # coord_flip() +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(y=biomeCode, x=ET.Precip.diff, fill=biomeName), scale="width") +
  geom_vline(xintercept=0, linetype="dashed") +
  annotate(geom="text", y=10, x=-8, label=c("Precip Deficit"), hjust=0, size=3) +
  annotate(geom="text", y=10, x=18, label=c("Precip Surplus"), hjust=1, size=3) +
  # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
  scale_fill_manual(values=biome.pall.ShortCB) +
  labs(x="Precipitation Deficit (mm/Day)", y="Biome") +
  guides(fill="none") +
  theme_bw()
plotDeficit

png(file.path(path.figsMS, "Figure5_Risk-Deficit_Combined.png"), height=8, width=14, units="in", res=320)
cowplot::plot_grid(map.Risk, plotDeficit, ncol=2, rel_widths = c(0.55, 0.45), labels=c("A", "B"))
dev.off()

pdf(file.path(path.MS, "Figure5_Risk-Deficit_Combined.pdf"), height=8, width=14)
cowplot::plot_grid(map.Risk, plotDeficit, ncol=2, rel_widths = c(0.55, 0.45), labels=c("A", "B"))
dev.off()

# plotDeficitMegaL <- ggplot(data=etSummary[!is.na(etSummary$biomeName),]) +
#   facet_grid(Scenario~.) +
#   # coord_flip() +
#   # coord_cartesian(ylim=c(0,2.5), expand=0) +
#   geom_violin(aes(y=biomeCode, x=PrecipDiff.MegaLiters, fill=biomeName), scale="width") +
#   geom_vline(xintercept=0, linetype="dashed") +
#   # annotate(geom="text", y=10, x=-3, label=c("Precip Surplus"), hjust=0, size=3) +
#   # annotate(geom="text", y=10, x=3, label=c("Precip Deficit"), hjust=0, size=3) +
#   # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
#   scale_fill_manual(values=biome.pall.ShortCB) +
#   labs(x="Precipitation Deficit", y="Biome") +
#   guides(fill="none") +
#   theme_bw()
# plotDeficitMegaL
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 3.2 Supplemental tables & figures breaking down by biome ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
summary(etSummary)
summary(StatsCombined)

#-#-#-#-#-#-#-#-
# 3.2.1. Making a figure showing the spatial patterns of climate & its change ----
#-#-#-#-#-#-#-#-
tasGLDAS <- ggplot(data=StatsCombined[,]) +
  # facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=Tmean.GLDAS), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nTemp. (deg. C)", colors=gradTemp2, n.breaks=13, oob=squish) +
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
tasGLDAS

# prBreaks <- round(quantile(cmip6Agg$pr[cmip6Agg$Scenario=="historical"], seq(0.1, 0.9, by=0.1)), 1)
prGLDAS <- ggplot(data=StatsCombined[,]) +
  # facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=Precip.GLDAS), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=gradPrcp2, n.breaks=13, oob=squish) + 
  # scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=grad.prcp, limits=c(0,10), breaks=c(0.1, 1.5, 2.3, 2.7, 3.7, 4.8, 5.8, 7.1, 9.3), oob=squish) + # Using breaks from IPCC AR6 figures
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
prGLDAS

# figClimPresent <- cowplot:: plot_grid(tasGLDAS, prGLDAS, labels=c("A", "B"), nrow=1)

tasFuture <- ggplot(data=cmip6AggMean[cmip6AggMean$Time==2100,]) +
  facet_grid(.~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tas.diff), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Temp Change\n(deg. C)", colors=grad.temp, limits=c(-4.5, 4.5), n.breaks=13, oob=squish) +
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
tasFuture

prFuture <- ggplot(data=cmip6AggMean[cmip6AggMean$Time==2100,]) +
  facet_grid(.~Scenario) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=(pr.per-1)*100), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Precip Change\n (%)", colors=grad.prcp, limits=c(-0.8, 0.8)*100, n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
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
prFuture


png(file.path(path.figsMS, "FigureS5_Climate_GLDAS_CMIP6-EnsembleMeans.png"), height=6, width=14, units="in", res=320)
cowplot::plot_grid(tasGLDAS, prGLDAS, tasFuture, prFuture, ncol=2, rel_heights = c(0.45, 0.65), labels=c("A", "B", "C", "D"))
dev.off()

pdf(file.path(path.MS, "FigureS5_Climate_GLDAS_CMIP6-EnsembleMeans.pdf"), height=6, width=14)
cowplot::plot_grid(tasGLDAS, prGLDAS, tasFuture, prFuture, ncol=2, rel_heights = c(0.45, 0.65), labels=c("A", "B", "C", "D"))
dev.off()

#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#-
# 3.2.2. Summarizing the climate & ET change by biome ----
#-#-#-#-#-#-#-#-
# Adding a figure shoing chang in ET
mapETcurrent <- ggplot(data=StatsCombined[,]) +
  # facet_grid(Time~Scenario)+
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", linewidth=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=modET.Base), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="Mean Summer\nET (mm/day)", colors=gradPrcp2, n.breaks=13, oob=squish) + 
  # scale_color_stepsn(name="Mean Summer\nPrecip (mm/day)", colors=grad.prcp, limits=c(0,10), breaks=c(0.1, 1.5, 2.3, 2.7, 3.7, 4.8, 5.8, 7.1, 9.3), oob=squish) + # Using breaks from IPCC AR6 figures
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
mapETcurrent

mapETfuture <- ggplot(data=cmip6AggMean[cmip6AggMean$Time==2100,]) +
  facet_grid(.~Scenario) +
  geom_rect(xmin=min(world$long), xmax=max(world$long), ymin=min(world$lat), ymax=max(world$lat), fill="gray80") +
  geom_map(map=world, data=world, aes( map_id = region), fill="gray30", size=0.1) +
  # coord_map("merc") +
  coord_map("moll") +
  expand_limits(x = world$long, y = world$lat) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=(modET.perChange-1)*100), size=0.1, alpha=0.8) +
  scale_color_stepsn(name="ET Change\n (%)", colors=grad.prcp, limits=c(-0.8, 0.8)*100, n.breaks=13, oob=squish) + # Using breaks from IPCC AR6 figures
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
mapETfuture

png(file.path(path.figsMS, "FigureED1_ET-ETchange_current-CMIP6.png"), height=6, width=9, units="in", res=320)
cowplot::plot_grid(mapETcurrent, mapETfuture, ncol=1, rel_heights = c(0.45, 0.55), labels=c("A", "B"))
dev.off()

pdf(file.path(path.MS, "FigureED1_ET-ETchange_current-CMIP6.pdf"), height=6, width=9)
cowplot::plot_grid(mapETcurrent, mapETfuture, ncol=1, rel_heights = c(0.45, 0.55), labels=c("A", "B"))
dev.off()




violinTas <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=tas.diff, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.ShortCB) + 
  scale_fill_manual(name="Biome", values=biome.pall.ShortCB) + 
  coord_cartesian(ylim=c(0, max(cmip6AggMean$tas.diff))) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="Temperature Change (deg. C)") +
  theme_bw()
violinTas

violinPr <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=(pr.per-1)*100, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.ShortCB) + 
  scale_fill_manual(name="Biome", values=biome.pall.ShortCB) + 
  coord_cartesian(ylim=c(-1, 1.25)*100) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="Precip Change (%)") +
  theme_bw()
violinPr

violinET <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=modET, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.ShortCB) + 
  scale_fill_manual(name="Biome", values=biome.pall.ShortCB) + 
  # coord_cartesian(ylim=c(-1, 1.25)*100) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="ET (mm/day)") +
  theme_bw()
violinET

violinETper <- ggplot(data=cmip6AggMean) +
  facet_grid(.~Scenario) +
  geom_violin(aes(x=biomeCode, y=(modET.perChange-1)*100, fill=biomeName), scale="width") +
  geom_hline(yintercept=0) +
  scale_color_manual(name="Biome", values=biome.pall.ShortCB) + 
  scale_fill_manual(name="Biome", values=biome.pall.ShortCB) + 
  # coord_cartesian(ylim=c(-1, 1.25)*100) +
  guides(color="none", fill="none") +
  labs(x="Biome", y="ET Change (%)") +
  theme_bw()
violinETper

summary(cmip6AggMean)

png(file.path(path.figsMS, "FigureS6_Climate-ET-Change_Biomes.png"), height=8, width=8, units="in", res=320)
cowplot::plot_grid(violinTas, violinPr, violinETper, labels=c("A", "B", "C"), ncol=1)
dev.off()

pdf(file.path(path.MS, "FigureS6_Climate-ET-Change_Biomes.pdf"), height=8, width=8)
cowplot::plot_grid(violinTas, violinPr, violinETper, labels=c("A", "B", "C"), ncol=1)
dev.off()

names(StatsCombined)
# We need baseline N Cities; ET; % cities with water risk
etBiomeAggMean <- aggregate(modET ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)
etBiomeAggMean$ET.sd <- aggregate(modET ~ biomeName + biomeCode, data=StatsCombined, FUN=sd)$modET
etBiomeAggMean[,c("modET", "ET.sd")] <- round(etBiomeAggMean[,c("modET", "ET.sd")], 2)
etBiomeAggMean$timeRMSE <- aggregate(ETxValid.timeRMSE ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)$ETxValid.timeRMSE
etBiomeAggMean$timeRMSE.sd <- aggregate(ETxValid.timeRMSE ~ biomeName + biomeCode, data=StatsCombined, FUN=sd)$ETxValid.timeRMSE
etBiomeAggMean[,c("timeRMSE", "timeRMSE.sd")] <- round(etBiomeAggMean[,c("timeRMSE", "timeRMSE.sd")], 2)
etBiomeAggMean[,c("timeRMSE", "timeRMSE.sd")] <- round(etBiomeAggMean[,c("timeRMSE", "timeRMSE.sd")], 2)

etBiomeAggMean$N.Cities <- aggregate(modET ~ biomeName + biomeCode, data=StatsCombined, FUN=length)$modET
etBiomeAggMean$perCities.HighRisk <- round(aggregate(Risk.per ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)[,"Risk.per"], 0)
etBiomeAggMean$perCities.HighUncert <- round(aggregate(Uncert.per ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)[,"Uncert.per"], 0)
etBiomeAggMean

# Summarizing risk stats
riskAgg <- aggregate(Risk.Level  ~ biomeName + biomeCode + Scenario, data=etSummary[!is.na(etSummary$Risk.Level),], FUN=length)
names(riskAgg)[names(riskAgg)=="Risk.Level"] <- "N.Cities"

riskHigh <- aggregate(Risk.Level  ~ biomeName + biomeCode + Scenario, data=etSummary[etSummary$Risk.Level=="High",], FUN=length)
names(riskHigh)[names(riskHigh)=="Risk.Level"] <- "N.HighRisk"
riskHigh$deficit.mmDay <- aggregate(ET.Precip.diff  ~ biomeName + biomeCode + Scenario, data=etSummary[etSummary$Risk.Level=="High",], FUN=mean)$ET.Precip.diff
riskHigh$deficitSD.mmDay <- aggregate(ET.Precip.diff  ~ biomeName + biomeCode + Scenario, data=etSummary[etSummary$Risk.Level=="High",], FUN=sd)$ET.Precip.diff
riskHigh[,c("deficit.mmDay", "deficitSD.mmDay")] <- round(riskHigh[,c("deficit.mmDay", "deficitSD.mmDay")], 2)

riskHigh$deficit.ML <- aggregate(PrecipDiff.MegaLiters  ~ biomeName + biomeCode + Scenario, data=etSummary[etSummary$Risk.Level=="High",], FUN=mean)$PrecipDiff.MegaLiters
riskHigh$deficitSD.ML <- aggregate(PrecipDiff.MegaLiters  ~ biomeName + biomeCode + Scenario, data=etSummary[etSummary$Risk.Level=="High",], FUN=sd)$PrecipDiff.MegaLiters
riskHigh[,c("deficit.ML", "deficitSD.ML")] <- round(riskHigh[,c("deficit.ML", "deficitSD.ML")], 0)


uncertHigh <- aggregate(Uncert.Level  ~ biomeName + biomeCode + Scenario, data=etSummary[etSummary$Uncert.Level=="High" & !is.na(etSummary$Uncert.Level),], FUN=length)
names(uncertHigh)[names(uncertHigh)=="Uncert.Level"] <- "N.HighUncert"

riskAgg <- merge(riskAgg, riskHigh, all=T)
riskAgg <- merge(riskAgg, uncertHigh, all=T)
riskAgg$N.HighRisk[is.na(riskAgg$N.HighRisk)] <- 0
riskAgg$N.HighUncert[is.na(riskAgg$N.HighUncert)] <- 0
summary(riskAgg)

riskAgg[,c("Per.HighRisk", "Per.HighUncert")] <- round(riskAgg[,c("N.HighRisk", "N.HighUncert")]/riskAgg$N.Cities*100, 0)

# We'll need % change in ET; % cities drying; # cities with water risk; % cities with water risk
changeBiomeAggMedian <- aggregate(cbind(pr.per, modET, modET.perChange) ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=median)
changeBiomeAggMedian

changeBiomeAggMean <- aggregate(cbind(pr.per, modET, modET.perChange) ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=mean)
changeBiomeAggMean$modET <- round(changeBiomeAggMean$modET, 2)
changeBiomeAggMean[,c("pr.per", "modET.perChange")] <- round(changeBiomeAggMean[,c("pr.per", "modET.perChange")]-1,2)*100
changeBiomeAggMean$N.Cities.Dry <- aggregate(pr.per ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=function(x){length(which(x<1))})$pr.per
changeBiomeAggMean$N.Cities.Risk <- aggregate(modET.Precip ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=function(x){length(which(x>1))})$modET.Precip

changeBiomeAggSD <- aggregate(cbind(pr.per, modET, modET.perChange) ~ biomeName + biomeCode + Scenario, data=cmip6AggMean[cmip6AggMean$Time==2100,], FUN=sd)
changeBiomeAggSD$modET <- round(changeBiomeAggSD$modET, 2)
changeBiomeAggSD[,c("pr.per", "modET.perChange")] <- round(changeBiomeAggSD[,c("pr.per", "modET.perChange")],2)*100

etBiomeAggMean <- etBiomeAggMean[order(etBiomeAggMean$biomeName),]
changeBiomeAggMean <- changeBiomeAggMean[order(changeBiomeAggMean$biomeName),]
changeBiomeAggSD <- changeBiomeAggSD[order(changeBiomeAggSD$biomeName),]
riskAgg <- riskAgg[order(riskAgg$biomeName),]

TableED4 <- data.frame(Biome=pasteMeanSD(etBiomeAggMean$biomeName, etBiomeAggMean$biomeCode),
                      N.Cities = etBiomeAggMean$N.Cities,
                      ET.current = pasteMeanSD(etBiomeAggMean$modET, etBiomeAggMean$ET.sd),
                      ET.RMSEtime = pasteMeanSD(etBiomeAggMean$timeRMSE, etBiomeAggMean$timeRMSE.sd),
                      current.HighRisk = paste0(riskAgg$Per.HighRisk[riskAgg$Scenario=="Present"],"%"),
                      current.RiskDeficit = pasteMeanSD(riskAgg$deficit.mmDay[riskAgg$Scenario=="Present"],
                                                        riskAgg$deficitSD.mmDay[riskAgg$Scenario=="Present"]),
                      current.RiskDeficit.ML = pasteMeanSD(riskAgg$deficit.ML[riskAgg$Scenario=="Present"],
                                                        riskAgg$deficitSD.ML[riskAgg$Scenario=="Present"]),
                      current.HighUncert = paste0(riskAgg$Per.HighUncert[riskAgg$Scenario=="Present"],"%"),
                      SSP245.ETchange.per = pasteMeanSD(changeBiomeAggMean$modET.perChange[changeBiomeAggMean$Scenario=="SSP2-4.5"],
                                                        changeBiomeAggSD$modET.perChange[changeBiomeAggMean$Scenario=="SSP2-4.5"]),
                      SSP245.HighRisk = paste0(riskAgg$Per.HighRisk[riskAgg$Scenario=="SSP2-4.5"],"%"),
                      SSP245.RiskDeficit = pasteMeanSD(riskAgg$deficit.mmDay[riskAgg$Scenario=="SSP2-4.5"],
                                                        riskAgg$deficitSD.mmDay[riskAgg$Scenario=="SSP2-4.5"]),
                      SSP245.RiskDeficit.ML = pasteMeanSD(riskAgg$deficit.ML[riskAgg$Scenario=="SSP2-4.5"],
                                                       riskAgg$deficitSD.ML[riskAgg$Scenario=="SSP2-4.5"]),
                      SSP245.HighUncert = paste0(riskAgg$Per.HighUncert[riskAgg$Scenario=="SSP2-4.5"],"%"),
                      SSP585.ETchange.per = pasteMeanSD(changeBiomeAggMean$modET.perChange[changeBiomeAggMean$Scenario=="SSP5-8.5"],
                                                        changeBiomeAggSD$modET.perChange[changeBiomeAggMean$Scenario=="SSP5-8.5"]),
                      SSP585.HighRisk = paste0(riskAgg$Per.HighRisk[riskAgg$Scenario=="SSP5-8.5"],"%"),
                      SSP585.RiskDeficit = pasteMeanSD(riskAgg$deficit.mmDay[riskAgg$Scenario=="SSP5-8.5"],
                                                       riskAgg$deficitSD.mmDay[riskAgg$Scenario=="SSP5-8.5"]),
                      SSP585.RiskDeficit.ML = pasteMeanSD(riskAgg$deficit.ML[riskAgg$Scenario=="SSP5-8.5"],
                                                       riskAgg$deficitSD.ML[riskAgg$Scenario=="SSP5-8.5"]),
                      SSP585.HighUncert = paste0(riskAgg$Per.HighUncert[riskAgg$Scenario=="SSP5-8.5"],"%")
                      )
TableED4

write.csv(TableED4, file.path(path.figsMS, "TableED4_ET-Risk_CMIP6.csv"), row.names=F)

# Getting stats on percent changes in ET
changeBiomeAggMedian

bootET245 <- vector(length=1000)
bootET585 <- vector(length=1000)
bootETDiff <- vector(length=1000)
bootRisk245 <- vector(length=1000)
bootRisk585 <- vector(length=1000)
bootUncert245 <- vector(length=1000)
bootUncert585 <- vector(length=1000)

set.seed(1108)
ind245 <- which(cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5")
ind585 <- which(cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5")
for(i in 1:length(bootET245)){
  samp245 <- sample(ind245, length(ind245)/3*2)
  samp585 <- sample(ind585, length(ind585)/3*2)
  
  bootET245[i] <- median(cmip6AggMean$modET.perChange[samp245])
  bootET585[i] <- median(cmip6AggMean$modET.perChange[samp585])
  bootETDiff[i] <- median(cmip6AggMean$modET.perChange[samp585]) - median(cmip6AggMean$modET.perChange[samp245])
  
  bootRisk245[i] <- length(which(cmip6AggMean$Risk.Level[samp245]=="High"))/length(samp245)
  bootRisk585[i] <- length(which(cmip6AggMean$Risk.Level[samp585]=="High"))/length(samp585)
  bootUncert245[i] <- length(which(cmip6AggMean$Uncert.Level[samp245]=="High"))/length(samp245)
  bootUncert585[i] <- length(which(cmip6AggMean$Uncert.Level[samp585]=="High"))/length(samp585)
}
round(quantile(bootET245-1, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootET585-1, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootETDiff, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootRisk245, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootRisk585, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootUncert245, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootUncert585, c(0.5, 0.025, 0.975))*100, 0)

# Number of high-risk cities
length(which(etSummary$Scenario=="SSP2-4.5" & etSummary$Risk.Level=="High"))/length(which(etSummary$Scenario=="SSP2-4.5"))
length(which(etSummary$Scenario=="SSP5-8.5" & etSummary$Risk.Level=="High"))/length(which(etSummary$Scenario=="SSP5-8.5"))

length(which(etSummary$Scenario=="Present" & etSummary$Risk.Level=="High" & etSummary$biomeCode %in% c("TeBF", "TeCF", "TeGS")))/length(which(etSummary$Scenario=="Present" & etSummary$biomeCode %in% c("TeBF", "TeCF", "TeGS")))
length(which(etSummary$Scenario=="SSP5-8.5" & etSummary$Risk.Level=="High" & etSummary$biomeCode %in% c("TeBF", "TeCF", "TeGS")))/length(which(etSummary$Scenario=="SSP5-8.5" & etSummary$biomeCode %in% c("TeBF", "TeCF", "TeGS")))


bootDes245 <- vector(length=1000)
bootDes585 <- vector(length=1000)
bootTai245 <- vector(length=1000)
bootTai585 <- vector(length=1000)
set.seed(1219)
for(i in 1:length(bootTai585)){
  des245 <- sample(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5" & cmip6AggMean$biomeCode=="Des"], length(which(cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5" & cmip6AggMean$biomeCode=="Des"))/3*2)
  des585 <- sample(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5" & cmip6AggMean$biomeCode=="Des"], length(which(cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5" & cmip6AggMean$biomeCode=="Des"))/3*2)

  tai245 <- sample(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5" & cmip6AggMean$biomeCode=="Tai"], length(which(cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP2-4.5" & cmip6AggMean$biomeCode=="Tai"))/3*2)
  tai585 <- sample(cmip6AggMean$modET.perChange[cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5" & cmip6AggMean$biomeCode=="Tai"], length(which(cmip6AggMean$Time==2100 & cmip6AggMean$Scenario=="SSP5-8.5" & cmip6AggMean$biomeCode=="Tai"))/3*2)
  
  bootDes245[i] <- median(des245)
  bootDes585[i] <- median(des585)
  bootTai245[i] <- median(tai245)
  bootTai585[i] <- median(tai585)
}
round(quantile(bootDes245-1, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootDes585-1, c(0.5, 0.025, 0.975))*100, 0)

round(quantile(bootTai245-1, c(0.5, 0.025, 0.975))*100, 0)
round(quantile(bootTai585-1, c(0.5, 0.025, 0.975))*100, 0)

summary(etSummary[etSummary$modET.Precip<1 & etSummary$Scenario=="SSP5-8.5",])
summary(etSummary[etSummary$modET.Precip>1 & etSummary$Scenario=="SSP5-8.5",])


# Getting Stats on number of cities drying
sum(changeBiomeAggMean$N.Cities.Dry[changeBiomeAggMean$Scenario=="SSP2-4.5"])
sum(changeBiomeAggMean$N.Cities.Dry[changeBiomeAggMean$Scenario=="SSP5-8.5"])

# Getting Stats on number of cities with summer precip deficit
sum(changeBiomeAggMean$N.Cities.Risk[changeBiomeAggMean$Scenario=="SSP2-4.5"])/sum(etBiomeAggMean$N.Cities)
sum(changeBiomeAggMean$N.Cities.Risk[changeBiomeAggMean$Scenario=="SSP5-8.5"])/sum(etBiomeAggMean$N.Cities)

# Looking at change in % cities with risk
cbind(TableED4$Biome, as.numeric(gsub("%", "", TableED4$SSP245.CitiesRisk.per)) - as.numeric(gsub("%", "", TableED4$current.WaterRisk)))
cbind(TableED4$Biome, as.numeric(gsub("%", "", TableED4$SSP585.CitiesRisk.per)) - as.numeric(gsub("%", "", TableED4$current.WaterRisk)))


# Quanitfying the deficits
indNow <- which(etSummary$Scenario=="Present" & etSummary$Risk.Level=="High")
ind245 <- which(etSummary$Scenario=="SSP2-4.5" & etSummary$Risk.Level=="High")
ind545 <- which(etSummary$Scenario=="SSP5-8.5" & etSummary$Risk.Level=="High")
indTeCFnow <- which(etSummary$Scenario=="Present" & etSummary$biomeCode=="TeCF" & etSummary$Risk.Level=="High")

indTeBFnow <- which(etSummary$Scenario=="Present" & etSummary$biomeCode=="TeBF" & etSummary$Risk.Level=="High")
indTeBF245 <- which(etSummary$Scenario=="SSP2-4.5" & etSummary$biomeCode=="TeBF" & etSummary$Risk.Level=="High")
indTeBF585 <- which(etSummary$Scenario=="SSP5-8.5" & etSummary$biomeCode=="TeBF" & etSummary$Risk.Level=="High")

bootDefAllcurrent <- vector(length=1000)
bootDefAll245 <- vector(length=1000)
bootDefAll585 <- vector(length=1000)
bootDefTeCFcurrent <- vector(length=1000)
bootDefTeBFcurrent <- vector(length=1000)
bootDefTeBF245 <- vector(length=1000)
bootDefTeBF585 <- vector(length=1000)

set.seed(1647)
for(i in 1:length(bootDefAllcurrent)){
  sampCurrent <- sample(indNow, length(indNow)/3*2)
  samp245 <- sample(ind245, length(ind245)/3*2)
  samp458 <- sample(ind585, length(ind585)/3*2)
  sampTeCFnow <- sample(indTeCFnow, length(indTeCFnow)/3*2)
  sampTeBFnow <- sample(indTeBFnow, length(indTeBFnow)/3*2)
  sampTeBF245 <- sample(indTeBF245, length(indTeBF245)/3*2)
  sampTeBF585 <- sample(indTeBF585, length(indTeBF585)/3*2)
  
  bootDefAllcurrent[i] <- median(etSummary$PrecipDiff.MegaLiters[sampCurrent])
  bootDefTeCFcurrent[i] <- median(etSummary$PrecipDiff.MegaLiters[sampTeCFnow])
 
  bootDefTeBFcurrent[i] <-  mean(etSummary$PrecipDiff.MegaLiters[sampTeBFnow])
  bootDefTeBF245[i] <-  mean(etSummary$PrecipDiff.MegaLiters[sampTeBF245])
  bootDefTeBF585[i] <-  mean(etSummary$PrecipDiff.MegaLiters[sampTeBF585])
  
}
round(quantile(bootDefAllcurrent, c(0.5, 0.025, 0.975)), 0)
round(quantile(bootDefTeCFcurrent, c(0.5, 0.025, 0.975)), 0)

round(quantile(bootDefTeBFcurrent, c(0.5, 0.025, 0.975)), 0)
round(quantile(bootDefTeBF245, c(0.5, 0.025, 0.975)), 0)
round(quantile(bootDefTeBF585, c(0.5, 0.025, 0.975)), 0)

#-#-#-#-#-#-#-#-


#-#-#-#-#-#-#-#-
# Adding some figures that show variation by GCM
#-#-#-#-#-#-#-#-

ens245 <- ggplot(data=cmip6[!is.na(cmip6$biomeName) & cmip6$Time=="2100" & cmip6$Scenario=="SSP2-4.5",]) +
  facet_wrap(biomeName~.) +
  # coord_flip() +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(x=GCM, y=ET.Precip.diff, fill=biomeName), scale="width") +
  geom_hline(yintercept=0, linetype="dashed") +
  # annotate(geom="text", y=10, x=-3, label=c("Precip Surplus"), hjust=0, size=3) +
  # annotate(geom="text", y=10, x=3, label=c("Precip Deficit"), hjust=0, size=3) +
  # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
  scale_fill_manual(values=biome.pall.ShortCB) +
  labs(y="Precipitation Deficit (mm/day)", x="Ensemble Member") +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-90, hjust=0))

png(file.path(path.figsMS, "FigureS7_ET_vs_Precip_CMIP6_Ensemble_SSP2-4.5.png"), height=8, width=16, units="in", res=320)
print(ens245)
dev.off()

pdf(file.path(path.MS, "FigureS7 _ET_vs_Precip_CMIP6_Ensemble_SSP2-4.5.pdf"), height=8, width=16)
print(ens245)
dev.off()

ens585 <- ggplot(data=cmip6[!is.na(cmip6$biomeName) & cmip6$Time=="2100" & cmip6$Scenario=="SSP5-8.5",]) +
  facet_wrap(biomeName~.) +
  # coord_flip() +
  # coord_cartesian(ylim=c(0,2.5), expand=0) +
  geom_violin(aes(x=GCM, y=ET.Precip.diff, fill=biomeName), scale="width") +
  geom_hline(yintercept=0, linetype="dashed") +
  # annotate(geom="text", y=10, x=-3, label=c("Precip Surplus"), hjust=0, size=3) +
  # annotate(geom="text", y=10, x=3, label=c("Precip Deficit"), hjust=0, size=3) +
  # annotate(geom="text", x=14, y=5, l abel=c("Precip Deficit"), hjust=1) +
  scale_fill_manual(values=biome.pall.ShortCB) +
  labs(y="Precipitation Deficit (mm/day)", x="Ensemble Member") +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-90, hjust=0))

png(file.path(path.figsMS, "FigureS8_ET_vs_Precip_CMIP6_Ensemble_SSP5-8.5.png"), height=8, width=16, units="in", res=320)
print(ens585)
dev.off()

pdf(file.path(path.MS, "FigureS8_ET_vs_Precip_CMIP6_Ensemble_SSP5-8.5.pdf"), height=8, width=16)
print(ens585)
dev.off()
#-#-#-#-#-#-#-#-



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
