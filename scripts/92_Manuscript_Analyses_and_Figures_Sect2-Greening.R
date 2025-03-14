# Updated script for manuscript
# Outline
# 01. Trees cool cities & need water to do so: ---- 
#     Trees have a clear, consistent cooling potential on global urban surface temperatures… [treat cooling capacity as a known; we add a tiny bit of nuance; quickly bring in nuance & water]
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 03. Warming will increase water demand; precipitation will not keep pace in many regions ----
library(ggplot2); library(cowplot)

path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1")
path.cities <- file.path(path.google, "data_processed_final")

path.figsMS <- file.path(path.google, "figures_manuscript")
path.figsExplore <- file.path(path.google, "figures_exploratory")
dir.create(path.figsMS, recursive=T, showWarnings=F)

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

cityETStats <- read.csv(file.path(path.google, "city_stats_all_ET_scenarios.csv"))
summary(cityETStats)

# StatsCombined <- merge(StatsCombined, cityETStats[,c("ISOURBID", "cover.tree.TreeTargetBottomUp", "modET.Base", "modET.TreeTargetBottomUp")], all.x=T, all.y=F)
StatsCombined <- merge(StatsCombined, cityETStats[,c("ISOURBID", "cover.tree.TreeTargetBottom25", "cover.tree.TreeCityBottom25", "cover.tree.TreeCityBottom50", "modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50")], all.x=T, all.y=F)
summary(StatsCombined)




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# 02. Cities need more trees to offset UHIs; more trees means more water; ----
# 02. Key Results: (What Christy needs to get numbers for)
#  2.1. UHI greatest in regions where the “tree deficit” is greatest; Need XX% more tree cover to offset UHI
#       -- (SUPPLEMENT: Difference in tree cover; will link up with why our ET estimate lower than gldas)
#  2.2. Because the relationship between water use & tree cover is non-linear, how you accomplish the goal will impact the amount of water needed
#       2.2.1. Bottom-up approach: bringing things 50% below the biome target 
#              -- Results in X% more tree Canopy
#              -- Results in X˚ Cooling
#              -- Requires X amount more water
#       2.2.2. Uniform canopy water estimates; Reaching the same mean canopy level as should have the same effects on LST because we used linear relationship, but will require XX% more water
#       2.2.3. Benchmark against current precip
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#-#-#-#-#-#-#- 
# Do all necessary calculations up front
#-#-#-#-#-#-#-#- 
# Cooling contributed by current tree cover
# Breaking down contributions of tree cover to LST & UHI ----
StatsCombined$EffectLST.tree <- StatsCombined$LSTslope.tree*StatsCombined$value.tree.core # Cooling of trees in core
StatsCombined$EffectLST.other <- StatsCombined$LSTslope.veg*StatsCombined$value.other.core # coolign of other veg in core
StatsCombined$ContribUHI.tree <- StatsCombined$LSTslope.tree*StatsCombined$value.tree.diff # temp effect of tree diff w/ buffer
StatsCombined$ContribUHI.other <- StatsCombined$LSTslope.veg*StatsCombined$value.other.diff # temp effect of other veg diff w/ buffer
summary(StatsCombined)


# How much more trees would there need to be to offset UHI?
StatsCombined$TreeCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$LSTslope.tree # How much MORE tree cover you'd need
StatsCombined$OtherCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$LSTslope.veg # How much MORE other veg cover you'd need
StatsCombined$TreeCoverTargetUHI <- StatsCombined$TreeCoverUHINeed + StatsCombined$value.tree.core # Add MORE to CURRENT
StatsCombined$OtherCoverTargetUHI <- StatsCombined$OtherCoverUHINeed + StatsCombined$value.other.core # Add MORE to CURRENT
summary(StatsCombined)

# Calculating cooling trees added from bottom-up scenario and associated cooling
# cityETStats[,c("ISOURBID", "cover.tree.TreeTargetBottom25", "cover.tree.TreeCityBottom25", "cover.tree.TreeCityBottom50", "modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50")]
# Doing this for the biome Target
StatsCombined$greening.treeAddBiomeTarget <- StatsCombined$cover.tree.TreeTargetBottom25 - StatsCombined$value.tree.core
StatsCombined$greening.coolingAddBiomeTarget <- StatsCombined$greening.treeAddBiomeTarget*StatsCombined$LSTslope.tree
StatsCombined$greening.treeTotalBiomeTarget <- StatsCombined$cover.tree.TreeTargetBottom25 # Yeah, just renaming, but it will help below
StatsCombined$greening.coolingTotalBiomeTarget <- StatsCombined$cover.tree.TreeTargetBottom25*StatsCombined$LSTslope.tree
StatsCombined$greening.remainUHIBiomeTarget <- StatsCombined$value.LST.diff + StatsCombined$greening.coolingAddBiomeTarget
summary(StatsCombined)


# Doing this for the city 25 scenario
StatsCombined$greening.treeAddCity25 <- StatsCombined$cover.tree.TreeCityBottom25 - StatsCombined$value.tree.core
StatsCombined$greening.coolingAddCity25 <- StatsCombined$greening.treeAddCity25*StatsCombined$LSTslope.tree
StatsCombined$greening.treeTotalCity25 <- StatsCombined$cover.tree.TreeCityBottom25 # Yeah, just renaming, but it will help below
StatsCombined$greening.coolingTotalCity25 <- StatsCombined$cover.tree.TreeCityBottom25*StatsCombined$LSTslope.tree
StatsCombined$greening.remainUHICity25 <- StatsCombined$value.LST.diff + StatsCombined$greening.coolingAddCity25
summary(StatsCombined)
summary(StatsCombined[StatsCombined$value.LST.diff>0,c("value.tree.core", "value.LST.diff", "LSTslope.tree", names(StatsCombined)[grep("City25", names(StatsCombined))])])


# Doing this for the city 50 scenario
StatsCombined$greening.treeAddCity50 <- StatsCombined$cover.tree.TreeCityBottom50 - StatsCombined$value.tree.core
StatsCombined$greening.coolingAddCity50 <- StatsCombined$greening.treeAddCity50*StatsCombined$LSTslope.tree
StatsCombined$greening.treeTotalCity50 <- StatsCombined$cover.tree.TreeCityBottom50 # Yeah, just renaming, but it will help below
StatsCombined$greening.coolingTotalCity50 <- StatsCombined$cover.tree.TreeCityBottom50*StatsCombined$LSTslope.tree
StatsCombined$greening.remainUHICity50 <- StatsCombined$value.LST.diff + StatsCombined$greening.coolingAddCity50
summary(StatsCombined)




# Water Ratios
# "modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50"
StatsCombined$ETcurrent.Precip <- StatsCombined$modET.Base/StatsCombined$Precip.GLDAS
StatsCombined$ETgreenBiomeTarget.Precip <- StatsCombined$modET.TreeTargetBottom25/StatsCombined$Precip.GLDAS
StatsCombined$ETgreenCity25.Precip <- StatsCombined$modET.TreeCityBottom25/StatsCombined$Precip.GLDAS
StatsCombined$ETgreenCity50.Precip <- StatsCombined$modET.TreeCityBottom50/StatsCombined$Precip.GLDAS
summary(StatsCombined)

length(which(StatsCombined$ETcurrent.Precip<1 & !is.na(StatsCombined$ETcurrent.Precip)))
length(which(StatsCombined$ETgreenBiomeTarget.Precip<1 & !is.na(StatsCombined$ETgreenBiomeTarget.Precip)))
length(which(StatsCombined$ETgreenCity25.Precip<1 & !is.na(StatsCombined$ETgreenCity25.Precip)))
length(which(StatsCombined$ETgreenCity50.Precip<1 & !is.na(StatsCombined$ETgreenCity50.Precip)))
# Only ~ 220 cities lose their sustainability from greening!
#-#-#-#-#-#-#-#- 


# Comparing how much the greening does to get you to the UHI offset target; uses a biome-average


coverLong <- stack(StatsCombined[,c("value.tree.core", "cover.tree.TreeTargetBottom25", "cover.tree.TreeCityBottom25", "cover.tree.TreeCityBottom50")])
coverLong$biomeCode <- StatsCombined$biomeCode
coverLong$Scenario[grep("tree.core", coverLong$ind)] <- "Current"
coverLong$Scenario[grep("Target", coverLong$ind)] <- "Biome-25"
coverLong$Scenario[grep("CityBottom25", coverLong$ind)] <- "City-25"
coverLong$Scenario[grep("CityBottom50", coverLong$ind)] <- "City-50"
coverLong$Scenario <- factor(coverLong$Scenario, levels=c("Current", "Biome-25", "City-25", "City-50"))
summary(coverLong)

colorsScenario <- c("Current"="#005a32", "Biome UHI"="gray70", "Biome-25"="#b2df8a", "City-25"="#a6cee3", "City-50"="#1f78b4")

TreeCoverTarget <- ggplot(data=StatsCombined[,], aes(x=biomeCode, y=TreeCoverTargetUHI, fill="Biome UHI", color="Biome UHI")) +  
  geom_bar(stat="summary", fun="median") +
  geom_violin(data=coverLong[coverLong$Scenario!="City-50",], aes(x=biomeCode, y=values, fill=Scenario, color=Scenario), scale="width", alpha=0.9) +
  scale_fill_manual(name="Scenario", values=colorsScenario) +
  scale_color_manual(name="Scenario", values=colorsScenario) +
  labs(x="Biome", y="Tree Cover (%)") +
  scale_y_continuous(limits=c(0,65), expand=c(0,0)) +
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

png(file.path(path.figsExplore, "TreeCover_Targets.png"), height=6, width=6, units="in", res=320)
TreeCoverTarget
dev.off()


# Now looking at the cooling vs. UHI of the current and our scenario
# citiesUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.p<0.01)
summary(StatsCombined[,c("EffectLST.tree", "value.LST.diff", "greening.coolingTotalBiomeTarget", "greening.remainUHIBiomeTarget", "greening.coolingTotalCity25", "greening.remainUHICity25", "greening.coolingTotalCity50", "greening.remainUHICity50")])

# "greening.coolingTotalCity25", "greening.remainUHICity25", "greening.coolingTotalCity50", "greening.remainUHICity50"
effectsUHI <- stack(StatsCombined[,c("EffectLST.tree", "value.LST.diff")])
names(effectsUHI) <- c("Current", "effect")
effectsUHI$GreeningBiomeTarget <- stack(StatsCombined[,c("greening.coolingTotalBiomeTarget", "greening.remainUHIBiomeTarget")])[,1]
effectsUHI$GreeningCity25 <- stack(StatsCombined[,c("greening.coolingTotalCity25", "greening.remainUHICity25")])[,1]
# effectsUHI$GreeningCity50 <- stack(StatsCombined[,c("greening.coolingTotalCity50", "greening.remainUHICity50")])[,1]
effectsUHI$effect <- as.character(effectsUHI$effect)
effectsUHI$effect[grep("tree", effectsUHI$effect)] <- "Tree"

effectsUHI$effect[grep("LST", effectsUHI$effect)] <- "Remaining UHI"
effectsUHI$effect <- factor(effectsUHI$effect, levels=c("Tree", "Remaining UHI"))
summary(effectsUHI)

effectsUHI[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")] <- StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")]
summary(effectsUHI)

effectsUHI2 <- stack(effectsUHI[,c("Current", "GreeningBiomeTarget", "GreeningCity25")])
# names(effectsUHI2)[2] <- c("Scenario")
effectsUHI2$Scenario[grep("Current", effectsUHI2$ind)] <- "Current"
effectsUHI2$Scenario[grep("Target", effectsUHI2$ind)] <- "Biome-25"
effectsUHI2$Scenario[grep("City25", effectsUHI2$ind)] <- "City-25"
effectsUHI2$Scenario <- factor(effectsUHI2$Scenario, levels=c("Current", "Biome-25", "City-25"))

effectsUHI2[,c("effect", "ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")] <- effectsUHI[,c("effect", "ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")]
summary(effectsUHI2)


# "#67001f" "#b2182b" "#d6604d" "#f4a582" "#fbbdc7" "#d1e5f0" "#92c5de" "#4393c3" "#2166ac" "#053061"
# "#f03b20"    "#ef3b2c"
plotTempEffects <- ggplot(data=effectsUHI2, aes(x=Scenario, y=values, fill=effect)) + 
  facet_wrap(~biomeCode) +
  geom_bar(stat="summary", fun="mean", position="dodge") +
  stat_summary(fun = mean,
               fun.min=function(x){mean(x)-sd(x)},
               fun.max=function(x){mean(x)+sd(x)},
               geom="pointrange", position=position_dodge(0.9)) +
  # geom_bar(stat="summary", fun="mean") +
  # stat_summary(fun = mean, 
  #              fun.min=function(x){mean(x)-sd(x)}, 
  #              fun.max=function(x){mean(x)+sd(x)}, 
  #              geom="pointrange") +
  # stat_summary(fun.data = "mean_cl_normal", conf.int=0.95, geom="pointrange", position=position_dodge(0.9)) +
  geom_hline(yintercept=0, linewidth=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  scale_color_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  geom_text(x="Des", y=-7, hjust=1, label="Cooling Effect") +
  geom_text(x="Des", y=2.5, hjust=1, label="Warming Effect") +
  # coord_cartesian(ylim=c(-7.5, 2.5)) +
  labs(x="Biome", y="Temperature Effect (˚C)") +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face="bold"),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))

png(file.path(path.figsExplore, "TreeCover_Targets-Cooling.png"), height=6, width=6, units="in", res=320)
plotTempEffects
dev.off()


# Making a poster-ready figure where we're only looking at cities with UHI & showign teh Greening City 25 scenario
cityUHI <- StatsCombined$ISOURBID[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig]
length(cityUHI)


plotTempEffectsUHIonly <- ggplot(data=effectsUHI2[effectsUHI2$ISOURBID %in% cityUHI,], aes(x=Scenario, y=values, fill=effect)) + 
  facet_wrap(~biomeCode, ncol=2) +
  geom_bar(stat="summary", fun="mean") +
  stat_summary(fun = mean, 
               fun.min=function(x){mean(x)-sd(x)}, 
               fun.max=function(x){mean(x)+sd(x)}, 
               geom="pointrange") +
  # stat_summary(fun.data = "mean_cl_normal", conf.int=0.95, geom="pointrange", position=position_dodge(0.9)) +
  geom_hline(yintercept=0, linewidth=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  scale_color_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  labs(y="Temperature Effect (˚C)") +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face="bold"),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 2.5, 0.5, 0.5), "lines"))



png(file.path(path.figsMS, "Figure3_TreeCover_Targets-Cooling_UHIOnly.png"), height=6.5, width=3, units="in", res=320)
plotTempEffectsUHIonly
dev.off()





precip.means <- aggregate(Precip.GLDAS ~ biomeCode, data=StatsCombined, FUN=mean)

# Compare current vs. scenario with water use
plotPrecip <- ggplot(data=StatsCombined[,]) +
  geom_violin(aes(x=biomeCode, y=Precip.GLDAS, fill=biomeName), scale="width") +
  geom_errorbar(data=precip.means, aes(x=biomeCode, ymin=Precip.GLDAS, ymax=Precip.GLDAS), color="black", linewidth=1, width=1) +
  scale_fill_manual(values=biome.pall.all) +
  scale_y_continuous(limits=c(0, max(StatsCombined$Precip.GLDAS, na.rm=T)+1), expand=c(0,0)) +
  labs(y="GLDAS Precip\n(kg/m2/day)", x="Biome") +
  guides(fill="none") +
  theme_bw() + theme(plot.margin = unit(c(0.5,0.5,0.5,0.75), "lines")) 
plotPrecip

effectsET <- stack(StatsCombined[,c("modET.Base", "modET.TreeTargetBottom25", "modET.TreeCityBottom25")])
names(effectsET) <- c("ET", "Scenario")
effectsET$ETratio <- stack(StatsCombined[,c("ETcurrent.Precip", "ETgreenBiomeTarget.Precip", "ETgreenCity25.Precip")])[,1]
effectsET$Scenario <- as.character(effectsET$Scenario)
effectsET$Scenario[grep("Base", effectsET$Scenario)] <- "Current"
effectsET$Scenario[grep("Target", effectsET$Scenario)] <- "Biome-25"
effectsET$Scenario[grep("CityBottom25", effectsET$Scenario)] <- "City-25"
effectsET$Scenario <- factor(effectsET$Scenario, levels=c("Current", "Biome-25", "City-25"))

effectsET[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "Precip.GLDAS")] <- StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "Precip.GLDAS")]
summary(effectsET)

# "Current"="#005a32", "Biome Target"=grad.tree[4], "50% Greening"=grad.tree[6])


plotET <- ggplot(data=effectsET[,]) +
  geom_violin(aes(x=biomeCode, y=ET, fill=Scenario), scale="width") +
  geom_errorbar(data=precip.means, aes(x=biomeCode, ymin=Precip.GLDAS, ymax=Precip.GLDAS), color="black", linewidth=1, width=1) +
  # geom_errorbar(stat="summary", fun="mean", data=StatsCombined, aes(x=biomeCode, y=Precip.GLDAS)) +
  scale_fill_manual(values=colorsScenario) +
  # scale_color_manual(values=biomeCode.pall.all) +
  labs(y="ET (kg/m2/day)", x="Biome") +
  scale_y_continuous(limits=c(0, max(precip.means$Precip.GLDAS, na.rm=T)+0.5), expand=c(0,0)) +
  guides(color="none") +
  theme_bw() + theme(legend.position="top", plot.background = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,1.85), "lines"))
plotET

plotETratio <- ggplot(data=effectsET[,]) +
  geom_violin(aes(x=biomeCode, y=log(ETratio), fill=Scenario), scale="width") +
  scale_fill_manual(values=colorsScenario) +
  geom_hline(yintercept=log(1), linetype="dashed") +
  # annotate(geom="text", x=1, y=c(-2.5, 3), label=c("Precip Surplus", "Precip Deficit"), hjust=0) +
  labs(y="Precip. Deficit", x="Biome") +
  guides(fill="none") +
  theme_bw() + theme(plot.background = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,1.55), "lines"))

plotETratio

# plotETcomb <- cowplot::plot_grid(plotET,NA, plotETratio, NA, ncol=2, rel_widths=c(0.75, 0.25))

png(file.path(path.figsMS, "Figure4_TreeCover_Targets-ET.png"), height=6, width=6, units="in", res=320)
cowplot::plot_grid(plotPrecip, plotET,plotETratio, ncol=1, rel_heights=c(0.3, 0.4, 0.3), labels=c("A", "B", "C"))
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Effects of Greening on LST (UHI cities only) ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
length(cityUHI)
# cityUHI <- StatsCombined$ISOURBID[]
# dim(StatsUHI)
# summary(StatsUHI)
# names(StatsUHI)

length(which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig))
StatsCombined$TreeTarget.CurrentRatio <- StatsCombined$TreeCoverTargetUHI/StatsCombined$value.tree.core
mean(StatsCombined$TreeTarget.CurrentRatio[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig])
StatsCombined[,c("greening.coolPerAddBiomeTarget", "greening.coolPerAddCity25", "greening.coolPerAddCity50")] <- StatsCombined[,c("greening.coolingAddBiomeTarget", "greening.coolingAddCity25", "greening.coolingAddCity50")]/StatsCombined$value.LST.diff
summary(StatsCombined)

StatsUHI <- StatsCombined[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig,]

greenStatsBiomeUHI <- aggregate(Precip.GLDAS ~ biomeName + biomeCode, data=StatsUHI, FUN=length)
names(greenStatsBiomeUHI)[3] <- "N.Cities.UHI"
sum(greenStatsBiomeUHI$N.Cities.UHI)

greenStatsBiomeUHIMeans <- aggregate(cbind(TreeCoverTargetUHI, greening.treeAddBiomeTarget, greening.coolingAddBiomeTarget, greening.coolPerAddBiomeTarget, greening.treeAddCity25, greening.coolingAddCity25, greening.coolPerAddCity25) ~ biomeName + biomeCode, data=StatsUHI, FUN=mean)
greenStatsBiomeUHIMeans[,grep("treeAdd", names(greenStatsBiomeUHIMeans))] <- round(greenStatsBiomeUHIMeans[,grep("treeAdd", names(greenStatsBiomeUHIMeans))], 1)
greenStatsBiomeUHIMeans[,grep("coolingAdd", names(greenStatsBiomeUHIMeans))] <- round(greenStatsBiomeUHIMeans[,grep("coolingAdd", names(greenStatsBiomeUHIMeans))], 2)
greenStatsBiomeUHIMeans[,grep("coolPerAdd", names(greenStatsBiomeUHIMeans))] <- round(greenStatsBiomeUHIMeans[,grep("coolPerAdd", names(greenStatsBiomeUHIMeans))], 2)*100


greenStatsBiomeUHISDs <- aggregate(cbind(TreeCoverTargetUHI, greening.treeAddBiomeTarget, greening.coolingAddBiomeTarget, greening.coolPerAddBiomeTarget, greening.treeAddCity25, greening.coolingAddCity25, greening.coolPerAddCity25) ~ biomeName + biomeCode, data=StatsUHI, FUN=sd)
greenStatsBiomeUHISDs[,grep("treeAdd", names(greenStatsBiomeUHISDs))] <- round(greenStatsBiomeUHISDs[,grep("treeAdd", names(greenStatsBiomeUHISDs))], 1)
greenStatsBiomeUHISDs[,grep("coolingAdd", names(greenStatsBiomeUHISDs))] <- round(greenStatsBiomeUHISDs[,grep("coolingAdd", names(greenStatsBiomeUHISDs))], 2)
greenStatsBiomeUHISDs[,grep("coolPerAdd", names(greenStatsBiomeUHISDs))] <- round(greenStatsBiomeUHISDs[,grep("coolPerAdd", names(greenStatsBiomeUHISDs))], 2)*100

greenStatsBiomeUHIMeans


greenStatsBiomeUHI[,gsub("greening.", "", names(greenStatsBiomeUHIMeans)[!names(greenStatsBiomeUHIMeans) %in% names(greenStatsBiomeUHI)])] <- matrix(paste0(as.matrix(greenStatsBiomeUHIMeans[,!names(greenStatsBiomeUHIMeans) %in% names(greenStatsBiomeUHI)]), " (", as.matrix(greenStatsBiomeUHISDs[,!names(greenStatsBiomeUHISDs) %in% names(greenStatsBiomeUHI)]), ")"), length(which(!names(greenStatsBiomeUHIMeans) %in% names(greenStatsBiomeUHI))))
greenStatsBiomeUHI$TreeCoverTargetUHI <- round(greenStatsBiomeUHIMeans$TreeCoverTargetUHI, 1)
greenStatsBiomeUHI

write.csv(greenStatsBiomeUHI, file.path(path.figsMS, "tableS5_Biome_GreenLST_UHIonly.csv"), row.names=F)


summary(StatsCombined[,c("value.tree.core", "TreeCoverUHINeed", "greening.treeAddBiomeTarget", "greening.treeAddCity25")]) 

indUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig)
length(indUHI)

bootBiomeGreen <- vector(length=1000)
bootBiomeCool <- vector(length=1000)
bootBiomeCoolProp <- vector(length=1000)
set.seed(1643)
for(i in 1:length(bootBiomeGreen)){
  samp <- sample(indUHI, length(indUHI)/3*2)
  bootBiomeGreen[i] <- median(StatsCombined$greening.treeAddBiomeTarget[samp])
  bootBiomeCool[i] <- median(StatsCombined$greening.coolingAddBiomeTarget[samp])
  bootBiomeCoolProp[i] <- median(StatsCombined$greening.coolingAddBiomeTarget[samp]/StatsCombined$value.LST.diff[samp])
}
round(quantile(bootBiomeGreen, c(0.5, 0.025, 0.975)), 1)
round(quantile(bootBiomeCool, c(0.5, 0.025, 0.975)), 2)
round(quantile(bootBiomeCoolProp*100, c(0.5, 0.025, 0.975)), 0)


bootCityGreen <- vector(length=1000)
bootCityCool <- vector(length=1000)
bootCityCoolProp <- vector(length=1000)
set.seed(1643)
for(i in 1:length(bootCityGreen)){
  samp <- sample(indUHI, length(indUHI)/3*2)
  bootCityGreen[i] <- median(StatsCombined$greening.treeAddCity25[samp])
  bootCityCool[i] <- median(StatsCombined$greening.coolingAddCity25[samp])
  bootCityCoolProp[i] <- median(StatsCombined$greening.coolingAddCity25[samp]/StatsCombined$value.LST.diff[samp])
}
round(quantile(bootCityGreen, c(0.5, 0.025, 0.975)), 1)
round(quantile(bootCityCool, c(0.5, 0.025, 0.975)), 2)
round(quantile(bootCityCoolProp*100, c(0.5, 0.025, 0.975)), 1)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Effects of Greening on ET (All Cities) ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
StatsCombined[,c("ET.PerInc.BiomeTarget", "ET.PerInc.City25", "ET.PerInc.City50")] <- (StatsCombined[,c("modET.TreeTargetBottom25", "modET.TreeCityBottom25", "modET.TreeCityBottom50")]/StatsCombined$modET.Base-1)*100

greenStatsBiomeMeans <- aggregate(cbind(Precip.GLDAS, modET.Base, greening.treeAddBiomeTarget, ET.PerInc.BiomeTarget, greening.treeAddCity25, ET.PerInc.City25, greening.treeAddCity50, ET.PerInc.City50) ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)
greenStatsBiomeMeans[,names(greenStatsBiomeMeans)[!names(greenStatsBiomeMeans) %in% c("biomeName", "biomeCode")]] <- round(greenStatsBiomeMeans[,names(greenStatsBiomeMeans)[!names(greenStatsBiomeMeans) %in% c("biomeName", "biomeCode")]], 1)
greenStatsBiomeMeans

greenStatsBiomeSDs <- aggregate(cbind(Precip.GLDAS, modET.Base, greening.treeAddBiomeTarget, ET.PerInc.BiomeTarget, greening.treeAddCity25, ET.PerInc.City25, greening.treeAddCity50, ET.PerInc.City50) ~ biomeName + biomeCode, data=StatsCombined, FUN=sd)
greenStatsBiomeSDs[,names(greenStatsBiomeSDs)[!names(greenStatsBiomeSDs) %in% c("biomeName", "biomeCode")]] <- round(greenStatsBiomeSDs[,names(greenStatsBiomeSDs)[!names(greenStatsBiomeSDs) %in% c("biomeName", "biomeCode")]], 1)
greenStatsBiomeSDs

greenStatsBiomeN <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN=length)
names(greenStatsBiomeN)[names(greenStatsBiomeN)=="Precip.GLDAS"] <- "N.Total"

WaterRisk.Current <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETcurrent.Precip>1,], FUN=length)
names(WaterRisk.Current)[names(WaterRisk.Current)=="Precip.GLDAS"] <- "N.Risk.current"

WaterRisk.BiomeTarget <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETgreenBiomeTarget.Precip>1,], FUN=length)
names(WaterRisk.BiomeTarget)[names(WaterRisk.BiomeTarget)=="Precip.GLDAS"] <- "N.Risk.biomeTarget"


WaterRisk.City25 <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETgreenCity25.Precip>1,], FUN=length)
names(WaterRisk.City25)[names(WaterRisk.City25)=="Precip.GLDAS"] <- "N.Risk.City25"

greenStatsBiomeN <- merge(greenStatsBiomeN, WaterRisk.Current, all.x=T)
greenStatsBiomeN <- merge(greenStatsBiomeN, WaterRisk.BiomeTarget, all.x=T)
greenStatsBiomeN <- merge(greenStatsBiomeN, WaterRisk.City25, all.x=T)
greenStatsBiomeN[is.na(greenStatsBiomeN)] <- 0

TableS6 <- data.frame(Biome=pasteMeanSD(greenStatsBiomeN$biomeName, greenStatsBiomeN$biomeCode),
                      N.Cities = greenStatsBiomeN$N.Total,
                      Precip = pasteMeanSD(greenStatsBiomeMeans$Precip.GLDAS, greenStatsBiomeSDs$Precip.GLDAS),
                      current.WaterRisk = paste0(round(greenStatsBiomeN$N.Risk.current/greenStatsBiomeN$N.Total*100,0),"%"),
                      biomeTarget.TreeAdd = pasteMeanSD(greenStatsBiomeMeans$greening.treeAddBiomeTarget, greenStatsBiomeSDs$greening.treeAddBiomeTarget), 
                      biomeTarget.ETperInc=pasteMeanSD(greenStatsBiomeMeans$ET.PerInc.BiomeTarget, greenStatsBiomeSDs$ET.PerInc.BiomeTarget),
                      biomeTarget.WaterRisk = paste0(round(greenStatsBiomeN$N.Risk.biomeTarget/greenStatsBiomeN$N.Total*100, 0),"%"),
                      city25.TreeAdd = pasteMeanSD(greenStatsBiomeMeans$greening.treeAddCity25, greenStatsBiomeSDs$greening.treeAddCity25), 
                      city25.ETperInc=pasteMeanSD(greenStatsBiomeMeans$ET.PerInc.City25, greenStatsBiomeSDs$ET.PerInc.City25),
                      city25.WaterRisk = paste0(round(greenStatsBiomeN$N.Risk.City25/greenStatsBiomeN$N.Total*100, 0),"%"))
TableS6

write.csv(TableS6, file.path(path.figsMS, "TableS6_Biome_GreenET.csv"), row.names=F)


# Number of total current cities at water risk
length(which(StatsCombined$ETcurrent.Precip>1))
length(which(StatsCombined$ETcurrent.Precip>1))/nrow(StatsCombined)

length(which(StatsCombined$ETgreenBiomeTarget.Precip>1))
length(which(StatsCombined$ETgreenBiomeTarget.Precip>1))/nrow(StatsCombined)

length(which(StatsCombined$ETgreenCity25.Precip>1))
length(which(StatsCombined$ETgreenCity25.Precip>1))/nrow(StatsCombined)
length(which(StatsCombined$ETgreenCity25.Precip>1)) - length(which(StatsCombined$ETcurrent.Precip>1))

median(StatsCombined$ET.PerInc.BiomeTarget); median(StatsCombined$modET.TreeTargetBottom25 - StatsCombined$modET.Base)

median(StatsCombined$ET.PerInc.City25); median(StatsCombined$modET.TreeCityBottom25 - StatsCombined$modET.Base)
median(StatsCombined$ET.PerInc.City50); median(StatsCombined$modET.TreeCityBottom50 - StatsCombined$modET.Base)

length(which(StatsCombined$ETgreenBiomeTarget.Precip>1)) - length(which(StatsCombined$ETcurrent.Precip>1))
length(which(StatsCombined$ETgreenBiomeTarget.Precip>1))/nrow(StatsCombined)

summary(StatsCombined[StatsCombined$ETgreenBiomeTarget.Precip>1 & StatsCombined$ETcurrent.Precip<=1,"biomeName"])
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
