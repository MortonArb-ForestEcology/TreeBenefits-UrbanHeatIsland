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

path.MS <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Submission 5 - Nature Climate Change")

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
# effectsUHI$GreeningCity25 <- stack(StatsCombined[,c("greening.coolingTotalCity25", "greening.remainUHICity25")])[,1]
# effectsUHI$GreeningCity50 <- stack(StatsCombined[,c("greening.coolingTotalCity50", "greening.remainUHICity50")])[,1]
effectsUHI$effect <- as.character(effectsUHI$effect)
effectsUHI$effect[grep("tree", effectsUHI$effect)] <- "Tree"

effectsUHI$effect[grep("LST", effectsUHI$effect)] <- "Remaining UHI"
effectsUHI$effect <- factor(effectsUHI$effect, levels=c("Tree", "Remaining UHI"))
summary(effectsUHI)

effectsUHI[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")] <- StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev")]
summary(effectsUHI)

effectsUHI2 <- stack(effectsUHI[,c("Current", "GreeningBiomeTarget")])
# names(effectsUHI2)[2] <- c("Scenario")
effectsUHI2$Scenario[grep("Current", effectsUHI2$ind)] <- "Current"
effectsUHI2$Scenario[grep("Target", effectsUHI2$ind)] <- "Greening"
# effectsUHI2$Scenario[grep("City25", effectsUHI2$ind)] <- "City-25"
effectsUHI2$Scenario <- factor(effectsUHI2$Scenario, levels=c("Current", "Greening"))

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
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#1b9e77", "Non-Tree Veg"="#7570b3", "Remaining UHI"="#d95f02")) +
  scale_color_manual(name="Temp. Source", values=c("Tree"="#1b9e77", "Non-Tree Veg"="#7570b3", "Remaining UHI"="#d95f02")) +
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
cityUHI <- StatsCombined$ISOURBID[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig & StatsCombined$LSTslope.tree<0 & StatsCombined$LSTmodelFinal.tree.p<0.05]
length(cityUHI)


plotTempEffectsUHIonly <- ggplot(data=effectsUHI2[effectsUHI2$ISOURBID %in% cityUHI,], aes(x=Scenario, y=values, fill=effect)) + 
  facet_wrap(~biomeCode, ncol=2) +
  # coord_cartesian(ylim=c(-2.9, 2.9)) +
  geom_bar(stat="summary", fun="mean") +
  stat_summary(fun = mean, 
               fun.min=function(x){mean(x)-sd(x)}, 
               fun.max=function(x){mean(x)+sd(x)}, 
               geom="pointrange") +
  # stat_summary(fun.data = "mean_cl_normal", conf.int=0.95, geom="pointrange", position=position_dodge(0.9)) +
  geom_hline(yintercept=0, linewidth=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#1b9e77", "Non-Tree Veg"="#7570b3", "Remaining UHI"="#d95f02")) +
  scale_color_manual(name="Temp. Source", values=c("Tree"="#1b9e77", "Non-Tree Veg"="#7570b3", "Remaining UHI"="#d95f02")) +
  scale_y_continuous(breaks=c(-2, 0, 2)) +
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



png(file.path(path.figsMS, "Figure3_TreeCover_Targets-Cooling_UHIOnly.png"), height=5.5, width=3, units="in", res=320)
plotTempEffectsUHIonly
dev.off()

pdf(file.path(path.MS, "Figure3_TreeCover_Targets-Cooling_UHIOnly.pdf"), height=5.5, width=3)
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

effectsET <- stack(StatsCombined[,c("modET.Base", "modET.TreeTargetBottom25")])
names(effectsET) <- c("ET", "Scenario")
effectsET$ETratio <- stack(StatsCombined[,c("ETcurrent.Precip", "ETgreenBiomeTarget.Precip")])[,1]
effectsET$Scenario <- as.character(effectsET$Scenario)
effectsET$Scenario[grep("Base", effectsET$Scenario)] <- "Current"
effectsET$Scenario[grep("Target", effectsET$Scenario)] <- "Greening"
effectsET$Scenario <- factor(effectsET$Scenario, levels=c("Current", "Greening"))

effectsET[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "Precip.GLDAS")] <- StatsCombined[,c("ISOURBID", "biomeName", "biomeCode", "biomeCodeRev", "Precip.GLDAS")]
summary(effectsET)

# "Current"="#005a32", "Biome Target"=grad.tree[4], "50% Greening"=grad.tree[6])
colorsScenario2 <- c("Current"="#005a32", "Greening"="#b2df8a")


plotET <- ggplot(data=effectsET[,]) +
  geom_violin(aes(x=biomeCode, y=ET, fill=Scenario), scale="width") +
  geom_errorbar(data=precip.means, aes(x=biomeCode, ymin=Precip.GLDAS, ymax=Precip.GLDAS), color="black", linewidth=1, width=1) +
  # geom_errorbar(stat="summary", fun="mean", data=StatsCombined, aes(x=biomeCode, y=Precip.GLDAS)) +
  scale_fill_manual(values=colorsScenario2) +
  # scale_color_manual(values=biomeCode.pall.all) +
  labs(y="ET (kg/m2/day)", x="Biome") +
  scale_y_continuous(limits=c(0, max(precip.means$Precip.GLDAS, na.rm=T)+0.5), expand=c(0,0)) +
  guides(color="none") +
  theme_bw() + theme(legend.position="top", plot.background = element_blank(), plot.margin = unit(c(0.5,0.5,0.5,1.85), "lines"))
plotET

plotETratio <- ggplot(data=effectsET[,]) +
  geom_violin(aes(x=biomeCode, y=log(ETratio), fill=Scenario), scale="width") +
  scale_fill_manual(values=colorsScenario2) +
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

pdf(file.path(path.MS, "Figure4_TreeCover_Targets-ET.pdf"), height=6, width=6)
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
# Note: originally, could ignore the significance of tree cooling effects for this analysis, but no longer can
StatsUHIorig <- StatsCombined[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig,]
summary(StatsUHIorig)
dim(StatsUHIorig)

# To trim out weird values, need to look at only cities where trees have a cooling effect
StatsUHI <- StatsCombined[StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig & StatsCombined$LSTslope.tree<0 & StatsCombined$LSTmodelFinal.tree.p<0.05,]
summary(StatsUHI)
dim(StatsUHI)
nrow(StatsUHIorig) - nrow(StatsUHI)

# StatsUHIorig[StatsUHIorig$TreeTarget.CurrentRatio>100,]
# summary(StatsUHIorig[StatsUHIorig$TreeTarget.CurrentRatio<0,])





greenStatsBiomeUHI <- aggregate(Precip.GLDAS ~ biomeName + biomeCode, data=StatsUHI, FUN=length)
names(greenStatsBiomeUHI)[3] <- "N.Cities.UHI"
sum(greenStatsBiomeUHI$N.Cities.UHI)

# Add the ratio here!
greenStatsBiomeUHIMeans <- aggregate(cbind(TreeCoverTargetUHI, TreeTarget.CurrentRatio, greening.treeAddBiomeTarget, greening.coolingAddBiomeTarget, greening.coolPerAddBiomeTarget) ~ biomeName + biomeCode, data=StatsUHI, FUN=mean)
greenStatsBiomeUHIMeans$TreeTarget.CurrentRatio <- round(greenStatsBiomeUHIMeans$TreeTarget.CurrentRatio, 1)
greenStatsBiomeUHIMeans[,grep("treeAdd", names(greenStatsBiomeUHIMeans))] <- round(greenStatsBiomeUHIMeans[,grep("treeAdd", names(greenStatsBiomeUHIMeans))], 1)
greenStatsBiomeUHIMeans[,grep("coolingAdd", names(greenStatsBiomeUHIMeans))] <- round(greenStatsBiomeUHIMeans[,grep("coolingAdd", names(greenStatsBiomeUHIMeans))], 2)
greenStatsBiomeUHIMeans[,grep("coolPerAdd", names(greenStatsBiomeUHIMeans))] <- round(greenStatsBiomeUHIMeans[,grep("coolPerAdd", names(greenStatsBiomeUHIMeans))], 2)*100


greenStatsBiomeUHISDs <- aggregate(cbind(TreeCoverTargetUHI, TreeTarget.CurrentRatio, greening.treeAddBiomeTarget, greening.coolingAddBiomeTarget, greening.coolPerAddBiomeTarget) ~ biomeName + biomeCode, data=StatsUHI, FUN=sd)
greenStatsBiomeUHISDs$TreeTarget.CurrentRatio <- round(greenStatsBiomeUHISDs$TreeTarget.CurrentRatio, 1)

greenStatsBiomeUHISDs[,grep("treeAdd", names(greenStatsBiomeUHISDs))] <- round(greenStatsBiomeUHISDs[,grep("treeAdd", names(greenStatsBiomeUHISDs))], 1)
greenStatsBiomeUHISDs[,grep("coolingAdd", names(greenStatsBiomeUHISDs))] <- round(greenStatsBiomeUHISDs[,grep("coolingAdd", names(greenStatsBiomeUHISDs))], 2)
greenStatsBiomeUHISDs[,grep("coolPerAdd", names(greenStatsBiomeUHISDs))] <- round(greenStatsBiomeUHISDs[,grep("coolPerAdd", names(greenStatsBiomeUHISDs))], 2)*100

greenStatsBiomeUHIMeans

greenStatsBiomeUHI$TreeCoverTargetUHI <- round(greenStatsBiomeUHIMeans$TreeCoverTargetUHI, 1)
greenStatsBiomeUHI$TargetUHI.Ratio <- paste0(greenStatsBiomeUHIMeans$TreeTarget.CurrentRatio, " (", greenStatsBiomeUHISDs$TreeTarget.CurrentRatio, ")")
greenStatsBiomeUHI[,gsub("greening.", "", names(greenStatsBiomeUHIMeans)[!names(greenStatsBiomeUHIMeans) %in% names(greenStatsBiomeUHI)])] <- matrix(paste0(as.matrix(greenStatsBiomeUHIMeans[,!names(greenStatsBiomeUHIMeans) %in% names(greenStatsBiomeUHI)]), " (", as.matrix(greenStatsBiomeUHISDs[,!names(greenStatsBiomeUHISDs) %in% names(greenStatsBiomeUHI)]), ")"), length(which(!names(greenStatsBiomeUHIMeans) %in% names(greenStatsBiomeUHI))))
greenStatsBiomeUHI

write.csv(greenStatsBiomeUHI, file.path(path.figsMS, "tableS5_Biome_GreenLST_UHIonly.csv"), row.names=F)


summary(StatsCombined[,c("value.tree.core", "TreeCoverUHINeed", "greening.treeAddBiomeTarget")]) 

indUHI <- which(StatsCombined$value.LST.diff>0 & StatsCombined$value.LST.diff.sig  & StatsCombined$LSTslope.tree<0 & StatsCombined$LSTmodelFinal.tree.p<0.05)
length(indUHI)

bootTargetRatio <- vector(length=1000)
set.seed(1245)
for(i in 1:length(bootTargetRatio)){
  samp <- sample(indUHI, length(indUHI)/3*2)
  bootTargetRatio[i] <- median(StatsCombined$TreeTarget.CurrentRatio[samp])
}
round(quantile(bootTargetRatio, c(0.5, 0.025, 0.975)), 1) # % Ratio of the city-target vs. current cover



bootBiomeTreeNew <- vector(length=1000)
bootBiomeGreen <- vector(length=1000)
bootBiomeCool <- vector(length=1000)
bootBiomeCoolProp <- vector(length=1000)
set.seed(1643)
for(i in 1:length(bootBiomeGreen)){
  samp <- sample(indUHI, length(indUHI)/3*2)
  bootBiomeTreeNew[i] <- median(StatsCombined$greening.treeTotalBiomeTarget[samp])
  bootBiomeGreen[i] <- median(StatsCombined$greening.treeAddBiomeTarget[samp])
  bootBiomeCool[i] <- median(StatsCombined$greening.coolingAddBiomeTarget[samp])
  bootBiomeCoolProp[i] <- median(StatsCombined$greening.coolingAddBiomeTarget[samp]/StatsCombined$value.LST.diff[samp])
}
round(quantile(bootBiomeTreeNew, c(0.5, 0.025, 0.975)), 1) # % Tree Cover added with Biome-25 scenario
round(quantile(bootBiomeGreen, c(0.5, 0.025, 0.975)), 1) # % Tree Cover added with Biome-25 scenario
round(quantile(bootBiomeCool, c(0.5, 0.025, 0.975)), 2) # Cooling form the added trees
round(quantile(bootBiomeCoolProp*100, c(0.5, 0.025, 0.975)), 0) # Proportion of UHI offset



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Effects of Greening on ET (All Cities) ----
# NOTE NOTE: Add the magnitude of deficit where present
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
StatsCombined$ET.PerInc.BiomeTarget <- (StatsCombined$modET.TreeTargetBottom25/StatsCombined$modET.Base-1)*100

# Calculating the Precip deficit in mm/day
# StatsCombined$modET.Base/StatsCombined$Precip.GLDAS
StatsCombined$ET.Precip.diff <- StatsCombined$Precip.GLDAS - StatsCombined$modET.Base
StatsCombined$ET.Precip.diff.Biome25 <- StatsCombined$Precip.GLDAS - (StatsCombined$modET.Base*(1+StatsCombined$ET.PerInc.BiomeTarget*0.01))
summary(StatsCombined)

# Looking at the precip deficit as a percentage of precip
StatsCombined$ET.Precip.diffper <- StatsCombined$ET.Precip.diff/StatsCombined$Precip.GLDAS
StatsCombined$ET.Precip.diffper.Biome25 <- StatsCombined$ET.Precip.diff.Biome25/StatsCombined$Precip.GLDAS
summary(StatsCombined)
summary(StatsCombined$ET.Precip.diffper[StatsCombined$ET.Precip.diffper<0])
# hist(StatsCombined$ET.Precip.diffper[StatsCombined$ET.Precip.diffper<0])

# Checking for whether the deficit is within our margin of error based on the RMSE from the cross-validation
StatsCombined$DeficitError.RMSEall <- ifelse(abs(StatsCombined$ET.Precip.diff)<StatsCombined$ETmodel.RMSE, T, F) # Using the RMSE from my model fitting everying
StatsCombined$DeficitError.RMSExValid <- ifelse(abs(StatsCombined$ET.Precip.diff)<StatsCombined$ETxValid.spatRMSE.mean, T, F) # Using the RMSE from my model fitting everying
summary(StatsCombined)

StatsCombined$DeficitError.Biome25.RMSEall <- ifelse(abs(StatsCombined$ET.Precip.diff.Biome25)<StatsCombined$ETmodel.RMSE, T, F) # Using the RMSE from my model fitting everying
StatsCombined$DeficitError.Biome25.RMSExValid <- ifelse(abs(StatsCombined$ET.Precip.diff.Biome25)<StatsCombined$ETxValid.spatRMSE.mean, T, F) # Using the RMSE from my model fitting everying


#

# StatsCombined[StatsCombined$ETcurrent.Precip>1 & StatsCombined$ET.Precip.diff>0,]


greenStatsBiomeMeans <- aggregate(cbind(Precip.GLDAS, modET.Base, greening.treeAddBiomeTarget, ET.PerInc.BiomeTarget) ~ biomeName + biomeCode, data=StatsCombined, FUN=mean)
greenStatsBiomeMeans[,names(greenStatsBiomeMeans)[!names(greenStatsBiomeMeans) %in% c("biomeName", "biomeCode")]] <- round(greenStatsBiomeMeans[,names(greenStatsBiomeMeans)[!names(greenStatsBiomeMeans) %in% c("biomeName", "biomeCode")]], 1)
greenStatsBiomeMeans

greenStatsBiomeSDs <- aggregate(cbind(Precip.GLDAS, modET.Base, greening.treeAddBiomeTarget, ET.PerInc.BiomeTarget) ~ biomeName + biomeCode, data=StatsCombined, FUN=sd)
greenStatsBiomeSDs[,names(greenStatsBiomeSDs)[!names(greenStatsBiomeSDs) %in% c("biomeName", "biomeCode")]] <- round(greenStatsBiomeSDs[,names(greenStatsBiomeSDs)[!names(greenStatsBiomeSDs) %in% c("biomeName", "biomeCode")]], 1)
greenStatsBiomeSDs

greenStatsBiomeN <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined, FUN=length)
names(greenStatsBiomeN)[names(greenStatsBiomeN)=="Precip.GLDAS"] <- "N.Total"


summary(StatsCombined[StatsCombined$ETcurrent.Precip>1,])
# StatsCombined[StatsCombined$ETcurrent.Precip>1 & StatsCombined$ET.Precip.diff>0,]

WaterRisk.Current <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETcurrent.Precip>1 & !(StatsCombined$DeficitError.RMSExValid),], FUN=length)
names(WaterRisk.Current)[names(WaterRisk.Current)=="Precip.GLDAS"] <- "N.Deficit.current"

WaterUncertain.Current <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[(StatsCombined$DeficitError.RMSExValid),], FUN=length)
names(WaterUncertain.Current)[names(WaterUncertain.Current)=="Precip.GLDAS"] <- "N.Uncertain.current"

Deficit.Current <- aggregate(cbind(ETxValid.spatRMSE.mean, ET.Precip.diff) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETcurrent.Precip>1 & !(StatsCombined$DeficitError.RMSExValid),], FUN=mean)
names(Deficit.Current)[names(Deficit.Current) %in% c("ETxValid.spatRMSE.mean", "ET.Precip.diff")] <- c("RMSE.mmDay", "PrecipDeficit.mmDay")
Deficit.Current[,c("RMSE.mmDay", "PrecipDeficit.mmDay")] <- round(Deficit.Current[,c("RMSE.mmDay", "PrecipDeficit.mmDay")], 2)
# names(WaterRisk.Current)[names(WaterRisk.Current)=="Precip.GLDAS"] <- "N.Risk.current"
Deficit.Current <- merge(greenStatsBiomeN,Deficit.Current, all=T)
# Deficit.Current <- Deficit.Current[]
Deficit.Current


Deficit.CurrentSD <- aggregate(cbind(ETxValid.spatRMSE.mean, ET.Precip.diff) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETcurrent.Precip>1 & !(StatsCombined$DeficitError.RMSExValid),], FUN=sd)
names(Deficit.CurrentSD)[names(Deficit.CurrentSD) %in% c("ETxValid.spatRMSE.mean", "ET.Precip.diff")] <- c("RMSE.mmDay", "PrecipDeficit.mmDay")
Deficit.CurrentSD[,c("RMSE.mmDay", "PrecipDeficit.mmDay")] <- round(Deficit.CurrentSD[,c("RMSE.mmDay", "PrecipDeficit.mmDay")], 2)
Deficit.CurrentSD <- merge(greenStatsBiomeN, Deficit.CurrentSD, all=T)
Deficit.CurrentSD

# WaterStats.Current <- merge(WaterStats.Current, greenStatsBiomeN)

WaterRisk.Biome25 <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETgreenBiomeTarget.Precip>1 & !(StatsCombined$DeficitError.Biome25.RMSExValid),], FUN=length)
names(WaterRisk.Biome25)[names(WaterRisk.Biome25)=="Precip.GLDAS"] <- "N.Deficit.Greening"

WaterUncertain.Biome25 <- aggregate(cbind(Precip.GLDAS) ~ biomeName + biomeCode, data=StatsCombined[(StatsCombined$DeficitError.Biome25.RMSExValid),], FUN=length)
names(WaterUncertain.Biome25)[names(WaterUncertain.Biome25)=="Precip.GLDAS"] <- "N.Uncertain.Greening"

Deficit.Greening <- aggregate(cbind(ET.Precip.diff.Biome25) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETgreenBiomeTarget.Precip>1 & !(StatsCombined$DeficitError.Biome25.RMSExValid),], FUN=mean)
names(Deficit.Greening)[names(Deficit.Greening) == "ET.Precip.diff.Biome25"] <- c("PrecipDeficit.mmDay")
Deficit.Greening[,c("PrecipDeficit.mmDay")] <- round(Deficit.Greening[,c("PrecipDeficit.mmDay")], 2)
Deficit.Greening <- merge(greenStatsBiomeN, Deficit.Greening, all=T)
Deficit.Greening

Deficit.GreeningSD <- aggregate(cbind(ET.Precip.diff.Biome25) ~ biomeName + biomeCode, data=StatsCombined[StatsCombined$ETgreenBiomeTarget.Precip>1 & !(StatsCombined$DeficitError.Biome25.RMSExValid),], FUN=sd)
names(Deficit.GreeningSD)[names(Deficit.GreeningSD) == "ET.Precip.diff.Biome25"] <- c("PrecipDeficit.mmDay")
Deficit.GreeningSD[,c("PrecipDeficit.mmDay")] <- round(Deficit.GreeningSD[,c("PrecipDeficit.mmDay")], 2)
Deficit.GreeningSD <- merge(greenStatsBiomeN, Deficit.GreeningSD, all=T)
Deficit.GreeningSD


WaterStats.Current <- merge(WaterRisk.Current, WaterUncertain.Current, all=T)
WaterStats.Greening <- merge(WaterRisk.Biome25, WaterUncertain.Biome25, all=T)

greenStatsBiomeN <- merge(greenStatsBiomeN, WaterStats.Current, all.x=T)
greenStatsBiomeN <- merge(greenStatsBiomeN, WaterStats.Greening, all.x=T)
greenStatsBiomeN[is.na(greenStatsBiomeN)] <- 0

TableS6 <- data.frame(Biome=pasteMeanSD(greenStatsBiomeN$biomeName, greenStatsBiomeN$biomeCode),
                      N.Cities = greenStatsBiomeN$N.Total,
                      Precip = pasteMeanSD(greenStatsBiomeMeans$Precip.GLDAS, greenStatsBiomeSDs$Precip.GLDAS),
                      current.WaterRisk = paste0(greenStatsBiomeN$N.Deficit.current, " (", round(greenStatsBiomeN$N.Deficit.current/greenStatsBiomeN$N.Total*100,0),"%)"),
                      current.Deficit = pasteMeanSD(Deficit.Current$PrecipDeficit.mmDay, Deficit.CurrentSD$PrecipDeficit.mmDay),
                      current.Uncertain = paste0(greenStatsBiomeN$N.Uncertain.current, " (", round(greenStatsBiomeN$N.Uncertain.current/greenStatsBiomeN$N.Total*100,0),"%)"),
                      # Now doing the greening scenario stuff
                      biomeTarget.TreeAdd = pasteMeanSD(greenStatsBiomeMeans$greening.treeAddBiomeTarget, greenStatsBiomeSDs$greening.treeAddBiomeTarget), 
                      biomeTarget.ETperInc=pasteMeanSD(greenStatsBiomeMeans$ET.PerInc.BiomeTarget, greenStatsBiomeSDs$ET.PerInc.BiomeTarget),
                      greening.WaterRisk = paste0(greenStatsBiomeN$N.Deficit.Greening, " (", round(greenStatsBiomeN$N.Deficit.Greening/greenStatsBiomeN$N.Total*100, 0),"%)"),
                      greening.Deficit = pasteMeanSD(Deficit.Greening$PrecipDeficit.mmDay, Deficit.GreeningSD$PrecipDeficit.mmDay),
                      greening.Uncertain = paste0(greenStatsBiomeN$N.Uncertain.Greening, " (", round(greenStatsBiomeN$N.Uncertain.Greening/greenStatsBiomeN$N.Total*100,0),"%)")
                      
                      )
TableS6

write.csv(TableS6, file.path(path.figsMS, "TableS6_Biome_GreenET.csv"), row.names=F)


# Number of total current cities at water risk
Looking at ratios with current ET ----
  length(which(StatsCombined$ETcurrent.Precip>1)) # Number with on average precip deficit
length(which(StatsCombined$ETcurrent.Precip>1 & !(StatsCombined$DeficitError.RMSExValid))) # Number of cities outside the margin of error
length(which(StatsCombined$ETcurrent.Precip>1 & !(StatsCombined$DeficitError.RMSExValid)))/(nrow(StatsCombined)) # Precentage

length(which(StatsCombined$DeficitError.RMSExValid)) # Cities within the margin of error
length(which(StatsCombined$DeficitError.RMSExValid))/nrow(StatsCombined) # Cities within the margin of error

length(which(StatsCombined$ETcurrent.Precip<=1 & !(StatsCombined$DeficitError.RMSExValid))) # Number of cities outside the margin of error


# Looking at ratios with Biome-25 greening ----
length(which(StatsCombined$ETgreenBiomeTarget.Precip>1)) # Number with on average precip deficit
length(which(StatsCombined$ETgreenBiomeTarget.Precip>1 & !(StatsCombined$DeficitError.Biome25.RMSExValid))) # Number of cities outside the margin of error
length(which(StatsCombined$ETgreenBiomeTarget.Precip>1 & !(StatsCombined$DeficitError.Biome25.RMSExValid)))/(nrow(StatsCombined)) # Precentage

length(which(StatsCombined$ETgreenBiomeTarget.Precip>1 & !(StatsCombined$DeficitError.Biome25.RMSExValid))) - length(which(StatsCombined$ETcurrent.Precip>1 & !(StatsCombined$DeficitError.RMSExValid)))


length(which(StatsCombined$DeficitError.Biome25.RMSExValid)) # Cities within the margin of error
length(which(StatsCombined$DeficitError.Biome25.RMSExValid))/nrow(StatsCombined) # Cities within the margin of error
length(which(StatsCombined$DeficitError.Biome25.RMSExValid)) - length(which(StatsCombined$DeficitError.RMSExValid))

length(which(StatsCombined$ETgreenBiomeTarget.Precip<=1 & !(StatsCombined$DeficitError.Biome25.RMSExValid))) # Number of cities outside the margin of error



median(StatsCombined$ET.PerInc.BiomeTarget); median(StatsCombined$modET.TreeTargetBottom25 - StatsCombined$modET.Base)


length(which(StatsCombined$ETgreenBiomeTarget.Precip>1)) - length(which(StatsCombined$ETcurrent.Precip>1))
length(which(StatsCombined$ETgreenBiomeTarget.Precip>1))/nrow(StatsCombined)

summary(StatsCombined[StatsCombined$ETgreenBiomeTarget.Precip>1 & StatsCombined$ETcurrent.Precip<=1,"biomeName"])
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
