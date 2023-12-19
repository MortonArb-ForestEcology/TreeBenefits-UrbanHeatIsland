# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
# user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
path.cities <- file.path(path.google)

file.cityAll.stats <- file.path(path.cities, "city_stats_all.csv")
# summary(file.cityAll.stats)

# Adding info for the previous version so we can double check
# path.google2 <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v2")
# path.cities2 <- file.path(path.google2)
# file.cityAll.stats2 <- file.path(path.cities2, "city_stats_all.csv")


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
cityAll.stats <- read.csv(file.cityAll.stats)
# summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),9:25])
summary(cityAll.stats)

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

biome.hist <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_bar(aes(x=biomeName, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(name="Biome") +
  guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle=-30, hjust=0),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"), 
        plot.margin = margin(1, 2, 0.5, 1, "lines"))

biome.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=biomeName), size=0.5) +
  scale_color_manual(name="biome", values=biome.pall.all) +
  # scale_shape_manual(name="biome", values=1:length(biome.pall.all)) +
  guides(color="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin = margin(0.5, 2, 1, 3, "lines"))

# biome.map
png(file.path(path.figs, "CityDistribution_Biomes.png"), height=8, width=8, units="in", res=220)
plot_grid(biome.map, biome.hist, ncol=1, rel_heights = c(0.45, 0.55))
dev.off()
# ##########################################


# ##########################################
# Exploratory of raw output ----
# ##########################################

# ##########################
# Start with looking at some patterns of overall model fit (R2adj) ----
# ##########################
summary(cityAll.stats$LSTmodel.R2adj) # Have 3 cities missing compared to previous version
hist(cityAll.stats$LSTmodel.R2adj)

# don't save it, but do a quick map of model performance
# # The models with weird tree slopes are not the ones with low R2
# ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),])+
#   geom_point(aes(x=LSTmodel.R2adj, y=LSTmodel.tree.slope, color=biomeName)) +
#   scale_color_manual(name="biome", values=biome.pall.all) +
#   theme_bw()

mod.r2.biome <- lm(LSTmodel.R2adj ~ biomeName-1, data=cityAll.stats)
anova(mod.r2.biome)
summary(mod.r2.biome)

r2.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=LSTmodel.R2adj), size=0.5) +
  # scale_color_manual(name="Tree Effect\n(deg. C / % cover)", values=colors.cut) +
  # scale_color_gradient2(name="Tree Effect\n(deg. C / % cover)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

png(file.path(path.figs, "ModelFit_R2adj_Map.png"), height=6, width=12, units="in", res=220)
r2.map
dev.off()

# This looks different
r2.histo.biome <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),])+
  geom_histogram(aes(x=LSTmodel.R2adj, fill=biomeName)) +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(size=rel(0.8), color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())


png(file.path(path.figs, "ModelFit_R2adj_histogram.png"), height=8, width=8, units="in", res=220)
plot_grid(r2.map, r2.histo.biome, ncol=1, rel_heights = c(0.6, 0.4))
dev.off()

# ##########################

hist(cityAll.stats$LSTmodel.elev.slope)
summary(cityAll.stats$LSTmodel.elev.slope)
nrow(cityAll.stats)
# ##########################
# Now looking at the model slopes for trees ----
# ##########################
summary(cityAll.stats$LSTmodel.tree.slope)
cityAll.stats$tree.slope.cut <- cut(cityAll.stats$LSTmodel.tree.slope, breaks=c(-Inf, -1, -0.5, -0.25, -0.1, -0.05, -0.025, 0, 0.025, 0.5, Inf))
summary(cityAll.stats$tree.slope.cut)
colors.cut <- c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff", "#fee0d2", "#fc9272", "#de2d26")


cooling.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.slope.cut), size=0.5) +
  scale_color_manual(name="Tree Effect\n(deg. C / % cover)", values=colors.cut) +
  # scale_color_gradient2(name="Tree Effect\n(deg. C / % cover)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

TreeCoolingLat <- ggplot(data=cityAll.stats,) +
  coord_flip(xlim=c(-65,80)) +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(LSTmodel.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  stat_smooth(aes(x=LATITUDE, y=LSTmodel.tree.slope), color="black") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(y="Tree Effect\n(deg C / % cover)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        plot.margin=margin(8,1, 1.5, 0.5, "lines"))



png(file.path(path.figs, "TreeCooling_PercentEffect_LatitudeCombo.png"), height=6, width=12, units="in", res=220)
plot_grid(cooling.map, TreeCoolingLat, nrow=1, rel_widths = c(0.8, 0.2))
dev.off()

ggplot(data=cityAll.stats,) +
  coord_flip(xlim=c(-65,80)) +
  # coord_cartesian(, ylim=c(-65,80))
  geom_histogram(aes(x=LATITUDE)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  # stat_smooth(aes(x=LATITUDE, y=LSTmodel.tree.slope), color="black") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(y="Number Cities", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        plot.margin=margin(8,1, 1.5, 0.5, "lines"))


# TreeSlope.breaks <- c(-1, -0.5, -0.25, -0.1, -0.05, -0.025, 0, 0.025, 0.5)
# TreeCoolingBiomeHist <- 
ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  # geom_histogram(aes(x=LSTmodel.tree.slope, fill=biomeName)) +
  geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  # scale_x_continuous(breaks=TreeSlope.breaks)+
  labs(x="Tree Effect (deg. C / % cover)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

cityAll.stats$LSTmodel.tree.slope2 <- cityAll.stats$LSTmodel.tree.slope
cityAll.stats$LSTmodel.tree.slope2[cityAll.stats$LSTmodel.tree.slope2 < -1] <- -1
cityAll.stats$LSTmodel.tree.slope2[cityAll.stats$LSTmodel.tree.slope2 >0.25] <- 0.25

cityAll.stats$LSTmodel.veg.slope2 <- cityAll.stats$LSTmodel.veg.slope
cityAll.stats$LSTmodel.veg.slope2[cityAll.stats$LSTmodel.veg.slope2 < -1] <- -1
cityAll.stats$LSTmodel.veg.slope2[cityAll.stats$LSTmodel.veg.slope2 >0.25] <- 0.25

TreeEffectBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=LSTmodel.tree.slope2, fill=biomeName), breaks=seq(-1.1,0.3, by=0.05)) +
  geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  # geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Tree Effect (deg. C / % cover)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

VegEffectBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=LSTmodel.veg.slope2, fill=biomeName), breaks=seq(-1.1,0.3, by=0.05)) +
  geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  # geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Veg Effect (deg. C / % cover)") +
  guides(fill="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

png(file.path(path.figs, "TreeEffect_Histogram_Biome.png"), height=6, width=10, units="in", res=220)
TreeEffectBiomeHisto
dev.off()

png(file.path(path.figs, "TreeVegEffect_Histogram_Biome.png"), height=8, width=10, units="in", res=220)
plot_grid(TreeEffectBiomeHisto, VegEffectBiomeHisto, ncol=1, rel_heights = c(0.6, 0.4))
dev.off()


ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),]) +
  facet_wrap(~biomeName, scales="free", ncol=3) +
  geom_density(aes(x=LSTmodel.elev.slope, fill="Elev Effect"), adjust=2, alpha=0.5) +
  geom_density(aes(x=LSTmodel.veg.slope, fill="Other Veg Effect"), adjust=2, alpha=0.5) +
  geom_density(aes(x=LSTmodel.tree.slope, fill="Tree Effect"), adjust=2, alpha=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  scale_fill_manual(values=c("Tree Effect" = "darkgreen", "Other Veg Effect"="darkgoldenrod", "Elev Effect" = "gray20")) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="LST Effect: deg. C per (% cover or meter)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),]) +
  coord_cartesian(xlim=c(-1.0, 0.5)) +
  # facet_wrap(~biomeName, scales="free", ncol=3) +
  # geom_density(aes(x=LSTmodel.elev.slope, fill="Elev Effect"), adjust=2, alpha=0.5) +
  geom_density(aes(x=LSTmodel.veg.slope, fill="Other Veg Effect"), adjust=2, alpha=0.5) +
  geom_density(aes(x=LSTmodel.tree.slope, fill="Tree Effect"), adjust=2, alpha=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  scale_fill_manual(values=c("Tree Effect" = "darkgreen", "Other Veg Effect"="darkgoldenrod", "Elev Effect" = "gray20")) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Model Effects: deg. C per (% cover or meter)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Quick summary of places with significant tree cover trends -- most places (so far) have been INCREASING in cover, but also still getting warmer on average; how much would these cities have warmed without their trees??  
summary(cityAll.stats[!is.na(cityAll.stats$trend.tree.slope) & cityAll.stats$trend.tree.p<0.01,])
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.p<0.01,])
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.p>0.01,])
nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.p<0.01,]);  # 6 fewer than before
nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.p>0.01,]) # 3 more than before

nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope<0,]); # 2 fewer
nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope>0,]) # 1 fewer

nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope<0 & cityAll.stats$LSTmodel.tree.p<0.01,]); # 4 fewer
nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope<0 & cityAll.stats$LSTmodel.tree.p<0.01,])/nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope),]) # very slightly lower percent


# Cities with significant warming effect of trees
summary(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope>0.0,])

nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope>0 & cityAll.stats$LSTmodel.tree.p<0.01,])
nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope>0 & cityAll.stats$LSTmodel.tree.p<0.01,])/nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope),])

# There are a couple in the US --> lets find those 
cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope>0 & cityAll.stats$LSTmodel.tree.p<0.01 & cityAll.stats$ISO3=="USA",]
cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope>0.5 & cityAll.stats$LSTmodel.tree.p<0.01 ,]
cityAll.stats[cityAll.stats$ISO3=="USA" & cityAll.stats$NAME=="Chicago",]

cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope) & cityAll.stats$LSTmodel.tree.slope< -2.5 & cityAll.stats$LSTmodel.tree.p<0.01,]



ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=tree.sd, y=LSTmodel.tree.slope))
ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=veg.mean, y=LSTmodel.tree.slope))
ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=100-(veg.mean + tree.mean), y=LSTmodel.tree.slope))
ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=100-(veg.mean + tree.mean), y=LSTmodel.R2adj))


ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=trend.tree.slope, y=LSTmodel.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=trend.veg.slope, y=LSTmodel.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=(trend.veg.slope-trend.tree.slope)/trend.veg.slope, y=exp(LSTmodel.tree.slope)))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=trend.LST.slope, y=LSTmodel.tree.slope))

# ggplot(data=cityAll.stats[,]) +
#   geom_point(aes(x=tree.veg.trend, y=LSTmodel.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=veg.mean/tree.mean, y=LSTmodel.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=LSTmodel.tree.slope, y=LSTmodel.veg.slope))


# Looking at the tree slope by biome & by LST
TreeSlope.lm.biome <- lm(LSTmodel.tree.slope ~ biome-1, data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope),])
anova(TreeSlope.lm.biome)
summary(TreeSlope.lm.biome)

TreeSlope.lm.lst <- lm(LSTmodel.tree.slope ~ LST.mean, data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.tree.slope),])
anova(TreeSlope.lm.lst)
summary(TreeSlope.lm.lst)


ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),]) +
  geom_histogram(aes(x=LSTmodel.tree.slope, fill=biomeName, y=log(stat(count)))) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_alpha_manual(values=c(0.4,1)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Tree Effect as a funciton of LST
ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj) & cityAll.stats$LSTmodel.tree.p<0.01,]) +
  geom_point(aes(x = LST.mean, y=LSTmodel.tree.slope, color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Tree Effect as a funciton of mean tree cover -- where do we get diminishing returns?
ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj) & cityAll.stats$LSTmodel.tree.p<0.01,]) +
  coord_cartesian(ylim=c(-2.5, 0.5)) +
  # geom_point(aes(x = 100-tree.mean, y=LSTmodel.tree.slope, color=biomeName)) +
  # geom_smooth(method="lm", formula=(y~exp(x)), aes(x=100-tree.mean, y=LSTmodel.tree.slope)) +
  geom_point(aes(x = tree.mean, y=LSTmodel.tree.slope, color=biomeName)) +
  geom_smooth(method="lm", formula=(y~ -exp(-x)), aes(x=tree.mean, y=LSTmodel.tree.slope)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj) & cityAll.stats$LSTmodel.tree.p<0.01,]) +
  # coord_cartesian(ylim=c(0, 1.5)) +
  geom_point(aes(x = exp(-tree.mean), y=LSTmodel.tree.slope, color=biomeName), alpha=0.5) +
  geom_hline(yintercept=exp(0), linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Ross had the suggestion of plotting the best fit lines for each biome, but to do that, we'll need to compute the trends by hand 
biome.npts <- aggregate(LSTmodel.tree.slope ~ biomeName, data=cityAll.stats, FUN = length)

ggplot(data=cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj) & cityAll.stats$biomeName %in% biome.npts$biomeName[biome.npts$LSTmodel.tree.slope>20],]) +
  coord_cartesian(xlim=c(0, 20)) +
  # geom_point(aes(x = tree.mean, y=LSTmodel.tree.slope, color=biomeName), size=0.1) +
  stat_smooth(aes(x = tree.mean, y=LSTmodel.tree.slope, color=biomeName, fill=biomeName), alpha=0.3) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))



cityAll.stats[cityAll.stats$LSTmodel.tree.slope< -2.5 & !is.na(cityAll.stats$LSTmodel.tree.slope),]

summary(cityAll.stats$LSTmodel.tree.slope/cityAll.stats$LSTmodel.veg.slope)


# ##########################

# ##########################################
# Looking at regional tree cover and estimated effects on temperature ----
# BECAUSE THE UHI part (diff b/n metro area + buffer) didn't save :-( 
# ##########################################
summary(cityAll.stats)


TreeCoverBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=tree.mean, fill=biomeName), breaks=seq(0,100, by=5)) +
  # geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  # geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Tree Cover (%)") +
  # guides(fill="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

VegCoverBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=veg.mean, fill=biomeName), breaks=seq(0,100, by=5)) +
  # geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  # geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Other Veg Cover (%)") +
  guides(fill="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

summary(cityAll.stats[,c("tree.mean", "veg.mean")])

summary(cityAll.stats$tree.mean/cityAll.stats$veg.mean)
summary(cityAll.stats$veg.mean/cityAll.stats$tree.mean)


png(file.path(path.figs, "TreeVegCover_Histogram_Biome.png"), height=8, width=10, units="in", res=220)
plot_grid(TreeCoverBiomeHisto, VegCoverBiomeHisto, ncol=1, rel_heights = c(0.6, 0.4))
dev.off()





# Calculating the effect trees & veg have on regional temperatures
cityAll.stats$effect.tree.degC <- cityAll.stats$tree.mean*cityAll.stats$LSTmodel.tree.slope
cityAll.stats$effect.veg.degC <- cityAll.stats$veg.mean*cityAll.stats$LSTmodel.veg.slope
summary(cityAll.stats)

# Comparing temperature effects attributed to different veg covers
TreeEffectTempBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=effect.tree.degC, fill=biomeName), breaks=seq(-10, 10, by=2)) +
  # geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Tree Effect (deg. C)") +
  # guides(fill="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

VegEffectTempBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=effect.veg.degC, fill=biomeName), breaks=seq(-10, 10, by=2)) +
  # geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Other Veg Effect (deg. C)") +
  guides(fill="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

png(file.path(path.figs, "TreeVegEffectTemperature_Histogram_Biome.png"), height=8, width=10, units="in", res=220)
plot_grid(TreeEffectTempBiomeHisto, VegEffectTempBiomeHisto, ncol=1, rel_heights = c(0.6, 0.4))
dev.off()

summary(cityAll.stats[,c("effect.tree.degC", "effect.veg.degC")])

# ##########################################

# ##########################################
# Now looking at regional trends in cover & temperature ----
# ##########################################

grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with teal
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green

summary(cityAll.stats[,c("trend.LST.slope", "trend.tree.slope", "trend.veg.slope")])

trendLST.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.LST.slope), size=0.5) +
  scale_color_gradientn(name="LST Trend\n(deg. C / yr)", colors=grad.lst, limits=c(-0.32, 0.32)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

trendTree.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.tree.slope), size=0.5) +
  scale_color_gradientn(name="Tree Trend\n(% cover / yr)", colors=grad.tree, limits=c(-1,1)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

trendVeg.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.tree.slope), size=0.5) +
  scale_color_gradientn(name="Veg Trend\n(% cover / yr)", colors=grad.other, limits=c(-1.8,1.8)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))


png(file.path(path.figs, "Trends_LST-Tree-Veg_map.png"), height=10, width=10, units="in", res=220)
plot_grid(trendLST.map, trendTree.map, trendVeg.map, ncol=1)
dev.off()


# Translating tree trends into 
cityAll.stats$trend.tree.degC <- cityAll.stats$trend.tree.slope*cityAll.stats$LSTmodel.tree.slope
cityAll.stats$trend.veg.degC <- cityAll.stats$trend.veg.slope*cityAll.stats$LSTmodel.veg.slope
summary(cityAll.stats[,c("trend.tree.degC", "trend.veg.degC")]*20)

# Without changes in tree canopy, temperatures would have risen X% more
summary((cityAll.stats$trend.tree.degC + cityAll.stats$trend.LST.slope)/cityAll.stats$trend.LST.slope)


summary(cityAll.stats$trend.tree.degC/(cityAll.stats$trend.tree.degC + cityAll.stats$trend.LST.slope))



trendLSTSD.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trendYear.LST.sd.slope), size=0.5) +
  scale_color_gradientn(name="LST SD Trend\n(deg. C / yr)", colors=grad.lst, limits=c(-0.17, 0.17)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

trendTreeSD.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.tree.slope), size=0.5) +
  scale_color_gradientn(name="Tree SD Trend\n(% cover / yr)", colors=grad.tree, limits=c(-0.45,0.45)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

trendVegSD.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.tree.slope), size=0.5) +
  scale_color_gradientn(name="Veg SD Trend\n(% cover / yr)", colors=grad.other, limits=c(-0.6,0.6)) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))


png(file.path(path.figs, "Trends-SD_LST-Tree-Veg_map.png"), height=10, width=10, units="in", res=220)
plot_grid(trendLSTSD.map, trendTreeSD.map, trendVegSD.map, ncol=1)
dev.off()

# Looking at how many places have *decreasing* SD of LST, Trees, & Veg --> decreasing SD indicates increasing equity
summary(cityAll.stats)
length(which(!is.na(cityAll.stats$trendYear.LST.sd.slope)))

length(which(!is.na(cityAll.stats$trendYear.LST.sd.slope) & cityAll.stats$trendYear.LST.sd.slope<0 ))
length(which(!is.na(cityAll.stats$trendYear.LST.sd.slope) & cityAll.stats$trendYear.LST.sd.slope<0 & cityAll.stats$trendYear.LST.sd.p<0.05))

length(which(!is.na(cityAll.stats$trendYear.LST.sd.slope) & cityAll.stats$trendYear.LST.sd.slope>0 ))
length(which(!is.na(cityAll.stats$trendYear.LST.sd.slope) & cityAll.stats$trendYear.LST.sd.slope>0 & cityAll.stats$trendYear.LST.sd.p<0.05))



length(which(!is.na(cityAll.stats$trendYear.tree.sd.slope) & cityAll.stats$trendYear.tree.sd.slope<0 ))
# length(which(!is.na(cityAll.stats$trendYear.tree.sd.slope) & cityAll.stats$trendYear.tree.sd.slope<0 & cityAll.stats$trendYear.tree.sd.p<0.05))

length(which(!is.na(cityAll.stats$trendYear.veg.sd.slope) & cityAll.stats$trendYear.veg.sd.slope<0 ))
# length(which(!is.na(cityAll.stats$trendYear.veg.sd.slope) & cityAll.stats$trendYear.veg.sd.slope<0 & cityAll.stats$trendYear.veg.sd.p<0.05))

plot.trendCorrTree <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biomeName),]) +
  facet_wrap(~biomeName) +
  geom_hline(yintercept=0, size=0.25) +
  geom_vline(xintercept=0, size=0.25) +
  geom_point(aes(x=trendYear.tree.mean.slope, y=trendYear.tree.sd.slope, color=biomeName), size=0.5, alpha=0.5) +
  stat_smooth(method="lm" ,aes(x=trendYear.tree.mean.slope, y=trendYear.tree.sd.slope, color=biomeName, fill=biomeName)) +
  labs(x="mean tree cover trend (%/yr)", y="sd tree cover trend (%/yr)") +
  annotate(geom="text", x=0.6, y=-0.30, label="+ cover\n - inequality\n(Good)", size=3) +
  annotate(geom="text", x=-0.6, y=+0.30, label="- cover\n + inequality\n(Bad)", size=3) +
  scale_color_manual(name="biome", values=biome.pall.all) +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

png(file.path(path.figs, "Trends-mean-sd_Tree_corr.png"), height=10, width=12, units="in", res=220)
plot.trendCorrTree
dev.off()

# ##########################################


# ##########################################
# Looking at some of the ET stuff ----
# ##########################################
summary(cityAll.stats)
ggplot(data=cityAll.stats) +
  geom_point(aes(x=aet, y=ET.mean))

# ##########################################
