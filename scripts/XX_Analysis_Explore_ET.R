# Looking more into how cooling patterns and water use correspond

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
nrow(cityAll.stats[!is.na(cityAll.stats$LSTmodel.R2adj),])

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
# Comparing cooling & ET effects
# ##########################################
names(cityAll.stats)
# summary(cityAll.stats[,c("LST.NoVeg.model.R2adj", "LSTmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "ETmodel.tree.slope", "ETmodel.veg.slope", "LSTmodel.tree.p", "LSTmodel.veg.p", "ETmodel.tree.p", "ETmodel.veg.p")])
ncities <- length(which(!is.na(cityAll.stats$TreeVeg.LSTratio))) # More cooling from trees

summary(cityAll.stats[,c("LST.NoVeg.model.R2adj", "LSTmodel.R2adj", "LSTmodel.tree.slope", "LSTmodel.veg.slope", "ETmodel.tree.slope", "ETmodel.veg.slope")])


# ------------
# Comparing Veg Cover types
# ------------
# Looking at how much more cooling per percent cover trees provide; >1 = trees more cooling
cityAll.stats$TreeVeg.LSTratio <- cityAll.stats$LSTmodel.tree.slope/cityAll.stats$LSTmodel.veg.slope
summary(cityAll.stats$TreeVeg.LSTratio)
treecool <- length(which(!is.na(cityAll.stats$TreeVeg.LSTratio) & cityAll.stats$TreeVeg.LSTratio>1)) # More cooling from trees
vegcool <- length(which(!is.na(cityAll.stats$TreeVeg.LSTratio) & cityAll.stats$TreeVeg.LSTratio<1)) # More cooling from other veg
treecool; treecool/ncities
vegcool; vegcool/ncities

# Looking at how much higher per percent cover ET is; >1 means trees use more water
cityAll.stats$TreeVeg.ETratio <- cityAll.stats$ETmodel.tree.slope/cityAll.stats$ETmodel.veg.slope
summary(cityAll.stats$TreeVeg.ETratio)
length(which(!is.na(cityAll.stats$TreeVeg.ETratio) & cityAll.stats$TreeVeg.ETratio>1)) # More water from trees
length(which(!is.na(cityAll.stats$TreeVeg.ETratio) & cityAll.stats$TreeVeg.ETratio<1)) # More water from other veg

# ------------


# ------------
# Looking at how much cooling you get for the water used by trees or non-tree veg
# ------------
# deg. c per kg/m2 water from trees
cityAll.stats$LST.ETratio.tree <- cityAll.stats$LSTmodel.tree.slope/cityAll.stats$ETmodel.tree.slope
summary(cityAll.stats$LST.ETratio.tree)


# deg. c per kg/m2 water from other veg
cityAll.stats$LST.ETratio.veg <- cityAll.stats$LSTmodel.veg.slope/cityAll.stats$ETmodel.veg.slope
summary(cityAll.stats$LST.ETratio.veg)

# >1 means trees more efficient; <1 means non-tree veg more efficient --> will be good to see how much this varies among biomes!
cityAll.stats$LST.ETratio.comparison <- cityAll.stats$LST.ETratio.tree/cityAll.stats$LST.ETratio.veg
summary(cityAll.stats$LST.ETratio.comparison)

# Looking at the number of cities where trees or other veg are more efficient
treeEff <- length(which(!is.na(cityAll.stats$LST.ETratio.comparison) & cityAll.stats$LST.ETratio.comparison>1)) # Trees more efficient
vegEff <- length(which(!is.na(cityAll.stats$LST.ETratio.comparison) & cityAll.stats$LST.ETratio.comparison<1)) # Other Veg more efficient

treeEff; treeEff/ncities
vegEff; vegEff/ncities
# ------------

# ##########################################
