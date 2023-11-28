# Looking at changes in water demand & availability for vegetation
# 1. [other scripts] Here’s what tree cover you need to offset urban heat; benchmark against current cover & trajectories (compare to Konijnendijk’s 3-30-300 rule) [done]
# 2. Here’s how much more water would needed to sustain that canopy; compared to current precipitation [have estimated ET; just need calcs]
# 3. In a warming climate, the amount of water needed to sustain vegetation will increase – how much will that be??
  

library(ggplot2)

# file paths for where to put the processed data ----
path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")

file.cityClim <- file.path(path.cities, "city_climatology.csv")
file.cityAll.stats <- file.path(path.cities, "city_stats_all copy.csv")

# -------------------
# Reading in base data form our original analyses ----
# -------------------
cityAll.stats <- read.csv(file.cityAll.stats)
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
summary(cityAll.stats)

# Adjusting our ET numbers to be per day --> currently kg/m2/8day --> this won't be necessary with the updated run
cityAll.stats[, c("ET.mean", "ET.sd", "ET.min", "ET.max")] <- cityAll.stats[, c("ET.mean", "ET.sd", "ET.min", "ET.max")]/8
cityAll.stats[, c("ETmodel.tree.slope", "ETmodel.veg.slope")] <- cityAll.stats[, c("ETmodel.tree.slope", "ETmodel.veg.slope")]/8

cityAll.stats
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
cityClim <- read.csv(file.cityClim)
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
  
  rowDiff <- which(diffClim$ISOURBID==URBID)
  cityAll.stats[i,c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")] <- cityClim[rowRef, c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")]
  cityAll.stats[i,c("tmax2", "tmin2", "ppt2", "aet2", "pet2", "def2", "soil2")] <- cityClim[rowFut2, c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")]
  cityAll.stats[i,c("tmax4", "tmin4", "ppt4", "aet4", "pet4", "def4", "soil4")] <- cityClim[rowFut4, c("tmax", "tmin", "ppt", "aet", "pet", "def", "soil")]
  
}

summary(cityAll.stats)

ggplot(data=diffClim) +
  # facet_grid(TIME~.) +
  geom_histogram(aes(x=ppt.Fut4))

ggplot(data=diffClim) +
  # facet_grid(TIME~.) +
  geom_histogram(aes(x=pet.Fut4))

ggplot(data=diffClim) +
  # facet_grid(TIME~.) +
  geom_histogram(aes(x=tmax.Fut2))
# -------------------

# -------------------
# Water Analysis 1: how much water does the current canopy need; would be needed to increase tree cover to target ----
# -------------------
# How much water the current canopy needs: current % tree cover x ET Tree slope
cityAll.stats$CurrentCanopyWater <- cityAll.stats$tree.mean*cityAll.stats$ETmodel.tree.slope
summary(cityAll.stats)
# -------------------







# -------------------
# -------------------
# -------------------