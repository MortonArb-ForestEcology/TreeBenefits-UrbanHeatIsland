# Script to check the ET claculations 
if (!grepl("scripts", getwd())) setwd("scripts")

library(terra)
library(lubridate)
library(ggplot2); library(RColorBrewer); library(cowplot)

# ---- Paths ----------------------------------------------------------------
path.google   <- file.path("~/Google Drive/My Drive")
path.proc   <- file.path("~/Google Drive/Shared Drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v5") 
path.v4       <- file.path(path.google, "UHI_Analysis_Output_Final_v4")
path.v5       <- file.path(path.google, "UHI_Analysis_Output_Final_v5")
path.shp      <- file.path("..", "data_raw", "sdei-global-uhi-2013-shp", "shp",
                           "sdei-global-uhi-2013.shp")

file.elev.lookup <- file.path(path.proc, "city_elev_lookup.csv")
file.et0.summary <- file.path(path.proc, "city_ET0_summary.csv")
file.et0.means   <- file.path(path.proc, "city_ET0_20yr_means.csv")

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

# ---- Read in Data ----------------------------------------------------------------
# In the following datasets:
# ET0 = calculated ET0 from ERA5-Land met
# et_total = toal evapotranspritation from ERA5-Land met
# et_transp = vegetation transpriration comonent

et0.means <- read.csv(file.et0.means) # These are the mean ET0 values for each city across the 20 year period.
et0.yrs <- read.csv(file.et0.summary) # This is the ET0 value for each year

summary(et0.yrs)

ggplot(data=et0.means, aes(x=ET0_mean_daily, y=et_total_mean_daily)) +
    geom_point() +
    geom_abline(slope=1, intercept=0, color="red") +
    theme_bw() 

ggplot(data=et0.means, aes(x=ET0_mean_daily, y=et_transp_mean_daily)) +
    geom_point() +
    geom_abline(slope=1, intercept=0, color="red") +
    theme_bw() 

et0.yrs
nrow(sum.et0)/nrow(et0.means)

