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
path.google <- file.path("~/Google Drive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v3")
# path.google <- file.path(path.google)
path.raw <- file.path("~/Google Drive/My Drive/UHI_Analysis_Output_Final_v3/")

# Reading in our ET dataset
cityAll.ET <-  read.csv(file.path(path.google, "city_stats_all_ET.csv"))
# cityAll.ET <- read.csv("~/Desktop/city_stats_all_ET.csv") # read.csv(file.path(path.google, "city_stats_all_ET.csv"))
cityAll.ET$ETmodel.R2adj[cityAll.ET$ETmodel.R2adj<0] <- NA
cityAll.ET <- cityAll.ET[!is.na(cityAll.ET$ETmodel.R2adj) & cityAll.ET$ETobs.max>1,] # get rid of anything we didn't model or that has a very low range of ET
summary(cityAll.ET)

# Original dataset without the year intercept
cityAll.ETOrig <- read.csv(file.path(path.google, "city_stats_all_ET_v1_2023-12-20.csv"))
# cityAll.ET$ETmodel.R2adj[cityAll.ET$ETmodel.R2adj<0] <- NA
cityAll.ETOrig <- cityAll.ETOrig[cityAll.ETOrig$ISOURBID %in% cityAll.ET$ISOURBID,] # get rid of anything we didn't model or that has a very low range of ET
summary(cityAll.ETOrig)

summary(cityAll.ETOrig[,c("ISOURBID", "ETmodel.R2adj", "ETmodel.RMSE")])
summary(cityAll.ET[,c("ISOURBID", "ETmodel.R2adj", "ETmodel.RMSE")])
summary(cityAll.ET$ETmodel.R2adj - cityAll.ETOrig$ETmodel.R2adj)
hist(cityAll.ET$ETmodel.R2adj - cityAll.ETOrig$ETmodel.R2adj)
hist(cityAll.ET$ETmodel.RMSE - cityAll.ETOrig$ETmodel.RMSE)

plot(cityAll.ET$ETmodel.R2adj ~ cityAll.ETOrig$ETmodel.R2adj); abline(a=0, b=1, col="red2")
plot(cityAll.ET$ETmodel.RMSE ~ cityAll.ETOrig$ETmodel.RMSE); abline(a=0, b=1, col="red2")

par(mfrow=c(2,2))
hist(cityAll.ETOrig$ETmodel.R2adj, xlim=c(0,1), main="Original R2"); hist(cityAll.ETOrig$ETmodel.RMSE, xlim=c(0,1.5), main="Original RMSE")
hist(cityAll.ET$ETmodel.R2adj, xlim=c(0,1), main="New R2"); hist(cityAll.ET$ETmodel.RMSE,  xlim=c(0,1.5), main="New RMSE"); par(mfrow=c(1,1))

length(which(cityAll.ET$ETmodel.R2adj<0.33))/nrow(cityAll.ET); length(which(cityAll.ETOrig$ETmodel.R2adj<0.33))/nrow(cityAll.ETOrig)
length(which(cityAll.stats$LSTmodel.R2adj<0.33))/nrow(cityAll.stats)
summary(cityAll.stats[,c("ISOURBID", "LSTmodel.R2adj")])

