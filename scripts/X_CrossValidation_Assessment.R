# Assessment of the cross-validation stats

library(ggplot2)

path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/")

# Read in the base model stats

# Read in the LST & ET stats
xValidLST <- read.csv(file.path(path.cities, "CrossValidation-LST.csv"))
xValidET <- read.csv(file.path(path.cities, "CrossValidation-ET.csv"))

summary(xValidLST)
summary(xValidET)
