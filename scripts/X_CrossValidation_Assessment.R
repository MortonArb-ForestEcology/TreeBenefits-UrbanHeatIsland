# Assessment of the cross-validation stats

library(ggplot2)

path.google <- file.path("~/Google Drive/")
path.cities <- file.path(path.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v4.1/")

# Read in the base model stats

# Read in the LST & ET stats
nLSTold <- nLSTnew
nETold <- nETnew
SysLast <- SysStart


SysStart <- Sys.time()
xValidLST <- read.csv(file.path(path.cities, "CrossValidation-LST.csv"))
xValidET <- read.csv(file.path(path.cities, "CrossValidation-ET.csv"))
summary(xValidLST)
summary(xValidET)

nLSTnew <- nrow(xValidLST[!is.na(xValidLST$spatError.mean),])
nETnew <- nrow(xValidET[!is.na(xValidET$spatError.mean),])


SysStart - SysLast
as.numeric(SysStart - SysLast)/(nLSTnew - nLSTold)*60 # time per file
as.numeric(SysStart - SysLast) /(nETnew - nETold )*60 # time per file
