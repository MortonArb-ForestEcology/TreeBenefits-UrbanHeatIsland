# Formatting and Extracting ERA5-Land reanalysis based on continued reviewer comments.
# this might get used in one of a couple ways
# 1. Use to parameterize a mechanistic model of ET rather than rely on MODIS (probably possible, requires PM model etc; would bea lot of work)
# 2. Use to train a 10km-scale ET model instead of our MODIS ET (if train at 10 km scale, could *potentially* downscale to 1km or less?)
# 3. Use to calibrate/validate city-scale ET estimates; possibly still using out existing MODIS benchmark

# Step 1: Get ERA formatted like our existing MODIS data
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR 
# Variables we care about -- CITY SCALE
