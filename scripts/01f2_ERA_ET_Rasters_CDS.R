# ERA5-Land spatial ET raster export via Copernicus CDS API.
# Replaces GEE-based 01f2_ERA_ET_Rasters.R; avoids GEE task throttling.
# Output: {ISOURBID}_ERA5_et_raster.tif per city, 20-band GeoTIFF (one band per year 2001-2020).
# Each band = mean daily et_total_mm across all summer days in that year, at ERA5 native ~9km resolution.
# Clipped to city + 10 km buffer extent.
#
# REQUIRES: CDS credentials set up (see 01f_ERA_Formatting_CDS.R header).
# Can reuse nc_etotal from 01f if both scripts are run together (saves re-download).
#
# UNIT CONVENTION:
#   CDS daily_mean of total_evaporation (variable 'e') gives mean 1-hourly accumulation in m.
#   Daily total = daily_mean * 24 hours. ERA5 convention: evaporation is negative.
#   et_total_mm = daily_mean * 24 * (-1000): positive mm/day.

if(!grepl("scripts", getwd())) setwd("scripts")

library(terra)
library(ecmwfr)
library(lubridate)

# ---- Paths ----
path.google      <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
dir.out  <- file.path(path.google, GoogleFolderSave)
dir.temp <- file.path(dirname(getwd()), "data_raw", "era5_cds_temp")
dir.create(dir.out,  showWarnings=FALSE, recursive=TRUE)
dir.create(dir.temp, showWarnings=FALSE, recursive=TRUE)

overwrite <- FALSE

# ---- Load and filter cities ----
shp.path  <- "../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"
sdei.df   <- data.frame(vect(shp.path))
sdei.df   <- sdei.df[sdei.df$ES00POP >= 100e3 & sdei.df$SQKM_FINAL >= 1e2, ]

sdei.vect <- vect(shp.path)
sdei.vect <- sdei.vect[sdei.vect$ES00POP >= 100e3 & sdei.vect$SQKM_FINAL >= 1e2, ]
sdei.buf  <- buffer(sdei.vect, width=10000)

# ---- Skip cities with existing rasters ----
if(!overwrite) {
  done <- sub("_ERA5_et_raster\\.tif$", "", dir(dir.out, pattern="_ERA5_et_raster\\.tif$"))
  keep <- !sdei.df$ISOURBID %in% done
  sdei.df   <- sdei.df[keep, ]
  sdei.vect <- sdei.vect[keep, ]
  sdei.buf  <- sdei.buf[keep, ]
}
message(nrow(sdei.df), " cities to process.")
if(nrow(sdei.df) == 0) stop("All cities already processed. Set overwrite=TRUE to rerun.")


# ---- Download total_evaporation from CDS ----
# Reuse file from 01f if already downloaded; otherwise download here.
nc_et_path <- file.path(dir.temp, "era5_daily_mean_JulAug_JanFeb_2001-2020.nc")
nc_et_only_path <- file.path(dir.temp, "era5_daily_etotal_JulAug_JanFeb_2001-2020.nc")

if(file.exists(nc_et_path)) {
  # Reuse the full mean file from 01f (contains 'e' sub-dataset)
  nc_et <- nc_et_path
  use_full_mean_file <- TRUE
  message("Reusing full mean netCDF from 01f.")
} else {
  use_full_mean_file <- FALSE
  if(!file.exists(nc_et_only_path)) {
    message("Downloading total_evaporation from CDS...")
    request <- list(
      dataset_short_name = "derived-era5-land-daily-statistics",
      product_type       = "reanalysis",
      variable           = "total_evaporation",
      year               = as.character(2001:2020),
      month              = sprintf("%02d", c(1, 2, 7, 8)),
      day                = sprintf("%02d", 1:31),
      daily_statistic    = "daily_mean",
      time_zone          = "UTC+00:00",
      frequency          = "1_hourly",
      format             = "netcdf",
      target             = basename(nc_et_only_path)
    )
    wf_request(request=request, path=dir.temp, verbose=TRUE)
  }
  nc_et <- nc_et_only_path
}

message("Loading ET raster...")
r_et_full <- if(use_full_mean_file) terra::rast(nc_et, subds="e") else terra::rast(nc_et)

times_et  <- as.Date(terra::time(r_et_full))
years_all <- 2001:2020


# ---- Raster export function ----
# For each city: crops et raster to city+buffer, computes annual summer mean,
# stacks 20 annual bands, writes as GeoTIFF matching GEE output format.
export_et_raster <- function(r_et_full, times_et, years_all,
                              city_buf_vec, city_ids, sdei_df,
                              season_months, dir_out, overwrite=FALSE) {

  # Filter to season months once (shared across all cities)
  keep_season <- month(times_et) %in% season_months
  r_et        <- r_et_full[[keep_season]]
  times       <- times_et[keep_season]
  years_vec   <- year(times)

  pb <- txtProgressBar(min=0, max=length(city_ids), style=3)
  for(i in seq_along(city_ids)) {
    setTxtProgressBar(pb, i)
    cityID  <- city_ids[i]
    outFile <- file.path(dir_out, paste0(cityID, "_ERA5_et_raster.tif"))
    if(!overwrite && file.exists(outFile)) next

    buf_i <- city_buf_vec[i, ]

    # Crop ERA5 to city buffer extent
    r_city <- tryCatch(
      terra::crop(r_et, buf_i),
      error = function(e) { warning("Crop failed for ", cityID, ": ", e$message); NULL }
    )
    if(is.null(r_city)) next

    # Compute annual mean of daily et_total_mm, one band per year
    annual_bands <- lapply(years_all, function(yr) {
      yr_layers <- r_city[[years_vec == yr]]
      if(nlyr(yr_layers) == 0) return(NULL)
      # Convert daily_mean (m/h) to daily total mm: * 24 * (-1000)
      # Then take the seasonal mean across all days in this year's summer window
      yr_mm <- yr_layers * 24 * (-1000)  # daily total mm (positive), one layer per day
      mn    <- terra::mean(yr_mm, na.rm=TRUE)
      names(mn) <- paste0("et_total_", yr)
      mn
    })

    # Remove any NULL years (shouldn't happen with 2001-2020 coverage)
    annual_bands <- Filter(Negate(is.null), annual_bands)
    if(length(annual_bands) == 0) next

    et_stack <- terra::rast(annual_bands)  # 20-band SpatRaster

    terra::writeRaster(
      et_stack,
      filename  = outFile,
      filetype  = "GTiff",
      overwrite = overwrite,
      datatype  = "FLT4S"
    )
  }
  close(pb)
  invisible(NULL)
}


# ---- Hemisphere splits and run ----
idx_NH <- which(sdei.df$LATITUDE >= 0)
idx_SH <- which(sdei.df$LATITUDE <  0)

if(length(idx_NH) > 0) {
  message("--- NH cities (Jul/Aug): ", length(idx_NH), " ---")
  export_et_raster(
    r_et_full, times_et, years_all,
    city_buf_vec  = sdei.buf[idx_NH, ],
    city_ids      = sdei.df$ISOURBID[idx_NH],
    sdei_df       = sdei.df[idx_NH, ],
    season_months = c(7, 8),
    dir_out       = dir.out,
    overwrite     = overwrite
  )
}

if(length(idx_SH) > 0) {
  message("--- SH cities (Jan/Feb): ", length(idx_SH), " ---")
  export_et_raster(
    r_et_full, times_et, years_all,
    city_buf_vec  = sdei.buf[idx_SH, ],
    city_ids      = sdei.df$ISOURBID[idx_SH],
    sdei_df       = sdei.df[idx_SH, ],
    season_months = c(1, 2),
    dir_out       = dir.out,
    overwrite     = overwrite
  )
}

message("=== Done! GeoTIFFs written to: ", dir.out, " ===")


# ---- VERIFICATION ----
# test_city <- "USA4201"
# ras <- terra::rast(file.path(dir.out, paste0(test_city, "_ERA5_et_raster.tif")))
# nlyr(ras)          # should be 20
# names(ras)         # should be et_total_2001 ... et_total_2020
# plot(ras[[1]])     # 2001 summer mean ET map for city buffer
# summary(values(ras[[1]], na.rm=TRUE))  # typical range ~0.5-5 mm/day
