# ERA5-Land daily city-level extraction via Copernicus CDS API.
# Replaces GEE-based 01f_ERA_Formatting.R; avoids GEE task throttling.
# Output: {ISOURBID}_ERA5_daily.csv per city, identical format to GEE version.
#
# FIRST-TIME SETUP (run once in console before executing script):
#   install.packages("ecmwfr")
#   ecmwfr::wf_set_key(key="YOUR_CDS_PERSONAL_ACCESS_TOKEN")
#   # Get token at: https://cds-beta.climate.copernicus.eu -> user icon -> API tokens
#
# DOWNLOAD APPROACH:
#   3 global netCDF files from CDS (derived-era5-land-daily-statistics):
#     1. daily_mean of 10 variables, months 1+2+7+8, years 2001-2020
#     2. daily_minimum of 2m_temperature (for tmin)
#     3. daily_maximum of 2m_temperature (for tmax)
#   Files downloaded to data_raw/era5_cds_temp/, deleted after extraction.
#
# UNIT CONVENTIONS:
#   CDS daily_mean of flux variables (precip, radiation, evaporation) gives the
#   mean 1-hourly accumulation. Multiplying by 24 yields the full daily total —
#   equivalent to the "sum" bands in GEE's ECMWF/ERA5_LAND/DAILY_AGGR.
#   Evaporation in ERA5 is negative (upward flux); multiply by -1 to get positive mm.

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
shp.path <- "../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"
sdei.df   <- data.frame(vect(shp.path))
sdei.df   <- sdei.df[sdei.df$ES00POP >= 100e3 & sdei.df$SQKM_FINAL >= 1e2, ]

sdei.vect <- vect(shp.path)
sdei.vect <- sdei.vect[sdei.vect$ES00POP >= 100e3 & sdei.vect$SQKM_FINAL >= 1e2, ]

# Probably do this when we extract each city!
sdei.buf  <- buffer(sdei.vect, width=10000)  # 10 km geodesic buffer, matches GEE buffer(10e3)

# ---- Skip cities with existing output ----
if(!overwrite) {
  done  <- sub("_ERA5_daily\\.csv$", "", dir(dir.out, pattern="_ERA5_daily\\.csv$"))
  keep  <- !sdei.df$ISOURBID %in% done
  sdei.df   <- sdei.df[keep, ]
  sdei.vect <- sdei.vect[keep, ]
  # sdei.buf  <- sdei.buf[keep, ]
}
message(nrow(sdei.df), " cities to process.")
if(nrow(sdei.df) == 0) stop("All cities already processed. Set overwrite=TRUE to rerun.")


# ---- CDS download function ----
# Submits request to CDS and blocks until download completes.
# Skips download if destFile already exists (allows restarting without re-downloading).
#
# VERIFY PARAMETERS against CDS catalog before first run:
#   https://cds-beta.climate.copernicus.eu/datasets/derived-era5-land-daily-statistics
# The 'daily_statistic' key and accepted values should match the web form exactly.
cds_download <- function(variables, statistic, months, dest_file) {
  if(file.exists(dest_file)) {
    message("Using cached: ", basename(dest_file))
    return(invisible(dest_file))
  }
  request <- list(
    dataset_short_name = "derived-era5-land-daily-statistics",
    product_type       = "reanalysis",
    variable           = variables,
    year               = as.character(2001:2020),
    month              = sprintf("%02d", months),
    day                = sprintf("%02d", 1:31),  # CDS filters invalid dates automatically
    daily_statistic    = statistic,
    time_zone          = "UTC+00:00",
    frequency          = "1_hourly",             # use all 24 hourly values for aggregation
    format             = "netcdf",
    target             = basename(dest_file)
  )
  wf_request(request=request, path=dirname(dest_file), verbose=TRUE)
  invisible(dest_file)
}

# ---- CDS variable names ----
# CDS API name -> ECMWF short name in downloaded netCDF (used by terra::rast subds)
# GEE DAILY_AGGR name -> CDS API name
#   temperature_2m                      -> 2m_temperature             -> t2m
#   dewpoint_temperature_2m             -> 2m_dewpoint_temperature    -> d2m
#   surface_pressure                    -> surface_pressure           -> sp
#   u_component_of_wind_10m             -> 10m_u_component_of_wind    -> u10
#   v_component_of_wind_10m             -> 10m_v_component_of_wind    -> v10
#   total_precipitation_sum             -> total_precipitation        -> tp
#   surface_solar_radiation_downwards_sum -> surface_solar_radiation_downwards -> ssrd
#   surface_thermal_radiation_downwards_sum -> surface_thermal_radiation_downwards -> strd
#   total_evaporation_sum               -> total_evaporation          -> e
#   evaporation_from_bare_soil_sum      -> evaporation_from_bare_soil -> evbs
#   temperature_2m_min/max: requested as daily_minimum/maximum of 2m_temperature -> t2m

vars_mean_cds <- c(
  "2m_temperature",
  "2m_dewpoint_temperature",
  "surface_pressure",
  "10m_u_component_of_wind",
  "10m_v_component_of_wind",
  "total_precipitation",
  "surface_solar_radiation_downwards",
  "surface_thermal_radiation_downwards",
  "total_evaporation",
  "evaporation_from_bare_soil"
)

# netCDF short names terra will use (in the order CDS outputs them)
# If terra reports unexpected variable names, check: names(terra::rast(nc_mean))
nc_shortnames <- c("t2m", "d2m", "sp", "u10", "v10", "tp", "ssrd", "strd", "e", "evbs")


# ---- Unit conversion ----
# raw_list: named list of numeric vectors (one value per date), from terra::extract
# dates_vec: Date vector corresponding to each element
convert_units <- function(raw_list, dates_vec) {
  data.frame(
    year        = year(dates_vec),
    date        = format(dates_vec, "%Y-%m-%d"),
    # Instantaneous variables: daily_mean is the daily average
    tmean_C     = raw_list$t2m   - 273.15,
    # Flux variables: daily_mean * 24 = daily total (same as GEE DAILY_AGGR sum bands)
    precip_mm   = raw_list$tp    * 24 * 1e3,          # m/h * 24h * 1000 = mm/day
    rlong_MJm2  = raw_list$strd  * 24 / 1e6,          # J/m2/h * 24h / 1e6 = MJ/m2/day
    rshort_MJm2 = raw_list$ssrd  * 24 / 1e6,          # solar radiation (FIXED: was thermal in original)
    wind_ms     = sqrt(raw_list$u10^2 + raw_list$v10^2),
    tdew_C      = raw_list$d2m   - 273.15,
    press_kPA   = raw_list$sp    / 1e3,
    tmin_C      = raw_list$tmin  - 273.15,             # from daily_minimum file
    tmax_C      = raw_list$tmax  - 273.15,             # from daily_maximum file
    et_mm       = raw_list$evbs  * 24 * (-1e3),        # negative ERA5 convention -> positive mm
    et_total_mm = raw_list$e     * 24 * (-1e3),
    stringsAsFactors = FALSE
  )
}


# ---- Core extraction function ----
# For one season (NH or SH): filters downloaded rasters to the correct months,
# extracts spatial means for all cities at once, writes per-city CSVs.
extract_season <- function(nc_mean, nc_tmin, nc_tmax,
                           city_buf, city_ids,
                           season_months, dir_out, overwrite=FALSE) {

  message("Loading mean variables netCDF...")
  r_mean <- terra::rast(nc_mean)
  times  <- as.Date(terra::time(r_mean))
  keep   <- month(times) %in% season_months
  r_mean <- r_mean[[keep]]
  dates  <- times[keep]

  message("Loading tmin/tmax netCDFs...")
  r_tmin_full <- terra::rast(nc_tmin)
  r_tmax_full <- terra::rast(nc_tmax)
  r_tmin <- r_tmin_full[[month(as.Date(terra::time(r_tmin_full))) %in% season_months]]
  r_tmax <- r_tmax_full[[month(as.Date(terra::time(r_tmax_full))) %in% season_months]]

  message("Extracting spatial means for ", length(city_ids), " cities (all dates at once)...")
  # terra::extract returns n_cities x n_layers matrix (one col per date)
  ex_mean <- terra::extract(r_mean, city_buf, fun=mean, na.rm=TRUE, ID=FALSE)
  ex_tmin <- terra::extract(r_tmin, city_buf, fun=mean, na.rm=TRUE, ID=FALSE)
  ex_tmax <- terra::extract(r_tmax, city_buf, fun=mean, na.rm=TRUE, ID=FALSE)

  # Identify which columns correspond to which variable using layer varnames
  vn <- terra::varnames(r_mean)  # one varname per layer, repeated for each date
  n_dates <- sum(keep)

  # Build an index mapping each variable to its column positions in ex_mean
  # Variables appear in the same order as vars_mean_cds; each repeats n_dates times
  var_col_idx <- lapply(nc_shortnames, function(v) which(vn == v))

  message("Writing per-city CSVs...")
  pb <- txtProgressBar(min=0, max=length(city_ids), style=3)
  for(i in seq_along(city_ids)) {
    setTxtProgressBar(pb, i)
    cityID  <- city_ids[i]
    outFile <- file.path(dir_out, paste0(cityID, "_ERA5_daily.csv"))
    if(!overwrite && file.exists(outFile)) next

    # Extract this city's values for each variable across all dates
    raw <- lapply(seq_along(nc_shortnames), function(j) {
      as.numeric(ex_mean[i, var_col_idx[[j]]])
    })
    names(raw) <- nc_shortnames
    raw$tmin <- as.numeric(ex_tmin[i, ])
    raw$tmax <- as.numeric(ex_tmax[i, ])

    city_df <- convert_units(raw, dates)
    write.csv(city_df, outFile, row.names=FALSE)
  }
  close(pb)
  invisible(NULL)
}


# ---- Download ERA5 from CDS ----
# Months 1+2 (SH summer = Jan/Feb) and 7+8 (NH summer = Jul/Aug) in one batch.
all_months <- c(1, 2, 7, 8)

message("=== Downloading ERA5-Land daily statistics from CDS ===")
message("This may take 30-120 minutes depending on CDS queue load.")
message("Progress: ")

nc_mean <- cds_download(
  variables = vars_mean_cds,
  statistic = "daily_mean",
  months    = all_months,
  dest_file = file.path(dir.temp, "era5_daily_mean_JanFeb_2001-2020.nc")
)
nc_tmin <- cds_download(
  variables = "2m_temperature",
  statistic = "daily_minimum",
  months    = all_months,
  dest_file = file.path(dir.temp, "era5_daily_tmin_JulAug_JanFeb_2001-2020.nc")
)
nc_tmax <- cds_download(
  variables = "2m_temperature",
  statistic = "daily_maximum",
  months    = all_months,
  dest_file = file.path(dir.temp, "era5_daily_tmax_JulAug_JanFeb_2001-2020.nc")
)

message("=== All downloads complete. Starting extraction. ===")


# ---- Hemisphere splits ----
idx_NH <- which(sdei.df$LATITUDE >= 0)
idx_SH <- which(sdei.df$LATITUDE < 0)

if(length(idx_NH) > 0) {
  message("--- NH cities (Jul/Aug): ", length(idx_NH), " ---")
  extract_season(
    nc_mean, nc_tmin, nc_tmax,
    city_buf     = sdei.buf[idx_NH, ],
    city_ids     = sdei.df$ISOURBID[idx_NH],
    season_months = c(7, 8),
    dir_out      = dir.out,
    overwrite    = overwrite
  )
}

if(length(idx_SH) > 0) {
  message("--- SH cities (Jan/Feb): ", length(idx_SH), " ---")
  extract_season(
    nc_mean, nc_tmin, nc_tmax,
    city_buf     = sdei.buf[idx_SH, ],
    city_ids     = sdei.df$ISOURBID[idx_SH],
    season_months = c(1, 2),
    dir_out      = dir.out,
    overwrite    = overwrite
  )
}

message("=== Done! CSVs written to: ", dir.out, " ===")


# ---- VERIFICATION (run manually on a few cities already extracted via GEE) ----
# Compare CDS-derived CSV against existing GEE CSV for the same city.
# Values should be within floating-point rounding (~1e-4 relative difference).
#
# test_city <- "USA4201"  # pick a city with an existing GEE CSV
# gee_csv <- read.csv(file.path(dir.out, paste0(test_city, "_ERA5_daily.csv")))
# cds_csv <- read.csv(file.path(dir.out, paste0(test_city, "_ERA5_daily_CDS.csv")))
# # compare column-by-column:
# all.equal(gee_csv$tmean_C, cds_csv$tmean_C, tolerance=1e-3)
# all.equal(gee_csv$precip_mm, cds_csv$precip_mm, tolerance=1e-3)
# plot(gee_csv$tmean_C, cds_csv$tmean_C, xlab="GEE", ylab="CDS", main="tmean validation")
# abline(0,1, col="red")
