# Calculate FAO-56 Penman-Monteith ET0 for all cities using ERA5-Land city-average CSVs.
#
# Inputs:
#   - {ISOURBID}_ERA5_daily.csv  (v5 Google Drive folder)
#   - {ISOURBID}_elevation.tif   (v4 Google Drive folder; SRTM mean elevation per city)
#   - sdei-global-uhi-2013.shp   (city lat/lon)
#
# Key methodological notes:
#   - FAO-56 Penman-Monteith (Allen et al. 1998, Eq. 6); functions in 0_ET0-functions.R
#   - ERA5 surface pressure used directly for the psychrometric constant (gamma) instead
#     of the standard-atmosphere altitude formula (FAO-56 Eq. 7), capturing real daily
#     pressure variability (Allen et al. 1998).
#   - City mean elevation (from SRTM rasters) used for clear-sky radiation Rso (Eq. 37).
#   - Wind speed at 10 m corrected to 2 m internally in calc_daily_et0() (FAO-56 Eq. 47).
#   - et_transp_mm: sourced from GEE band 'evaporation_from_bare_soil_sum', which despite
#     its name contains VEGETATION TRANSPIRATION due to a known ECMWF/GEE band label swap
#     in the ERA5-Land daily aggregates dataset. et_total_mm (param 182) is the true total
#     evaporation from all components and is unaffected by this swap.
#     Reference: https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR
#
# Outputs:
#   - {ISOURBID}_ERA5_ET0_daily.csv  per city  → v5 Google Drive folder
#   - data_processed/city_ET0_summary.csv       one row per city-year
#   - data_processed/city_ET0_20yr_means.csv    20-year means per city

if (!grepl("scripts", getwd())) setwd("scripts")

library(terra)
library(lubridate)

source("0_ET0-functions.R")

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

overwrite <- FALSE

# ---- Step 0: Build city elevation lookup (run once, cached) ---------------
# Reads mean elevation from SRTM rasters extracted via GEE (v4 output folder).
# Saved to city_elev_lookup.csv so the rasters only need to be read once.
if (!file.exists(file.elev.lookup) | overwrite) {
  message("Building city elevation lookup from v4 rasters...")
  elev.files <- dir(path.v4, pattern = "_elevation\\.tif$", full.names = FALSE)
  elev.ids   <- sub("_elevation\\.tif$", "", elev.files)

  elev.means <- vapply(elev.files, function(f) {
    r <- terra::rast(file.path(path.v4, f))
    mean(terra::values(r), na.rm = TRUE)
  }, numeric(1))

  elev.lookup <- data.frame(ISOURBID   = elev.ids,
                            elev_mean_m = elev.means,
                            stringsAsFactors = FALSE)
  write.csv(elev.lookup, file.elev.lookup, row.names = FALSE)
  message("  Saved: ", file.elev.lookup, " (", nrow(elev.lookup), " cities)")
} else {
  message("Loading cached elevation lookup: ", file.elev.lookup)
}
elev.lookup <- read.csv(file.elev.lookup, stringsAsFactors = FALSE)

# ---- Step 1: Setup --------------------------------------------------------
sdei.df <- data.frame(terra::vect(path.shp))
sdei.df <- sdei.df[sdei.df$ES00POP >= 100e3 & sdei.df$SQKM_FINAL >= 1e2, ]

# City list: ERA5 daily CSVs available in v5
era5.files <- dir(path.v5, pattern = "_ERA5_daily\\.csv$")
era5.ids   <- sub("_ERA5_daily\\.csv$", "", era5.files)

# Restrict to cities with ERA5 data, elevation, and SDEI metadata
cities.use <- era5.ids[era5.ids %in% elev.lookup$ISOURBID &
                         era5.ids %in% sdei.df$ISOURBID]
message(length(cities.use), " cities ready for ET0 calculation.")

# Skip cities already processed (unless overwrite)
if (!overwrite) {
  done <- sub("_ERA5_ET0_daily\\.csv$", "",
              dir(path.v5, pattern = "_ERA5_ET0_daily\\.csv$"))
  cities.use <- cities.use[!cities.use %in% done]
  message(length(cities.use), " cities remaining after skipping completed.")
}
if (length(cities.use) == 0) stop("All cities complete. Set overwrite=TRUE to rerun.")

# Pre-allocate seasonal summary (appended each iteration, written at end)
summary.list <- vector("list", length(cities.use))

# ---- Step 2: Per-city loop ------------------------------------------------
pb <- txtProgressBar(min = 0, max = length(cities.use), style = 3)

for (i in seq_along(cities.use)) {
  setTxtProgressBar(pb, i)
  CITY <- cities.use[i]

  # -- Load ERA5 daily CSV
  dat <- read.csv(file.path(path.v5, paste0(CITY, "_ERA5_daily.csv")),
                  stringsAsFactors = FALSE)

  # Skip if missing critical columns
  req.cols <- c("tmax_C", "tmin_C", "tdew_C", "rshort_MJm2", "wind_ms",
                "press_kPA", "et_mm", "et_total_mm", "date", "year")
  if (!all(req.cols %in% names(dat))) {
    warning(CITY, ": missing required columns, skipping.")
    next
  }

  # -- Get city metadata
  lat    <- sdei.df$LATITUDE[sdei.df$ISOURBID == CITY]
  elev_m <- elev.lookup$elev_mean_m[elev.lookup$ISOURBID == CITY]

  # -- Day of year
  dat$doy <- lubridate::yday(as.Date(dat$date))

  # -- Calculate FAO-56 ET0 (daily)
  #    press_kPA supplied as press_actual → overrides altitude-formula pressure for gamma
  dat$ET0_mm <- calc_daily_et0(
    Tmax        = dat$tmax_C,
    Tmin        = dat$tmin_C,
    Tdew        = dat$tdew_C,
    Rs          = dat$rshort_MJm2,
    wind_10m    = dat$wind_ms,
    elev        = elev_m,
    lat         = lat,
    doy         = dat$doy,
    press_actual = dat$press_kPA
  )

  # -- Rename et_mm → et_transp_mm (vegetation transpiration; see file header note)
  dat$et_transp_mm <- dat$et_mm

  # -- Select and order output columns
  out.daily <- dat[, c("year", "date", "doy",
                       "tmax_C", "tmin_C", "tdew_C",
                       "rshort_MJm2", "wind_ms", "press_kPA",
                       "ET0_mm", "et_total_mm", "et_transp_mm")]
  out.daily$elev_m <- elev_m

  # -- Save per-city daily CSV
  write.csv(out.daily,
            file.path(path.v5, paste0(CITY, "_ERA5_ET0_daily.csv")),
            row.names = FALSE)

  # -- Aggregate to seasonal means per year
  yr.agg <- aggregate(
    cbind(ET0_mm, et_total_mm, et_transp_mm) ~ year,
    data    = dat,
    FUN     = mean,
    na.rm   = TRUE
  )
  yr.agg$ET0_total_season <- aggregate(ET0_mm ~ year, data = dat,
                                       FUN = sum, na.rm = TRUE)$ET0_mm
  yr.agg$n_days <- as.integer(table(dat$year[!is.na(dat$ET0_mm)]))

  names(yr.agg)[names(yr.agg) == "ET0_mm"]         <- "ET0_mean_daily"
  names(yr.agg)[names(yr.agg) == "et_total_mm"]    <- "et_total_mean_daily"
  names(yr.agg)[names(yr.agg) == "et_transp_mm"]   <- "et_transp_mean_daily"

  yr.agg$ISOURBID  <- CITY
  yr.agg$NAME      <- sdei.df$NAME[sdei.df$ISOURBID == CITY]
  yr.agg$LATITUDE  <- lat
  yr.agg$LONGITUDE <- sdei.df$LONGITUDE[sdei.df$ISOURBID == CITY]
  yr.agg$elev_m    <- elev_m

  # QA flags (soft — for transparency, not exclusion)
  mean_et0   <- mean(yr.agg$ET0_mean_daily,    na.rm = TRUE)
  mean_total <- mean(yr.agg$et_total_mean_daily, na.rm = TRUE)
  mean_transp <- mean(yr.agg$et_transp_mean_daily, na.rm = TRUE)

  yr.agg$flag_et0_outlier       <- mean_et0 < 0.5 | mean_et0 > 15
  yr.agg$flag_ratio             <- mean_total / mean_et0 > 1.0
  yr.agg$flag_transp_exceeds_total <- mean_transp > mean_total

  summary.list[[i]] <- yr.agg[, c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "elev_m",
                                   "year", "n_days",
                                   "ET0_mean_daily", "ET0_total_season",
                                   "et_total_mean_daily", "et_transp_mean_daily",
                                   "flag_et0_outlier", "flag_ratio",
                                   "flag_transp_exceeds_total")]
}
close(pb)

# ---- Step 3: Write summary outputs ----------------------------------------
summary.df <- do.call(rbind, summary.list[!sapply(summary.list, is.null)])

write.csv(summary.df, file.et0.summary, row.names = FALSE)
message("Seasonal summary saved: ", file.et0.summary)

# 20-year means per city
means.df <- aggregate(
  cbind(ET0_mean_daily, ET0_total_season, et_total_mean_daily, et_transp_mean_daily) ~ ISOURBID,
  data = summary.df,
  FUN  = mean,
  na.rm = TRUE
)
means.sd <- aggregate(
  cbind(ET0_mean_daily, et_total_mean_daily, et_transp_mean_daily) ~ ISOURBID,
  data = summary.df,
  FUN  = sd,
  na.rm = TRUE
)
names(means.sd)[-1] <- paste0(names(means.sd)[-1], "_sd")

# Merge in city metadata and flags
meta.cols <- unique(summary.df[, c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "elev_m")])
flag.cols <- aggregate(
  cbind(flag_et0_outlier, flag_ratio, flag_transp_exceeds_total) ~ ISOURBID,
  data = summary.df,
  FUN  = any
)

means20 <- Reduce(function(a, b) merge(a, b, by = "ISOURBID", all.x = TRUE),
                  list(meta.cols, means.df, means.sd, flag.cols))

write.csv(means20, file.et0.means, row.names = FALSE)
message("20-year means saved: ", file.et0.means)
message("Done.")


# ---- VERIFICATION (run manually on test cities after full run) -------------
# Cairo (EGY44702): desert, July.  Expect ET0 ~ 7-11 mm/day (high radiation, dry air)
# Amsterdam (NLD9694): temperate, July.  Expect ET0 ~ 3-5 mm/day
# Denver-area (USA26687): high-elevation (~1600 m).  Check elev_m in lookup matches ~1600 m
#
# Sanity check: ET0 should generally exceed et_total_mm (reference > actual, Kc < 1 on avg)
# Any city with flag_et0_outlier=TRUE or flag_ratio=TRUE warrants manual inspection.
#
# Quick single-city test:
# CITY <- "EGY44702"
# dat  <- read.csv(file.path(path.v5, paste0(CITY, "_ERA5_daily.csv")))
# lat  <- sdei.df$LATITUDE[sdei.df$ISOURBID == CITY]
# elev_m <- elev.lookup$elev_mean_m[elev.lookup$ISOURBID == CITY]
# dat$doy    <- lubridate::yday(as.Date(dat$date))
# dat$ET0_mm <- calc_daily_et0(dat$tmax_C, dat$tmin_C, dat$tdew_C,
#                              dat$rshort_MJm2, dat$wind_ms,
#                              elev=elev_m, lat=lat, doy=dat$doy,
#                              press_actual=dat$press_kPA)
# summary(dat$ET0_mm)
# summary(dat$et_total_mm)
