# Calibration script: verify daily aggregation logic for reanalysis-era5-land (hourly) data.
#
# ERA5-Land accumulated flux variables (precip, radiation, evaporation) are "d-accumulated"
# in ECMWF docs. For hourly reanalysis data, two interpretations exist:
#   Hypothesis A: each hourly value = 1-hour increment  => daily total = sum(24 values)
#   Hypothesis B: each hourly value = cumulative from midnight => daily total = max(24 values)
#
# Downloads total_precipitation hourly for 1 month/year (clipped to one city's region),
# computes both daily aggregations, and compares against an existing GEE CSV.
# Run BEFORE the full 01f_ERA_Formatting_CDS.R script.
#
# RESULT: whichever hypothesis lands on the 1:1 line vs GEE is the correct aggregation.
# Set accum_fun in 01f_ERA_Formatting_CDS.R accordingly.

if(!grepl("scripts", getwd())) setwd("scripts")
library(terra); library(ecmwfr); library(lubridate)

path.google      <- file.path("~/Google Drive/My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v5"
dir.out  <- file.path(path.google, GoogleFolderSave)
dir.temp <- file.path(dirname(getwd()), "data_raw", "era5_cds_temp")
dir.create(dir.temp, showWarnings=FALSE, recursive=TRUE)

# ---- Pick a NH city with an existing GEE CSV ----
done <- dir(dir.out, pattern="_ERA5_daily\\.csv$")
if(length(done) == 0) stop("No existing GEE CSVs found in ", dir.out)
cal_city <- sub("_ERA5_daily\\.csv$", "", done[1])
message("Calibrating against city: ", cal_city)

cal_year  <- 2015   # change if no 2015 data in GEE CSV
cal_month <- 7      # July — NH summer; change to 1 for SH city

# ---- Load city buffer ----
shp.path  <- "../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"
sdei.vect <- vect(shp.path)
sdei.vect <- sdei.vect[sdei.vect$ES00POP >= 100e3 & sdei.vect$SQKM_FINAL >= 1e2, ]
cal_buf   <- buffer(sdei.vect[sdei.vect$ISOURBID == cal_city, ], width=10000)

# CDS area format: c(N, W, S, E) with 1-degree pad so we capture all overlapping ERA5 pixels
e <- as.vector(ext(cal_buf))  # xmin, xmax, ymin, ymax
cal_area <- round(c(90, -180, -5, 180), 1)  # N, W, S, E

# ---- Download hourly total_precipitation (ONE request, small regional subset) ----
nc_cal <- file.path(dir.temp, sprintf("NH_cal_precip_hourly_%d_%02d.nc", cal_year, cal_month))

if(!file.exists(nc_cal)) {
  message("Submitting CDS request for hourly total_precipitation (~1 city region)...")
  wf_request(
    request = list(
      dataset_short_name = "reanalysis-era5-land",
      product_type       = "reanalysis",
      variable           = "total_precipitation",
      year               = as.character(cal_year),
      month              = sprintf("%02d", cal_month),
      day                = sprintf("%02d", 1:31),
      time               = sprintf("%02d:00", 0:23),
      area               = cal_area,          # regional subset keeps file small
      data_format        = "netcdf",
      download_format    = "unarchived",
      target             = basename(nc_cal)
    ),
    path    = dir.temp,
    verbose = TRUE
  )
} else {
  message("Using cached: ", basename(nc_cal))
}

# ---- Aggregate hourly -> daily, test both hypotheses ----
# Get the time values from the netdcf file separately than terra
install.packages("ncdf4")
testNC <- ncdf4::nc_open(nc_cal)
timeVals <- ncdf4::ncvar_get(testNC, "valid_time")  # seconds since 1970-01-01
ncdf4::nc_close(testNC)

vt <- as.POSIXct(timeVals, origin="1970-01-01", tz="UTC")

r_hourly <- terra::rast(nc_cal)
time(r_hourly) <- vt
message("Loaded ", nlyr(r_hourly), " hourly layers covering ",
        length(unique(as.Date(terra::time(r_hourly)))), " days")

dates <- as.Date(terra::time(r_hourly))

# Hypothesis A: each hourly value = 1-hour increment => sum 24 values = daily total
r_daily_A <- terra::tapp(r_hourly, dates, fun=sum)

# Hypothesis B: each hourly value = cumulative from midnight => last value = daily total
# max() is equivalent to last value for a monotonically increasing accumulation
r_daily_B <- terra::tapp(r_hourly, dates, fun=max)

# ---- Extract city spatial mean for both ----
extr <- function(r) {
  v <- terra::extract(r, cal_buf, fun=mean, na.rm=TRUE, ID=FALSE)
  # tapp names layers by the group index (dates as character); get actual dates
  data.frame(date=as.character(names(r)), value=as.numeric(v))
}

cds_A <- extr(r_daily_A); cds_A$precip_mm <- cds_A$value * 1e3   # m -> mm
cds_B <- extr(r_daily_B); cds_B$precip_mm <- cds_B$value * 1e3

# ---- Load GEE CSV and compare ----
gee_csv <- read.csv(file.path(dir.out, paste0(cal_city, "_ERA5_daily.csv")))
gee_mo  <- gee_csv[gee_csv$year == cal_year &
                   lubridate::month(as.Date(gee_csv$date)) == cal_month, ]

# Align dates — tapp may produce dates in a different format (e.g., "2015_07_01" or "1")
# Convert to common format for merging
cds_A$date <- as.character(as.Date(gsub("_", "-", cds_A$date)))
cds_B$date <- as.character(as.Date(gsub("_", "-", cds_B$date)))
gee_mo$date <- as.character(as.Date(gee_mo$date))

merged <- merge(gee_mo[, c("date","precip_mm")],
                cds_A[,  c("date","precip_mm")], by="date", suffixes=c(".gee",".A"))
merged <- merge(merged, cds_B[, c("date","precip_mm")], by="date")
names(merged)[ncol(merged)] <- "precip_mm.B"

message("\n--- Results for ", cal_city, ", ", cal_year, "-", sprintf("%02d", cal_month), " ---")
print(head(merged, 10))

slope <- function(x, y) round(coef(lm(y ~ x + 0))[1], 3)  # forced through origin
message("\nSlope vs GEE (want ~1.0):")
message("  Hyp A (sum of hourly * 1000): slope = ", slope(merged$precip_mm.gee, merged$precip_mm.A))
message("  Hyp B (max of hourly * 1000): slope = ", slope(merged$precip_mm.gee, merged$precip_mm.B))

# ---- Plot ----
lim <- range(c(merged$precip_mm.gee, merged$precip_mm.A, merged$precip_mm.B), na.rm=TRUE)
par(mfrow=c(1,2))
plot(merged$precip_mm.gee, merged$precip_mm.A, xlim=lim, ylim=lim,
     xlab="GEE precip_mm", ylab="CDS sum(hourly) * 1000 mm",
     main="Hyp A: sum"); abline(0, 1, col="red")
plot(merged$precip_mm.gee, merged$precip_mm.B, xlim=lim, ylim=lim,
     xlab="GEE precip_mm", ylab="CDS max(hourly) * 1000 mm",
     main="Hyp B: max"); abline(0, 1, col="red")
par(mfrow=c(1,1))

message("\nWhichever plot lands on the red 1:1 line is the correct daily aggregation.")
message("In 01f_ERA_Formatting_CDS.R, set:")
message("  accum_fun <- 'sum'   (if Hyp A is correct)")
message("  accum_fun <- 'max'   (if Hyp B is correct)")
