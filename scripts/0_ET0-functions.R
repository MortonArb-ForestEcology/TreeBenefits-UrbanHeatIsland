# Notes: 
# -- These should be mostly derived from FAO-56; equations 39 and 36;
# -- I admittedly used Google Gemini to help scrape them and format them, but they match my understanding/recollection having looked at the actual doc


#' Calculate Solar Geometry (Ra and Rso)
#' @param lat Latitude in decimal degrees
#' @param doy Day of year (1-365)
#' @param elev Elevation in meters
calc_solar_geom <- function(lat, doy, elev) {
  lat_rad <- (pi / 180) * lat
  dr <- 1 + 0.033 * cos(2 * pi * doy / 365)
  delta <- 0.409 * sin((2 * pi * doy / 365) - 1.39)
  
  # Sunset hour angle
  arg <- -tan(lat_rad) * tan(delta)
  omega_s <- acos(pmin(pmax(arg, -1), 1))
  
  # Extraterrestrial Radiation (Ra)
  Gsc <- 0.0820 # Solar constant
  Ra <- (24 * 60 / pi) * Gsc * dr * (
    omega_s * sin(lat_rad) * sin(delta) + 
      cos(lat_rad) * cos(delta) * sin(omega_s)
  )
  
  # Clear-sky Radiation (Rso)
  Rso <- (0.75 + (2e-05 * elev)) * Ra
  
  return(list(Ra = Ra, Rso = Rso))
}

#' Master FAO-56 Reference ET Function
#' @param Tmax,Tmin,Tdew Temperature and Dewpoint in Celsius
#' @param Rs Incoming solar radiation (MJ/m2/day)
#' @param wind_10m Wind speed at 10m height (m/s)
#' @param elev Elevation in meters
#' @param lat Latitude in decimal degrees
#' @param doy Day of year (1-365)
calc_daily_et0 <- function(Tmax, Tmin, Tdew, Rs, wind_10m, elev, lat, doy) {
  
  # 1. Psychrometric & Pressure
  pres <- 101.3 * ((293 - 0.0065 * elev) / 293)^5.26
  gamma <- 0.000665 * pres
  
  # 2. Vapor Pressure (ea and es)
  ea <- 0.6108 * exp((17.27 * Tdew) / (Tdew + 237.3))
  es_tmax <- 0.6108 * exp((17.27 * Tmax) / (Tmax + 237.3))
  es_tmin <- 0.6108 * exp((17.27 * Tmin) / (Tmin + 237.3))
  es <- (es_tmax + es_tmin) / 2
  vpd <- es - ea
  
  # 3. Slope of Vapor Pressure Curve (Delta)
  Tavg <- (Tmax + Tmin) / 2
  delta <- (4098 * (0.6108 * exp((17.27 * Tavg) / (Tavg + 237.3)))) / (Tavg + 237.3)^2
  
  # 4. Wind Speed scaling (10m to 2m)
  u2 <- wind_10m * (4.87 / log(67.8 * 10 - 5.42))
  
  # 5. Net Radiation (Rn)
  solar <- calc_solar_geom(lat, doy, elev)
  Rns <- (1 - 0.23) * Rs # Fixed albedo of 0.23
  
  # Net Longwave (Rnl)
  sigma <- 4.903e-09
  rel_rs <- pmin(Rs / solar$Rso, 1)
  Rnl <- sigma * ((Tmax + 273.16)^4 + (Tmin + 273.16)^4) / 2 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * rel_rs - 0.35)
  
  Rn <- Rns - Rnl
  
  # 6. Final Penman-Monteith (Equation 6 in FAO-56)
  G <- 0 # Soil heat flux (ignored for daily/summer steps per FAO)
  
  numerator <- 0.408 * delta * (Rn - G) + gamma * (900 / (Tavg + 273)) * u2 * vpd
  denominator <- delta + gamma * (1 + 0.34 * u2)
  
  et0 <- numerator / denominator
  return(pmax(et0, 0)) # Ensure no negative ET
}

# Function to calculate humidity variables from Dewpoint and Air Temp
# T: Air Temperature (Celsius)
# Td: Dewpoint Temperature (Celsius)
# P: Atmospheric Pressure (kPa) - optional, for Specific Humidity only

calc_humidity <- function(T, Td, P = 101.3) {
  
  # 1. Actual Vapor Pressure (ea) - FAO-56 Eq. 14
  # This is the most important one for ET0
  ea <- 0.6108 * exp((17.27 * Td) / (Td + 237.3))
  
  # 2. Saturation Vapor Pressure (es) - FAO-56 Eq. 13
  # Use air temperature T to see how much water the air *could* hold
  es <- 0.6108 * exp((17.27 * T) / (T + 237.3))
  
  # 3. Relative Humidity (RH) as a percentage
  rh <- 100 * (ea / es)
  
  # 4. Specific Humidity (q) - approx calculation
  # q = (0.622 * ea) / (P - (0.378 * ea))
  q <- (0.622 * ea) / (P - (0.378 * ea))
  
  return(list(ea = ea, es = es, rh = rh, q = q))
}