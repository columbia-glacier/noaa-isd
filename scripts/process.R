library(magrittr)

# ---- Load functions ----

#' @examples
#' parse_isd_value(x = c("999", "10"), na = 999, scale = 10, unit = "m", to_unit = "mm")
parse_isd_value <- function(x, class = "numeric", na, scale = 1, unit, to_unit) {
  x %<>%
    as(class) %>%
    replace(. == na, NA) %>%
    divide_by(scale)
  if (!missing(unit)) {
    x %<>% units2::as_units(unit)
  }
  if (!missing(to_unit)) {
    x %<>% units2::as_units(to_unit)
  }
  return(x)
}

# ---- Load field parsers ----

isd_station_parser <- list(
  id = function(usaf, wban) {
    paste(usaf, wban, sep = "-") %>%
      dpkg::set_field(description = "Station identifier (USAF-WBAN)")
  },
  name = function(station_name) {
    station_name %>%
      dpkg::set_field(description = "Station name")
  },
  longitude = function(lon) {
    lon %>%
      units2::as_units(unit = "°") %>%
      dpkg::set_field(description = "Longitude (unknown datum)")
  },
  latitude = function(lat) {
    lat %>%
      units2::as_units(unit = "°") %>%
      dpkg::set_field(description = "Latitutde (unknown datum)")
  },
  elevation = function(elev_m) {
    elev_m %>%
      units2::as_units(unit = "m") %>%
      dpkg::set_field(description = "Height above Mean Sea Level (MSL)")
  }
)

isd_data_parser <- list(
  # GEOPHYSICAL-POINT-OBSERVATION date
  # The date of a GEOPHYSICAL-POINT-OBSERVATION.
  # MIN: 00000101 MAX: 99991231
  # DOM: A general domain comprised of integer values 0-9 in the format YYYYMMDD.
  # YYYY can be any positive integer value; MM is restricted to values 01-12; and DD is restricted to values 01-31.
  # GEOPHYSICAL-POINT-OBSERVATION time
  # The time of a GEOPHYSICAL-POINT-OBSERVATION based on
  # Coordinated Universal Time Code (UTC).
  # MIN: 0000 MAX: 2359
  # DOM: A general domain comprised of integer values 0-9 in the format HHMM. HH is restricted to values 00-23; MM is restricted to values 00-59.
  t = function(date, time) {
    paste0(
      substr(date, 1, 4), "-",
      substr(date, 5, 6), "-",
      substr(date, 7, 8), "T",
      substr(time, 1, 2), ":",
      substr(time, 3, 4), "Z"
    ) %>%
      dpkg::set_field(type = "datetime", format = "%Y-%m-%dT%H:%MZ")
  },
  # WIND-OBSERVATION direction angle
  # The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing.
  # MIN: 001 MAX: 360 UNITS: Angular Degrees
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing. If type code (below) = V, then 999 indicates variable wind direction.
  wind_direction = function(wind_direction) {
    wind_direction %>%
      parse_isd_value(na = 999) %>%
      multiply_by(-1) %>%
      subtract(90) %>%
      mod(360) %>%
      units2::convert_units(from = "°", to = "rad") %>%
      dpkg::set_field(description = "Wind direction (direction of travel counterclockwise from east)")
  },
  wind_direction_quality = function(wind_direction_quality) {
    wind_direction_quality
  },
  # WIND-OBSERVATION speed rate
  # The rate of horizontal travel of air past a fixed point.
  # MIN: 0000 MAX: 0900 UNITS: meters per second SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  wind_speed = function(wind_speed) {
    wind_speed %>%
      parse_isd_value(na = 9999, scale = 10, unit = "m/s")
  },
  wind_speed_quality = function(wind_speed_quality) {
    wind_speed_quality
  },
  # AIR-TEMPERATURE-OBSERVATION air temperature
  # The temperature of the air.
  # MIN: -0932 MAX: +0618 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  air_temperature = function(temperature) {
    temperature %>%
      parse_isd_value(na = 9999, scale = 10, unit = "°C")
  },
  air_temperature_quality = function(temperature_quality) {
    temperature_quality
  },
  # AIR-TEMPERATURE-OBSERVATION dew point temperature
  # The temperature to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur.
  # MIN: -0982 MAX: +0368 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  dew_point = function(temperature_dewpoint) {
    temperature_dewpoint %>%
      parse_isd_value(na = 9999, scale = 10, unit = "°C")
  },
  dew_point_quality = function(temperature_dewpoint_quality) {
    temperature_dewpoint_quality
  },
  # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
  # The air pressure relative to Mean Sea Level (MSL).
  # MIN: 08600 MAX: 10900 UNITS: Hectopascals
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 99999 = Missing.
  air_pressure_at_sea_level = function(air_pressure) {
    air_pressure %>%
      parse_isd_value(na = 99999, scale = 10, unit = "hPa", to_unit = "Pa")
  },
  air_pressure_at_sea_level_quality = function(air_pressure_quality) {
    air_pressure_quality
  },
  # AA1-4
  # LIQUID-PRECIPITATION depth dimension
  # The depth of LIQUID-PRECIPITATION that is measured at the time of an observation. MIN: 0000 MAX: 9998 UNITS: millimeters
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  rainfall_1 = function(AA1_depth) {
    AA1_depth %>%
      parse_isd_value(na = 9999, scale = 10, unit = "mm", to_unit = "m")
  },
  rainfall_1_quality = function(AA1_quality_code) {
    AA1_quality_code
  },
  # LIQUID-PRECIPITATION period quantity in hours
  # The quantity of time over which the LIQUID-PRECIPITATION was measured. MIN: 00 MAX: 98 UNITS: Hours
  # SCALING FACTOR: 1
  # DOM: A specific domain comprised of the characters in the ASCII character set
  # 99 = Missing.
  rainfall_1_period = function(AA1_period_quantity_hrs) {
    AA1_period_quantity_hrs %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  # AA2
  rainfall_2 = function(AA2_depth) {
    AA2_depth %>%
      parse_isd_value(na = 9999, scale = 10, unit = "mm", to_unit = "m")
  },
  rainfall_2_quality = function(AA2_quality_code) {
    AA2_quality_code
  },
  rainfall_2_period = function(AA2_period_quantity_hrs) {
    AA2_period_quantity_hrs %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  # AA3
  rainfall_3 = function(AA3_depth) {
    AA3_depth %>%
      parse_isd_value(na = 9999, scale = 10, unit = "mm", to_unit = "m")
  },
  rainfall_3_quality = function(AA3_quality_code) {
    AA3_quality_code
  },
  rainfall_3_period = function(AA3_period_quantity_hrs) {
    AA3_period_quantity_hrs %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  # AA4
  rainfall_4 = function(AA4_depth) {
    AA4_depth %>%
      parse_isd_value(na = 9999, scale = 10, unit = "mm", to_unit = "m")
  },
  rainfall_4_quality = function(AA4_quality_code) {
    AA4_quality_code
  },
  rainfall_4_period = function(AA4_period_quantity_hrs) {
    AA4_period_quantity_hrs %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  # AJ1
  # SNOW-DEPTH dimension
  # The depth of snow and ice on the ground.
  # MIN: 0000 MAX: 1200 UNITS: centimeters
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  snow_thickness = function(AJ1_depth_dimension) {
    AJ1_depth_dimension %>%
      parse_isd_value(na = 9999, scale = 1, unit = "cm", to_unit = "m")
  },
  snow_thickness_quality = function(AJ1_quality_code) {
    AJ1_quality_code
  },
  # SNOW-DEPTH equivalent water depth dimension
  # The depth of the liquid content of solid precipitation that has accumulated on the ground. MIN: 000000 MAX: 120000 UNITS: millimeters
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999999 = Missing.
  snow_thickness_lwe = function(AJ1_equivalent_water_depth) {
    AJ1_equivalent_water_depth %>%
      parse_isd_value(na = 999999, scale = 10, unit = "mm", to_unit = "m")
  },
  snow_thickness_lwe_quality = function(AJ1_equivalent_water_condition_quality_code) {
    AJ1_equivalent_water_condition_quality_code
  },
  # AL1-4 SNOW-ACCUMULATION
  # SNOW-ACCUMULATION period quantity
  # The quantity of time over which the SNOW-ACCUMULATION occurred.
  # MIN: 00 MAX: 72 UNITS: Hours
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the characters in the ASCII character set.
  # 99 = Missing.
  snowfall_1_period = function(AL1_period_quantity) {
    AL1_period_quantity %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  # SNOW-ACCUMULATION depth dimension
  # The depth of a SNOW-ACCUMULATION.
  # MIN: 000 MAX: 500 UNITS: centimeters
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing.
  snowfall_1 = function(AL1_depth_dimension) {
    AL1_depth_dimension %>%
      parse_isd_value(na = 999, scale = 1, unit = "cm", to_unit = "m")
  },
  snowfall_1_quality = function(AL1_quality_code) {
    AL1_quality_code
  },
  # AL2
  snowfall_2_period = function(AL2_period_quantity) {
    AL2_period_quantity %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  snowfall_2 = function(AL2_depth_dimension) {
    AL2_depth_dimension %>%
      parse_isd_value(na = 999, scale = 1, unit = "cm", to_unit = "m")
  },
  snowfall_2_quality = function(AL2_quality_code) {
    AL2_quality_code
  },
  # AL3
  snow_accumulation_3_period = function(AL3_period_quantity) {
    AL3_period_quantity %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  snowfall_3 = function(AL3_depth_dimension) {
    AL3_depth_dimension %>%
      parse_isd_value(na = 999, scale = 1, unit = "cm", to_unit = "m")
  },
  snowfall_3_quality = function(AL3_quality_code) {
    AL3_quality_code
  },
  # AL4
  snowfall_4_period = function(AL4_period_quantity) {
    AL4_period_quantity %>%
      parse_isd_value(na = 99, scale = 1, unit = "hr", to_unit = "s")
  },
  snowfall_4 = function(AL4_depth_dimension) {
    AL4_depth_dimension %>%
      parse_isd_value(na = 999, scale = 1, unit = "cm", to_unit = "m")
  },
  snowfall_4_quality = function(AL4_quality_code) {
    AL4_quality_code
  },
  # GP1
  # Time period in minutes, for which the data in this section pertains—eg, 0060 = 60 minutes (1 hour). MIN: 0001 MAX: 9998 UNITS: Minutes
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  solar_irradiance_period = function(GP1_time_period_min) {
    GP1_time_period_min %>%
      parse_isd_value(na = 9999, scale = 1, unit = "min", to_unit = "s")
  },
  # Modeled global horizontal
  # Total amount of direct and diffuse solar radiation (modeled) received on a horizontal surface. Unit is watts per square meter (W/m2) in whole values.
  # MIN: 0000 MAX: 9998 UNITS: watts per square meter SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  solar_irradiance = function(GP1_modeled_global_horizontal) {
    GP1_modeled_global_horizontal %>%
      parse_isd_value(na = 9999, scale = 1, unit = "W/m^2") %>%
      dpkg::set_field(description = "Direct and diffuse solar radiation received on a horizontal surface (modeled)")
  },
  # Modeled global horizontal uncertainty
  # The uncertainty values are based on model type and quality of input data. MIN: 000 MAX: 100 UNITS: Percent
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing data
  solar_irradiance_uncertainty = function(GP1_modeled_global_horizontal_uncertainty) {
    GP1_modeled_global_horizontal_uncertainty %>%
      parse_isd_value(na = 999, scale = 1, unit = "%")
  },
  # SA1
  # SEA-SURFACE-TEMPERATURE-OBSERVATION temperature
  # The temperature of the water at the surface.
  # MIN: -050 MAX: +450 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters(0-9), a plus sign (+), and a minus
  # sign (-).
  # +999 = Missing
  sea_surface_temperature = function(SA1_temp) {
    SA1_temp %>%
      parse_isd_value(na = 999, scale = 10, unit = "°C")
  },
  sea_surface_temperature_quality = function(SA1_quality) {
    SA1_quality
  }
)

# ---- Download station data ----

station_ids <- c(
  "999999-26442", "702750-26442", # Weather Service Office
  "702756-99999", "702756-26479", "702754-99999", # Pioneer Field Airport
  "998271-99999", # Valdez (buoy)
  "994660-99999", # Potato Point (buoy)
  "994670-99999", # Middle Rock Light (buoy)
  "994680-99999", # Bligh Reef Light (buoy)
  "997203-99999", # Western Prince William Sound (buoy)
  "992850-99999" # West Orca Bay (buoy)
)

# Get station metadata
stations <- rnoaa::isd_stations_search(lat = 61.141482, lon = -147.075694, radius = 100)
stations$id <- paste0(stations$usaf, "-", stations$wban)
stations <- stations[stations$id %in% station_ids, ]

# Download station data
for (station_id in station_ids) {
  station <- stations[stations$id == station_id, ]
  years <- station %>%
    extract(c("begin", "end")) %>%
    substr(1, 4) %>%
    as.numeric() %>%
    {.[1]:.[2]}
  for (year in years) {
    basename <- paste0(station["id"], "-", year, ".rds")
    file <- file.path("archive", basename)
    if (!file.exists(file)) {
      temp <- try(
        rnoaa::isd(year, usaf = station["usaf"], wban = station["wban"], parallel = TRUE, progress = TRUE),
        silent = TRUE
      )
      file.copy(from = file.path(rappdirs::user_cache_dir("rnoaa/isd"), basename), to = file)
    }
  }
}

# ---- Load station data ----

# Load station metadata
files <- file.path("archive") %>%
  list.files(pattern = "[0-9]{6}\\-[0-9]{5}\\-[0-9]{4}\\.rds", full.names = TRUE)
station_ids <- regexpr("[0-9]{6}\\-[0-9]{5}", files) %>%
  regmatches(files, .) %>%
  unique()
stations <- rnoaa::isd_stations() %>%
  dplyr::mutate(
    id = paste(usaf, wban, sep = "-")
  ) %>%
  dplyr::filter(
    id %in% station_ids
  )

# Load and parse station data
parsed <- list()
for (station_id in station_ids) {
  cat(".")
  parsed[[station_id]] <- files %>%
    extract(grepl(station_id, .)) %>%
    lapply(readRDS) %>%
    cgr::rbind_tables() %>%
    cgr::remove_empty_dimensions(ignore = "t") %>%
    cgr::parse_table(isd_data_parser)
}

# ---- Package and write to file ----

# Set resources
for (i in seq_along(parsed)) {
  station_id <- names(parsed)[i]
  station_name <- stations$station_name[stations$id == station_id]
  parsed[[i]] %<>%
    dpkg::set_resource(
      title = paste0(station_name, " (", station_id, ")"),
      path = file.path("data", paste0(station_id, ".csv"))
    )
}

# Append stations
station_dr <- stations %>%
  cgr::parse_table(isd_station_parser) %>%
  dpkg::set_resource(
    name = "stations",
    path = "data/stations.csv",
    title = "Station metadata"
  )
parsed %<>% c(list(station_dr), .)

# Set package
parsed %<>%
  dpkg::set_package(
    name = "noaa-isd",
    title = "NOAA Integrated Surface Database (ISD)",
    description = "Meteorological observations from nearby weather stations.",
    version = "0.1.0",
    contributors = list(
      dpkg::contributor(title = "Ethan Welty", email = "ethan.welty@gmail.com", role = "author")
    ),
    sources = list(
      dpkg::source(title = "NOAA Integrated Surface Database FTP Access", path = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/")
    )
  )

# Write package
dpkg:::write_package(parsed)
