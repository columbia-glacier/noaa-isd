# ---- Install missing dependencies ----

packages <- c("rnoaa", "data.table")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
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
    x %<>% as_units(unit)
  }
  if (!missing(to_unit)) {
    x %<>% as_units(to_unit)
  }
  return(x)
}

isd_parsers <- list(
  t = function(date, time) {
    paste0(
      substr(date, 1, 4), "-",
      substr(date, 5, 6), "-",
      substr(date, 7, 8), "T",
      substr(time, 1, 2), ":",
      substr(time, 3, 4), "Z"
    )
  },
  wind_direction = function(wind_direction) {
    parse_isd_value(wind_direction, na = 999) %>%
      multiply_by(-1) %>%
      subtract(90) %>%
      mod(360) %>%
      convert_units(from = "°", to = "rad")
  },
  wind_speed <- function(wind_speed) {
    parse_isd_value(wind_speed, na = 9999, scale = 10, unit = "m/s")
  }
)

parse_expr <- expression(
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
  t = {
    paste0(
      substr(date, 1, 4), "-",
      substr(date, 5, 6), "-",
      substr(date, 7, 8), "T",
      substr(time, 1, 2), ":",
      substr(time, 3, 4), "Z"
    )
    # paste(date, time) %>%
    #   readr::parse_datetime("%Y%m%d %H%M") %>%
    #   format("%Y-%m-%dT%H:%MZ")
  },
  # WIND-OBSERVATION direction angle
  # The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing.
  # MIN: 001 MAX: 360 UNITS: Angular Degrees
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing. If type code (below) = V, then 999 indicates variable wind direction.
  wind_direction = {
    .$wind_direction %>%
      as.numeric() %>%
      replace(. == 999, NA) %>%
      multiply_by(-1) %>%
      subtract(90) %>%
      mod(360) %>%
      convert_units(from = "°", to = "rad")
  },
  wind_direction_quality = wind_direction_quality,
  # WIND-OBSERVATION speed rate
  # The rate of horizontal travel of air past a fixed point.
  # MIN: 0000 MAX: 0900 UNITS: meters per second SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  wind_speed = {
    wind_speed %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  wind_speed_quality = wind_speed_quality,
  # AIR-TEMPERATURE-OBSERVATION air temperature
  # The temperature of the air.
  # MIN: -0932 MAX: +0618 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  air_temperature = {
    temperature %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  air_temperature_quality = temperature_quality,
  # AIR-TEMPERATURE-OBSERVATION dew point temperature
  # The temperature to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur.
  # MIN: -0982 MAX: +0368 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  dew_point = {
    temperature_dewpoint %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  dew_point_quality = temperature_dewpoint_quality,
  # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
  # The air pressure relative to Mean Sea Level (MSL).
  # MIN: 08600 MAX: 10900 UNITS: Hectopascals
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 99999 = Missing.
  atmospheric_pressure = {
    air_pressure %>%
      as.numeric() %>%
      replace(. == 99999, NA) %>%
      divide_by(10) %>%
      convert_units(from = hPa, to = Pa)
  },
  atmospheric_pressure_quality = air_pressure_quality,
  # AA1-4
  # LIQUID-PRECIPITATION depth dimension
  # The depth of LIQUID-PRECIPITATION that is measured at the time of an observation. MIN: 0000 MAX: 9998 UNITS: millimeters
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  precipitation = {
    AA1_depth %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10) %>%
      convert_units(from = mm, to = m)
  },
  precipitation_1_quality = AA1_quality_code,
  # LIQUID-PRECIPITATION period quantity in hours
  # The quantity of time over which the LIQUID-PRECIPITATION was measured. MIN: 00 MAX: 98 UNITS: Hours
  # SCALING FACTOR: 1
  # DOM: A specific domain comprised of the characters in the ASCII character set
  # 99 = Missing.
  precipitation_period = {
    AA1_period_quantity_hrs %>%
      as.numeric() %>%
      replace(. == 99, NA) %>%
      convert_units(from = hr, to = s)
  },
  # AJ1
  # SNOW-DEPTH dimension
  # The depth of snow and ice on the ground.
  # MIN: 0000 MAX: 1200 UNITS: centimeters
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  snow_depth = {
    AJ1_depth_dimension %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      convert_units(from = cm, to = m)
  },
  snow_depth_quality = AJ1_quality_code,
  # SNOW-DEPTH equivalent water depth dimension
  # The depth of the liquid content of solid precipitation that has accumulated on the ground. MIN: 000000 MAX: 120000 UNITS: millimeters
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999999 = Missing.
  snow_depth_water = {
    AJ1_equivalent_water_depth %>%
      as.numeric() %>%
      replace(. == 999999, NA) %>%
      divide_by(10) %>%
      convert_units(from = mm, to = m)
  }
  snow_depth_water_quality = AJ1_equivalent_water_condition_quality_code,
  # CT1-3
  # AVG_TEMP air temperature
  # The average air temperature for a 5-minute period.
  # MIN: -9999 MAX: +9998 UNITS: degrees Celsius SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  air_temperature_5min = {
    CT1_average_air_temperature %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  air_temperature_5min_quality = CT1_average_air_temperature_quality_code,
  # GP1
  # Time period in minutes, for which the data in this section pertains—eg, 0060 = 60 minutes (1 hour). MIN: 0001 MAX: 9998 UNITS: Minutes
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  solar_irradiance_period = {
    GP1_time_period_min %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      convert_units(from = min, to = s)
  },
  # Modeled global horizontal
  # Total amount of direct and diffuse solar radiation (modeled) received on a horizontal surface. Unit is watts per square meter (W/m2) in whole values.
  # MIN: 0000 MAX: 9998 UNITS: watts per square meter SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  solar_irradiance = {
    GP1_modeled_global_horizontal %>%
      as.numeric() %>%
      replace(. == 9999, NA)
  },
  # Modeled global horizontal uncertainty
  # The uncertainty values are based on model type and quality of input data. MIN: 000 MAX: 100 UNITS: Percent
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing data
  solar_irradiance_uncertainty = {
    GP1_modeled_global_horizontal_uncertainty %>%
      as.numeric() %>%
      replace(. == 999, NA)
  },
  # SA1
  # SEA-SURFACE-TEMPERATURE-OBSERVATION temperature
  # The temperature of the water at the surface.
  # MIN: -050 MAX: +450 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters(0-9), a plus sign (+), and a minus
  # sign (-).
  # +999 = Missing
  water_temperature = {
    SA1_temp %>%
      as.numeric() %>%
      replace(. == 999, NA) %>%
      divide_by(10)
  },
  water_temperature_quality = SA1_quality
  # AL1 Snow accumulation
  # A01 Liquid precipitation
  # AP1-4 15 minute liquid precipitation
  # CB1 Subhourly Observed Liquid Precipitation
  # CH1 Hourly/Sub-Hourly Relative Humidity/Temperature Section
  # CI1 Hourly Relative Humidity/Temperature Section
  # CO1 UTC offset
  # CU1 Hourly Temperature Section
  # CV1 Hourly Temperature Extreme Section
  # CW1 Subhourly Wetness Section
  # GH1 Hourly Solar Radiation Section
  # GJ1 SUNSHINE-OBSERVATION
  # GK1 SUNSHINE-OBSERVATION
  # GM1 Solar Irradiance Section identifier
  # GN1 Solar Radiation
  # G01 Net Solar Radiation
  # KD1 HEATING-COOLING-DEGREE-DAYS
  # KF1 Hourly Calculated Temperature Section
  # RH1 RELATIVE HUMIDITY occurrence identifier
)

parse_isd_table <- function(.) {
  parser <- expression(
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
    t = {
      paste0(
        substr(.$date, 1, 4), "-",
        substr(.$date, 5, 6), "-",
        substr(.$date, 7, 8), "T",
        substr(.$time, 1, 2), ":",
        substr(.$time, 3, 4), "Z"
      )
      # paste(date, time) %>%
      #   readr::parse_datetime("%Y%m%d %H%M") %>%
      #   format("%Y-%m-%dT%H:%MZ")
    },
    # WIND-OBSERVATION direction angle
    # The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing.
    # MIN: 001 MAX: 360 UNITS: Angular Degrees
    # SCALING FACTOR: 1
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 999 = Missing. If type code (below) = V, then 999 indicates variable wind direction.
    wind_direction = {
      .$wind_direction %>%
        as.numeric() %>%
        replace(. == 999, NA) %>%
        multiply_by(-1) %>%
        subtract(90) %>%
        mod(360) %>%
        convert_units(from = "°", to = "rad")
    },
    wind_direction_quality = wind_direction_quality,
    # WIND-OBSERVATION speed rate
    # The rate of horizontal travel of air past a fixed point.
    # MIN: 0000 MAX: 0900 UNITS: meters per second SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 9999 = Missing.
    wind_speed = {
      wind_speed %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        divide_by(10)
    },
    wind_speed_quality = wind_speed_quality,
    # AIR-TEMPERATURE-OBSERVATION air temperature
    # The temperature of the air.
    # MIN: -0932 MAX: +0618 UNITS: Degrees Celsius
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
    # +9999 = Missing.
    air_temperature = {
      temperature %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        divide_by(10)
    },
    air_temperature_quality = temperature_quality,
    # AIR-TEMPERATURE-OBSERVATION dew point temperature
    # The temperature to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur.
    # MIN: -0982 MAX: +0368 UNITS: Degrees Celsius
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
    # +9999 = Missing.
    dew_point = {
      temperature_dewpoint %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        divide_by(10)
    },
    dew_point_quality = temperature_dewpoint_quality,
    # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
    # The air pressure relative to Mean Sea Level (MSL).
    # MIN: 08600 MAX: 10900 UNITS: Hectopascals
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 99999 = Missing.
    atmospheric_pressure = {
      air_pressure %>%
        as.numeric() %>%
        replace(. == 99999, NA) %>%
        divide_by(10) %>%
        convert_units(from = hPa, to = Pa)
    },
    atmospheric_pressure_quality = air_pressure_quality,
    # AA1-4
    # LIQUID-PRECIPITATION depth dimension
    # The depth of LIQUID-PRECIPITATION that is measured at the time of an observation. MIN: 0000 MAX: 9998 UNITS: millimeters
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 9999 = Missing.
    precipitation = {
      AA1_depth %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        divide_by(10) %>%
        convert_units(from = mm, to = m)
    },
    precipitation_1_quality = AA1_quality_code,
    # LIQUID-PRECIPITATION period quantity in hours
    # The quantity of time over which the LIQUID-PRECIPITATION was measured. MIN: 00 MAX: 98 UNITS: Hours
    # SCALING FACTOR: 1
    # DOM: A specific domain comprised of the characters in the ASCII character set
    # 99 = Missing.
    precipitation_period = {
      AA1_period_quantity_hrs %>%
        as.numeric() %>%
        replace(. == 99, NA) %>%
        convert_units(from = hr, to = s)
    },
    # AJ1
    # SNOW-DEPTH dimension
    # The depth of snow and ice on the ground.
    # MIN: 0000 MAX: 1200 UNITS: centimeters
    # SCALING FACTOR: 1
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 9999 = Missing.
    snow_depth = {
      AJ1_depth_dimension %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        convert_units(from = cm, to = m)
    },
    snow_depth_quality = AJ1_quality_code,
    # SNOW-DEPTH equivalent water depth dimension
    # The depth of the liquid content of solid precipitation that has accumulated on the ground. MIN: 000000 MAX: 120000 UNITS: millimeters
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 999999 = Missing.
    snow_depth_water = {
      AJ1_equivalent_water_depth %>%
        as.numeric() %>%
        replace(. == 999999, NA) %>%
        divide_by(10) %>%
        convert_units(from = mm, to = m)
    }
    snow_depth_water_quality = AJ1_equivalent_water_condition_quality_code,
    # CT1-3
    # AVG_TEMP air temperature
    # The average air temperature for a 5-minute period.
    # MIN: -9999 MAX: +9998 UNITS: degrees Celsius SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
    # +9999 = Missing.
    air_temperature_5min = {
      CT1_average_air_temperature %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        divide_by(10)
    },
    air_temperature_5min_quality = CT1_average_air_temperature_quality_code,
    # GP1
    # Time period in minutes, for which the data in this section pertains—eg, 0060 = 60 minutes (1 hour). MIN: 0001 MAX: 9998 UNITS: Minutes
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 9999 = Missing.
    solar_irradiance_period = {
      GP1_time_period_min %>%
        as.numeric() %>%
        replace(. == 9999, NA) %>%
        convert_units(from = min, to = s)
    },
    # Modeled global horizontal
    # Total amount of direct and diffuse solar radiation (modeled) received on a horizontal surface. Unit is watts per square meter (W/m2) in whole values.
    # MIN: 0000 MAX: 9998 UNITS: watts per square meter SCALING FACTOR: 1
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 9999 = Missing.
    solar_irradiance = {
      GP1_modeled_global_horizontal %>%
        as.numeric() %>%
        replace(. == 9999, NA)
    },
    # Modeled global horizontal uncertainty
    # The uncertainty values are based on model type and quality of input data. MIN: 000 MAX: 100 UNITS: Percent
    # SCALING FACTOR: 1
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 999 = Missing data
    solar_irradiance_uncertainty = {
      GP1_modeled_global_horizontal_uncertainty %>%
        as.numeric() %>%
        replace(. == 999, NA)
    },
    # SA1
    # SEA-SURFACE-TEMPERATURE-OBSERVATION temperature
    # The temperature of the water at the surface.
    # MIN: -050 MAX: +450 UNITS: Degrees Celsius
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters(0-9), a plus sign (+), and a minus
    # sign (-).
    # +999 = Missing
    water_temperature = {
      SA1_temp %>%
        as.numeric() %>%
        replace(. == 999, NA) %>%
        divide_by(10)
    },
    water_temperature_quality = SA1_quality
    # AL1 Snow accumulation
    # A01 Liquid precipitation
    # AP1-4 15 minute liquid precipitation
    # CB1 Subhourly Observed Liquid Precipitation
    # CH1 Hourly/Sub-Hourly Relative Humidity/Temperature Section
    # CI1 Hourly Relative Humidity/Temperature Section
    # CO1 UTC offset
    # CU1 Hourly Temperature Section
    # CV1 Hourly Temperature Extreme Section
    # CW1 Subhourly Wetness Section
    # GH1 Hourly Solar Radiation Section
    # GJ1 SUNSHINE-OBSERVATION
    # GK1 SUNSHINE-OBSERVATION
    # GM1 Solar Irradiance Section identifier
    # GN1 Solar Radiation
    # G01 Net Solar Radiation
    # KD1 HEATING-COOLING-DEGREE-DAYS
    # KF1 Hourly Calculated Temperature Section
    # RH1 RELATIVE HUMIDITY occurrence identifier
  )
}

parse_isd_table <- expression(
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
  t = {
    paste0(
      substr(date, 1, 4), "-",
      substr(date, 5, 6), "-",
      substr(date, 7, 8), "T",
      substr(time, 1, 2), ":",
      substr(time, 3, 4), "Z"
    )
    # paste(date, time) %>%
    #   readr::parse_datetime("%Y%m%d %H%M") %>%
    #   format("%Y-%m-%dT%H:%MZ")
  },
  # WIND-OBSERVATION direction angle
  # The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing.
  # MIN: 001 MAX: 360 UNITS: Angular Degrees
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing. If type code (below) = V, then 999 indicates variable wind direction.
  wind_direction = {
    wind_direction %>%
      as.numeric() %>%
      replace(. == 999, NA) %>%
      multiply_by(-1) %>%
      subtract(90) %>%
      mod(360) %>%
      convert_units(from = `°`, to = rad)
  },
  wind_direction_quality = wind_direction_quality,
  # WIND-OBSERVATION speed rate
  # The rate of horizontal travel of air past a fixed point.
  # MIN: 0000 MAX: 0900 UNITS: meters per second SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  wind_speed = {
    wind_speed %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  wind_speed_quality = wind_speed_quality,
  # AIR-TEMPERATURE-OBSERVATION air temperature
  # The temperature of the air.
  # MIN: -0932 MAX: +0618 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  air_temperature = {
    temperature %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  air_temperature_quality = temperature_quality,
  # AIR-TEMPERATURE-OBSERVATION dew point temperature
  # The temperature to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur.
  # MIN: -0982 MAX: +0368 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  dew_point = {
    temperature_dewpoint %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  dew_point_quality = temperature_dewpoint_quality,
  # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
  # The air pressure relative to Mean Sea Level (MSL).
  # MIN: 08600 MAX: 10900 UNITS: Hectopascals
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 99999 = Missing.
  atmospheric_pressure = {
    air_pressure %>%
      as.numeric() %>%
      replace(. == 99999, NA) %>%
      divide_by(10) %>%
      convert_units(from = hPa, to = Pa)
  },
  atmospheric_pressure_quality = air_pressure_quality,
  # AA1-4
  # LIQUID-PRECIPITATION depth dimension
  # The depth of LIQUID-PRECIPITATION that is measured at the time of an observation. MIN: 0000 MAX: 9998 UNITS: millimeters
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  precipitation = {
    AA1_depth %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10) %>%
      convert_units(from = mm, to = m)
  },
  precipitation_1_quality = AA1_quality_code,
  # LIQUID-PRECIPITATION period quantity in hours
  # The quantity of time over which the LIQUID-PRECIPITATION was measured. MIN: 00 MAX: 98 UNITS: Hours
  # SCALING FACTOR: 1
  # DOM: A specific domain comprised of the characters in the ASCII character set
  # 99 = Missing.
  precipitation_period = {
    AA1_period_quantity_hrs %>%
      as.numeric() %>%
      replace(. == 99, NA) %>%
      convert_units(from = hr, to = s)
  },
  # AJ1
  # SNOW-DEPTH dimension
  # The depth of snow and ice on the ground.
  # MIN: 0000 MAX: 1200 UNITS: centimeters
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  snow_depth = {
    AJ1_depth_dimension %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      convert_units(from = cm, to = m)
  },
  snow_depth_quality = AJ1_quality_code,
  # SNOW-DEPTH equivalent water depth dimension
  # The depth of the liquid content of solid precipitation that has accumulated on the ground. MIN: 000000 MAX: 120000 UNITS: millimeters
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999999 = Missing.
  snow_depth_water = {
    AJ1_equivalent_water_depth %>%
      as.numeric() %>%
      replace(. == 999999, NA) %>%
      divide_by(10) %>%
      convert_units(from = mm, to = m)
  }
  snow_depth_water_quality = AJ1_equivalent_water_condition_quality_code,
  # CT1-3
  # AVG_TEMP air temperature
  # The average air temperature for a 5-minute period.
  # MIN: -9999 MAX: +9998 UNITS: degrees Celsius SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
  # +9999 = Missing.
  air_temperature_5min = {
    CT1_average_air_temperature %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      divide_by(10)
  },
  air_temperature_5min_quality = CT1_average_air_temperature_quality_code,
  # GP1
  # Time period in minutes, for which the data in this section pertains—eg, 0060 = 60 minutes (1 hour). MIN: 0001 MAX: 9998 UNITS: Minutes
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  solar_irradiance_period = {
    GP1_time_period_min %>%
      as.numeric() %>%
      replace(. == 9999, NA) %>%
      convert_units(from = min, to = s)
  },
  # Modeled global horizontal
  # Total amount of direct and diffuse solar radiation (modeled) received on a horizontal surface. Unit is watts per square meter (W/m2) in whole values.
  # MIN: 0000 MAX: 9998 UNITS: watts per square meter SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 9999 = Missing.
  solar_irradiance = {
    GP1_modeled_global_horizontal %>%
      as.numeric() %>%
      replace(. == 9999, NA)
  },
  # Modeled global horizontal uncertainty
  # The uncertainty values are based on model type and quality of input data. MIN: 000 MAX: 100 UNITS: Percent
  # SCALING FACTOR: 1
  # DOM: A general domain comprised of the numeric characters (0-9).
  # 999 = Missing data
  solar_irradiance_uncertainty = {
    GP1_modeled_global_horizontal_uncertainty %>%
      as.numeric() %>%
      replace(. == 999, NA)
  },
  # SA1
  # SEA-SURFACE-TEMPERATURE-OBSERVATION temperature
  # The temperature of the water at the surface.
  # MIN: -050 MAX: +450 UNITS: Degrees Celsius
  # SCALING FACTOR: 10
  # DOM: A general domain comprised of the numeric characters(0-9), a plus sign (+), and a minus
  # sign (-).
  # +999 = Missing
  water_temperature = {
    SA1_temp %>%
      as.numeric() %>%
      replace(. == 999, NA) %>%
      divide_by(10)
  },
  water_temperature_quality = SA1_quality
  # AL1 Snow accumulation
  # A01 Liquid precipitation
  # AP1-4 15 minute liquid precipitation
  # CB1 Subhourly Observed Liquid Precipitation
  # CH1 Hourly/Sub-Hourly Relative Humidity/Temperature Section
  # CI1 Hourly Relative Humidity/Temperature Section
  # CO1 UTC offset
  # CU1 Hourly Temperature Section
  # CV1 Hourly Temperature Extreme Section
  # CW1 Subhourly Wetness Section
  # GH1 Hourly Solar Radiation Section
  # GJ1 SUNSHINE-OBSERVATION
  # GK1 SUNSHINE-OBSERVATION
  # GM1 Solar Irradiance Section identifier
  # GN1 Solar Radiation
  # G01 Net Solar Radiation
  # KD1 HEATING-COOLING-DEGREE-DAYS
  # KF1 Hourly Calculated Temperature Section
  # RH1 RELATIVE HUMIDITY occurrence identifier
)


temp <- apply_parser(meteo[[1]], noaa_isd_parser)

#' see: ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf
parse_noaa_isd <- function(.) {
  data.frame(
    stringsAsFactors = FALSE,
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
    t = {
      paste(.$date, .$time) %>%
        readr::parse_datetime("%Y%m%d %H%M") %>%
        format("%Y-%m-%dT%H:%MZ")
    },
    # WIND-OBSERVATION direction angle
    # The angle, measured in a clockwise direction, between true north and the direction from which the wind is blowing.
    # MIN: 001 MAX: 360 UNITS: Angular Degrees
    # SCALING FACTOR: 1
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 999 = Missing. If type code (below) = V, then 999 indicates variable wind direction.
    wind_direction = {
      wind_direction %>%
        replace(. == 999, NA) %>%
        multiply_by(-1) %>%
        subtract(90) %>%
        mod(360) %>%
        convert_units(from = `°`, to = rad)
    },
    wind_direction_quality = wind_direction_quality,
    # WIND-OBSERVATION speed rate
    # The rate of horizontal travel of air past a fixed point.
    # MIN: 0000 MAX: 0900 UNITS: meters per second SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 9999 = Missing.
    wind_speed = {
      wind_speed %>%
        replace(. == 999, NA) %>%
        divide_by(10)
    },
    wind_speed_quality = wind_speed_quality,
    # AIR-TEMPERATURE-OBSERVATION air temperature
    # The temperature of the air.
    # MIN: -0932 MAX: +0618 UNITS: Degrees Celsius
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
    # +9999 = Missing.
    air_temperature = {
      air_temperature %>%
        replace(. == 9999, NA) %>%
        divide_by(10)
    },
    air_temperature_quality = air_temperature_quality,
    # AIR-TEMPERATURE-OBSERVATION dew point temperature
    # The temperature to which a given parcel of air must be cooled at constant pressure and water vapor content in order for saturation to occur.
    # MIN: -0982 MAX: +0368 UNITS: Degrees Celsius
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9), a plus sign (+), and a minus sign (-).
    # +9999 = Missing.
    dew_point = {
      dew_point %>%
        replace(. == 9999, NA) %>%
        divide_by(10)
    },
    dew_point_quality = dew_point_quality,
    # ATMOSPHERIC-PRESSURE-OBSERVATION sea level pressure
    # The air pressure relative to Mean Sea Level (MSL).
    # MIN: 08600 MAX: 10900 UNITS: Hectopascals
    # SCALING FACTOR: 10
    # DOM: A general domain comprised of the numeric characters (0-9).
    # 99999 = Missing.
    pressure = {
      pressure %>%
        replace(. == 99999, NA) %>%
        divide_by(10) %>%
        convert_units(from = hPa, Pa)
    },
    pressure_quality = pressure_quality,
    # ...
    # AA1 Hourly to daily precipitation
    # AJ1 Snow depth
    # AL1 Snow accumulation
    # A01 Liquid precipitation
    # AP1-4 15 minute liquid precipitation
    # CB1 Subhourly Observed Liquid Precipitation
    # CH1 Hourly/Sub-Hourly Relative Humidity/Temperature Section
    # CI1 Hourly Relative Humidity/Temperature Section
    # CO1 UTC offset
    # CT1 Subhourly Temperature Section
    # CU1 Hourly Temperature Section
    # CV1 Hourly Temperature Extreme Section
    # CW1 Subhourly Wetness Section
    # GH1 Hourly Solar Radiation Section
    # GJ1 SUNSHINE-OBSERVATION
    # GK1 SUNSHINE-OBSERVATION
    # GM1 Solar Irradiance Section identifier
    # GN1 Solar Radiation
    # G01 Net Solar Radiation
    # GP1 Modeled Solar Irradiance
    # GQ1 Hourly Solar Angle
    # GR1 Hourly Extraterrestrial Radiation
    # KD1 HEATING-COOLING-DEGREE-DAYS
    # KF1 Hourly Calculated Temperature Section
    # MA1 ATMOSPHERIC-PRESSURE-OBSERVATION
    # OA1 SUPPLEMENTARY-WIND-OBSERVATION
    # OB1 Hourly/Sub-Hourly Wind Section
    # OD1 SUPPLEMENTARY-WIND-OBSERVATION
    # RH1 RELATIVE HUMIDITY occurrence identifier
    # SA1 SEA-SURFACE-TEMPERATURE-OBSERVATION

    # Precipitation (tenths of mm)
    precipitation = {
      prcp %>%
        divide_by(10) %>%
        convert_units(from = mm, to = m)
    },
    # Snowfall (mm)
    snowfall = {
      snow %>%
        convert_units(from = mm, to = m)
    },
    # # Water equivalent of snowfall (tenths of mm)
    # snowfall_water = {
    #   wesf %>%
    #     divide_by(10) %>%
    #     convert_units(from = mm, to = m)
    # },
    # Snow depth (mm)
    snow_depth = {
      snwd %>%
        convert_units(from = mm, to = m)
    },
    # Water equivalent of snow on the ground (tenths of mm)
    snow_depth_water = {
      wesd %>%
        divide_by(10) %>%
        convert_units(from = mm, to = m)
    },
    # Average temperature (tenths of degrees C)
    air_temperature_avg = {
      tavg %>%
        divide_by(10)
    },
    # Minimum temperature (tenths of degrees C)
    air_temperature_min = {
      tmin %>%
        divide_by(10)
    },
    # Maximum temperature (tenths of degrees C)
    air_temperature_max = {
      tmax %>%
        divide_by(10)
    }
    # # Daily total sunshine (minutes)
    # sunshine = {
    #   tsun %>%
    #     convert_units(from = min, to = s)
    # }
  )
}

# ---- Process station data ----

# Get station metadata
stations <- rnoaa::isd_stations_search(lat = 61.141482, lon = -147.075694, radius = 50)
# Get station data
meteo <- stations %>%
  apply(1, function(station) {
    years <- station %>%
      extract(c("begin", "end")) %>%
      substr(1, 4) %>%
      as.numeric() %>%
      {.[1]:.[2]}
    years %>%
      lapply(function(year) {
        tryCatch(
          rnoaa::isd(year, usaf = station["usaf"], wban = station["wban"], parallel = TRUE, progress = TRUE),
          error = function(e) {
            warning(e)
            data.frame()
          }
        )
      }) %>%
      data.table::rbindlist(fill = TRUE)
  })
# Write metadata to file
# Write data to file
