library(magrittr)
library(data.table)
library(ggplot2)

data <- dpkg::read_package(resources = c("702756-99999", "702756-26479", "702754-99999", "999999-26442", "702750-26442")) %>%
  cgr::rbind_tables(idcol = "station_id") %>%
  dpkg::unset() %>%
  data.table::as.data.table()
variables <- names(data) %>% extract(!grepl("_quality$|_uncertainty$|_period$|^station_id$|^t$", .))

# ---- Valdez Pioneer Field Airport ----

# Check that `702754-99999` fits into the gap in `702756-99999`.
# Verdict: YES

endpoints <- data[station_id == "702754-99999", .(from = min(t), to = max(t))]
nrow(data[station_id == "702756-99999" & t %between% endpoints, ..variables] %>% cgr::remove_empty_dimensions())

# Check that `702756-99999` and `702756-26479` are ~identical 2007-01-01 to 2007-12-31.
# Verdict: YES, but rainfall_1/2 has noise

# Timeline
endpoints <- as.POSIXct(c("2007-01-01", "2007-12-31"), tz = "UTC")
ranges <- cgr::tblranges(
  data[station_id %chin% c("702756-26479", "702756-99999") & t %between% endpoints, c("t", "station_id", variables), with = FALSE],
  time_col = "t",
  group_col = "station_id",
  maxdt = 60 * 60 * 24
)
cgr::timeline(ranges, name = "station_id", group = "variable", color = "gray40", width = 3)

# Scatter plots
breaks <- seq(min(endpoints), max(endpoints), by = 3600 * 24)
temp <- data[
  station_id %chin% c("702756-26479", "702756-99999")
][, 
  cgr::sample_interval(.SD, t, breaks, na.rm = TRUE), by = "station_id", .SDcols = variables
]

variable <- "air_temperature"
plot(temp[station_id == "702756-26479"][[variable]], temp[station_id == "702756-99999"][[variable]])
abline(0, 1)

temp %>%
  ggplot(aes_string("t", variable, color = "station_id")) +
  geom_line()

# ---- Valdez Weather Service Office ----

# Check that 999999-26442 directly precedes 702750-26442.
# Verdict: YES

data[station_id %chin% c("999999-26442", "702750-26442"), .(from = min(t), to = max(t)), by = station_id]

# ---- Airport vs. Weather Service Office ----

# Verdict: Similar but not identical. Transformation required.

endpoints <- as.POSIXct(c("2000-01-01", "2000-12-31"), tz = "UTC")
breaks <- seq(min(endpoints), max(endpoints), by = 3600 * 24)
temp <- data[
  station_id %chin% c("702750-26442", "702756-99999")
][
  , cgr::sample_interval(.SD, t, breaks, na.rm = TRUE), by = "station_id", .SDcols = variables
]

variable <- "air_temperature"
plot(temp[station_id == "702750-26442"][[variable]], temp[station_id == "702756-99999"][[variable]])
abline(0, 1)

temp %>%
  ggplot(aes_string("t", variable, color = "station_id")) +
  geom_line()

# Timeline
endpoints <- as.POSIXct(c("2005-01-01", "2007-12-31"), tz = "UTC")
ranges <- cgr::tblranges(
  data[station_id %chin% c("702756-26479", "702756-99999", "702750-26442") & t %between% endpoints, c("t", "station_id", variables), with = FALSE],
  time_col = "t",
  group_col = "station_id",
  maxdt = 60 * 60 * 24
)
cgr::timeline(ranges, name = "station_id", group = "variable", color = "gray40", width = 3)

# ---- Solar irradiance vs. Air temperature ----

# Verdict: Weak relationship

endpoints <- data[station_id == "702750-26442" & !is.na(solar_irradiance), .(from = min(t), to = max(t))]
breaks <- seq(endpoints$from, endpoints$to, by = 3600 * 24)
temp <- data[
  station_id %chin% c("702750-26442")
][
  , cgr::sample_interval(.SD, t, breaks), by = "station_id", .SDcols = variables
]

temp %>%
  ggplot(aes(air_temperature, solar_irradiance)) +
  geom_hex() +
  geom_smooth()

# ---- Spatial variability ----

dp <- dpkg::read_package()
dp$stations <- NULL
data <- dp %>%
  cgr::rbind_tables(idcol = "station_id") %>%
  dpkg::unset() %>%
  data.table::as.data.table()

variable <- "sea_surface_temperature"
endpoints <- as.POSIXct(c("2015-01-01", "2015-12-31"), tz = "UTC")

breaks <- seq(min(endpoints), max(endpoints), by = 3600 * 24)
temp <- data[
  , cgr::sample_interval(.SD, t, breaks), by = "station_id", .SDcols = variable
]
coverage <- temp[, lapply(.SD, function(x) {!all(is.na(x))}), by = "station_id", .SDcols = variable]
station_ids <- coverage$station_id[coverage[[2]]]

temp[station_id %chin% station_ids] %>%
  ggplot(aes_string("t", variable, color = "station_id")) +
  geom_line()

# ---- Temperature ----

temperatures <- list()
gap_start <- data[station_id == "702750-26442" & !is.na(air_temperature) & t < as.POSIXct("2006-01-01", tz = "UTC"), max(t)]
gap_end <- data[station_id == "702756-26479" & !is.na(air_temperature), min(t)]

# Valdez National Weather Service Office
temperatures$`999999-26442` <- data[station_id == "999999-26442"]
temperatures$`702750-26442` <- data[station_id == "702750-26442" & t <= gap_start]
# Valdez Pioneer Field Airport
temperatures$`702756-99999` <- data[station_id == "702756-99999" & t > gap_start & t < gap_end]
temperatures$`702756-26479` <- data[station_id == "702756-26479" & t >= gap_end]
# Merge together
temperatures %<>%
  data.table::rbindlist()
# Save to file
temperatures[, .(t, air_temperature)] %>%
  data.table::fwrite("~/desktop/temperatures.csv")

# cgr::tblranges(
#   temperatures,
#   time_col = "t",
#   group_col = "station_id",
#   maxdt = 60 * 60 * 24
# ) %>%
# cgr::timeline(name = "station_id", group = "variable", color = "gray40", width = 3)
