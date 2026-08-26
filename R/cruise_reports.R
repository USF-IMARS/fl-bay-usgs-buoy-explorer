source(here::here("R/ndbc_time.R"))
source(here::here("R/fetch_ndbc_observations.R"))

CRUISE_HISTORICAL_CACHE_ID <- "ndbc_buoys_historical"
CRUISE_SERIES_PAD_DAYS <- 3

read_cruise_checkpoints <- function(
    path = here::here("data", "keys_checkpoints_flat.csv")
) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      model_window_start = as.Date(model_window_start),
      model_window_end = as.Date(model_window_end)
    )
}

list_cruise_ids <- function(checkpoints = read_cruise_checkpoints()) {
  checkpoints |>
    dplyr::distinct(cruise_id, model_window_start) |>
    dplyr::arrange(model_window_start) |>
    dplyr::pull(cruise_id)
}

cruise_summary <- function(cruise_id_value, checkpoints = read_cruise_checkpoints()) {
  rows <- checkpoints |> dplyr::filter(.data$cruise_id == cruise_id_value)
  if (nrow(rows) == 0) {
    stop("No checkpoints found for cruise_id: ", cruise_id_value)
  }
  list(
    cruise_id = cruise_id_value,
    window_start = rows$model_window_start[1],
    window_end = rows$model_window_end[1],
    regions = sort(unique(rows$region)),
    stations = rows
  )
}

# great-circle distance in km between two lat/lon points (vectorized over the
# second point)
haversine_km <- function(lat1, lon1, lat2, lon2) {
  r_earth_km <- 6371
  to_rad <- function(x) x * pi / 180
  dlat <- to_rad(lat2 - lat1)
  dlon <- to_rad(lon2 - lon1)
  a <- sin(dlat / 2)^2 +
    cos(to_rad(lat1)) * cos(to_rad(lat2)) * sin(dlon / 2)^2
  2 * r_earth_km * asin(pmin(1, sqrt(a)))
}

# a representative lat/lon for each NDBC buoy, taken from its most recent
# reading over the lookback window
ndbc_station_locations <- function(stations = ndbc_stations, lookback_days = 60) {
  end_dt <- fetch_ndbc_erddap_end_time()
  start_time <- format(end_dt - lookback_days * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_time <- format(end_dt, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  rows <- lapply(stations, function(station_info) {
    df <- fetch_ndbc_timeseries(
      station_info,
      param = "sea_water_practical_salinity",
      start_time = start_time,
      end_time = end_time
    )
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    row <- df[which.max(df$time), ]
    dplyr::tibble(
      station_id = station_info$name,
      station = station_info$long_name,
      latitude = row$latitude,
      longitude = row$longitude
    )
  })
  dplyr::bind_rows(rows)
}

nearest_station_lookup <- function(lat, lon, station_locations) {
  d <- haversine_km(lat, lon, station_locations$latitude, station_locations$longitude)
  i <- which.min(d)
  dplyr::tibble(
    nearest_station_id = station_locations$station_id[i],
    nearest_station = station_locations$station[i],
    nearest_latitude = station_locations$latitude[i],
    nearest_longitude = station_locations$longitude[i],
    nearest_distance_km = d[i]
  )
}

# for each cruise checkpoint station, find the closest NDBC buoy
nearest_stations_for_cruise <- function(cruise_stations, station_locations) {
  matched <- dplyr::bind_rows(lapply(seq_len(nrow(cruise_stations)), function(i) {
    nearest_station_lookup(
      cruise_stations$latitude[i],
      cruise_stations$longitude[i],
      station_locations
    )
  }))
  dplyr::bind_cols(cruise_stations, matched)
}

# full NDBC time series (all buoys) spanning the cruise window (+/- padding),
# plus a single interpolation-ready snapshot nearest to local noon on the
# cruise's midpoint day
fetch_cruise_variable <- function(
    param,
    value_col,
    window_start,
    window_end,
    pad_days = CRUISE_SERIES_PAD_DAYS,
    stations = ndbc_stations
) {
  start_time <- format(
    as.POSIXct(window_start, tz = "UTC") - pad_days * 86400,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  end_time <- format(
    as.POSIXct(window_end, tz = "UTC") + (pad_days + 1) * 86400,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  all_series <- lapply(
    stations,
    fetch_ndbc_timeseries,
    param = param,
    start_time = start_time,
    end_time = end_time
  )

  midpoint_date <- window_start + floor(as.numeric(window_end - window_start) / 2)
  target_time <- as.POSIXct(
    sprintf("%s 12:00:00", format(midpoint_date)),
    tz = ndbc_display_tz
  )

  snapshot <- snapshot_ndbc_at_hour(all_series, target_time, value_col)

  list(
    series = dplyr::bind_rows(all_series),
    snapshot = snapshot,
    target_time = target_time
  )
}

# run (or fall back to a previously cached) DIVAnd interpolation for a
# cruise's snapshot observations
interpolate_or_load_cruise_field <- function(
    snapshot_obs,
    value_col,
    cruise_id_value,
    cache_id = CRUISE_HISTORICAL_CACHE_ID
) {
  suffix <- sprintf("_cruise_%s", cruise_id_value)
  field_file <- here::here(
    "data", "interpolated",
    sprintf("%s_field%s.csv", value_col, suffix)
  )

  if (is.null(snapshot_obs) || nrow(snapshot_obs) < 3) {
    if (file.exists(field_file)) {
      return(readr::read_csv(field_file, show_col_types = FALSE))
    }
    return(NULL)
  }

  tryCatch(
    interpolate_ndbc_field(
      snapshot_obs,
      value_col,
      suffix = suffix,
      cache_id = cache_id
    )$field,
    error = function(err) {
      warning(
        "Live interpolation failed for cruise ", cruise_id_value,
        "; using cached field if available. ", conditionMessage(err)
      )
      if (file.exists(field_file)) {
        readr::read_csv(field_file, show_col_types = FALSE)
      } else {
        NULL
      }
    }
  )
}

# assemble everything a cruise report template needs
build_cruise_report <- function(cruise_id_value) {
  checkpoints <- read_cruise_checkpoints()
  info <- cruise_summary(cruise_id_value, checkpoints)

  station_locations <- ndbc_station_locations()
  matched_stations <- nearest_stations_for_cruise(info$stations, station_locations)
  unique_buoys <- matched_stations |>
    dplyr::distinct(nearest_station_id, nearest_station, nearest_latitude, nearest_longitude) |>
    dplyr::arrange(nearest_station)

  salinity <- fetch_cruise_variable(
    "sea_water_practical_salinity", "salinity",
    info$window_start, info$window_end
  )
  temperature <- fetch_cruise_variable(
    "sea_water_temperature", "temperature",
    info$window_start, info$window_end
  )

  salinity_field <- interpolate_or_load_cruise_field(
    salinity$snapshot, "salinity", cruise_id_value
  )
  temperature_field <- interpolate_or_load_cruise_field(
    temperature$snapshot, "temperature", cruise_id_value
  )

  list(
    info = info,
    station_locations = station_locations,
    matched_stations = matched_stations,
    unique_buoys = unique_buoys,
    salinity = salinity,
    temperature = temperature,
    salinity_field = salinity_field,
    temperature_field = temperature_field
  )
}
