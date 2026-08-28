source(here::here("R/fetch_ndbc_history.R"))

# hourly precipitation + salinity for every buoy over a shared lookback
# window, so the two series can be compared directly
fetch_rainfall_salinity_history <- function(
    lookback_days = 60,
    stations = ndbc_stations
) {
  window <- ndbc_query_window(lookback_days * 24)

  precip_series <- lapply(
    stations,
    fetch_ndbc_timeseries,
    param = ndbc_precip_param,
    start_time = window$start_time,
    end_time = window$end_time
  )
  salinity_series <- lapply(
    stations,
    fetch_ndbc_timeseries,
    param = "sea_water_practical_salinity",
    start_time = window$start_time,
    end_time = window$end_time
  )

  list(
    precip = dplyr::bind_rows(precip_series),
    salinity = dplyr::bind_rows(salinity_series),
    start_time = window$start_time,
    end_time = window$end_time,
    data_as_of = window$data_as_of
  )
}

# total rainfall recorded at each buoy over the lookback window; stations
# with too few hourly readings (mostly-missing rain sensor) are dropped
# rather than shown as spuriously "dry"
station_cumulative_precip <- function(
    precip_df,
    value_col = "precip_mm",
    min_obs = 24
) {
  if (is.null(precip_df) || nrow(precip_df) == 0) {
    return(dplyr::tibble())
  }

  precip_df |>
    filter_ndbc_values(value_col) |>
    dplyr::group_by(.data$station_id, .data$station, .data$location, .data$latitude, .data$longitude) |>
    dplyr::summarize(
      cumulative_mm = sum(.data[[value_col]], na.rm = TRUE),
      n_obs = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_obs >= min_obs)
}

# network mean daily rainfall: each station's daily total, averaged across
# stations reporting that day (mirrors network_daily_means() for salinity)
network_daily_precip <- function(precip_df, value_col = "precip_mm", tz = ndbc_display_tz) {
  if (is.null(precip_df) || nrow(precip_df) == 0) {
    return(dplyr::tibble())
  }

  precip_df |>
    filter_ndbc_values(value_col) |>
    dplyr::mutate(date = as.Date(.data$time, tz = tz)) |>
    dplyr::group_by(.data$station_id, .data$date) |>
    dplyr::summarize(daily_total = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(.data$date) |>
    dplyr::summarize(
      mean_daily_mm = mean(.data$daily_total, na.rm = TRUE),
      n_stations = dplyr::n(),
      .groups = "drop"
    )
}

# per-buoy cumulative rainfall paired with that buoy's latest salinity
# reading, for testing whether locally wetter buoys run fresher
rainfall_salinity_by_station <- function(cumulative_precip, latest_salinity) {
  if (nrow(cumulative_precip) == 0 || nrow(latest_salinity) == 0) {
    return(dplyr::tibble())
  }

  cumulative_precip |>
    dplyr::inner_join(
      latest_salinity |> dplyr::select(station_id, salinity),
      by = "station_id"
    )
}
