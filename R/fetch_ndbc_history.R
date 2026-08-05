source("R/fetch_ndbc_observations.R")

ndbc_param_specs <- list(
  salinity = list(
    param = "sea_water_practical_salinity",
    value_col = "salinity",
    label = "Salinity",
    unit = "PSU"
  ),
  temperature = list(
    param = "sea_water_temperature",
    value_col = "temperature",
    label = "Temperature",
    unit = "°C"
  )
)

fetch_ndbc_network_history <- function(
    lookback_days = 365,
    stations = ndbc_stations
) {
  window <- ndbc_query_window(lookback_days * 24)
  lapply(names(ndbc_param_specs), function(var_key) {
    spec <- ndbc_param_specs[[var_key]]
    series <- lapply(
      stations,
      fetch_ndbc_timeseries,
      param = spec$param,
      start_time = window$start_time,
      end_time = window$end_time
    )
    dplyr::bind_rows(series)
  }) |>
    stats::setNames(names(ndbc_param_specs))
}

ndbc_valid_range <- function(value_col) {
  switch(
    value_col,
    salinity = c(5, 55),
    temperature = c(10, 40),
    c(-Inf, Inf)
  )
}

filter_ndbc_values <- function(df, value_col) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  rng <- ndbc_valid_range(value_col)
  df |>
    dplyr::filter(
      !is.na(.data[[value_col]]),
      is.finite(.data[[value_col]]),
      abs(.data[[value_col]]) < 1e3,
      .data[[value_col]] >= rng[1],
      .data[[value_col]] <= rng[2]
    )
}

prepare_ndbc_history <- function(df, value_col, tz = ndbc_display_tz) {
  if (is.null(df) || nrow(df) == 0) {
    return(dplyr::tibble())
  }

  df |>
    filter_ndbc_values(value_col) |>
    dplyr::mutate(
      time = as.POSIXct(time, tz = "UTC"),
      date = as.Date(time, tz = tz),
      doy = as.integer(format(time, "%j", tz = tz))
    )
}

network_daily_means <- function(df, value_col) {
  df |>
    dplyr::group_by(.data$station_id, .data$station, .data$date, .data$doy) |>
    dplyr::summarize(
      value = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$date, .data$doy) |>
    dplyr::summarize(
      value = mean(.data$value, na.rm = TRUE),
      n_stations = dplyr::n(),
      .groups = "drop"
    )
}

doy_climatology <- function(df, value_col, probs = c(0.1, 0.5, 0.9)) {
  if (nrow(df) == 0) {
    return(dplyr::tibble())
  }

  daily <- df |>
    dplyr::group_by(.data$station_id, .data$date, .data$doy) |>
    dplyr::summarize(
      value = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )

  daily |>
    dplyr::group_by(.data$doy) |>
    dplyr::summarize(
      p10 = stats::quantile(.data$value, probs[1], na.rm = TRUE),
      p50 = stats::quantile(.data$value, probs[2], na.rm = TRUE),
      p90 = stats::quantile(.data$value, probs[3], na.rm = TRUE),
      .groups = "drop"
    )
}

latest_ndbc_readings <- function(history_df, value_col) {
  if (nrow(history_df) == 0) {
    return(dplyr::tibble())
  }

  history_df |>
    dplyr::group_by(.data$station_id, .data$station, .data$location) |>
    dplyr::slice_max(.data$time, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

station_context_summary <- function(history_df, latest_df, value_col) {
  if (nrow(latest_df) == 0) {
    return(dplyr::tibble())
  }

  latest_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      history = list(
        history_df |>
          dplyr::filter(.data$station_id == .env$station_id) |>
          dplyr::pull(.data[[value_col]])
      ),
      n_history = length(.data$history),
      p10 = if (.data$n_history > 0) {
        stats::quantile(.data$history, 0.1, na.rm = TRUE)
      } else {
        NA_real_
      },
      p50 = if (.data$n_history > 0) {
        stats::quantile(.data$history, 0.5, na.rm = TRUE)
      } else {
        NA_real_
      },
      p90 = if (.data$n_history > 0) {
        stats::quantile(.data$history, 0.9, na.rm = TRUE)
      } else {
        NA_real_
      },
      percentile = if (.data$n_history > 0) {
        100 * stats::ecdf(.data$history)(.data[[value_col]])
      } else {
        NA_real_
      },
      delta_from_median = .data[[value_col]] - .data$p50,
      status = dplyr::case_when(
        is.na(.data$percentile) ~ "missing",
        .data$percentile < 10 ~ "low",
        .data$percentile > 90 ~ "high",
        TRUE ~ "usual"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-history)
}

network_context_counts <- function(summary_df) {
  list(
    n_stations = nrow(summary_df),
    n_low = sum(summary_df$status == "low", na.rm = TRUE),
    n_high = sum(summary_df$status == "high", na.rm = TRUE),
    n_usual = sum(summary_df$status == "usual", na.rm = TRUE),
    n_missing = sum(summary_df$status == "missing", na.rm = TRUE),
    median_percentile = stats::median(summary_df$percentile, na.rm = TRUE)
  )
}

network_daily_context <- function(daily_df, clim_df, value_col) {
  if (nrow(daily_df) == 0) {
    return(list())
  }

  latest <- daily_df |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::slice(1)
  clim_today <- clim_df |>
    dplyr::filter(.data$doy == latest$doy)

  year_pct <- if (nrow(daily_df) > 1) {
    100 * stats::ecdf(daily_df$value)(latest$value)
  } else {
    NA_real_
  }

  list(
    latest_date = latest$date,
    latest_value = latest$value,
    n_stations = latest$n_stations,
    clim_p10 = if (nrow(clim_today) > 0) clim_today$p10[1] else NA_real_,
    clim_p50 = if (nrow(clim_today) > 0) clim_today$p50[1] else NA_real_,
    clim_p90 = if (nrow(clim_today) > 0) clim_today$p90[1] else NA_real_,
    year_percentile = year_pct,
    year_mean = mean(daily_df$value, na.rm = TRUE),
    year_min = min(daily_df$value, na.rm = TRUE),
    year_max = max(daily_df$value, na.rm = TRUE)
  )
}

ndbc_qc_label <- function(value_col) {
  rng <- ndbc_valid_range(value_col)
  sprintf("%.0f–%.0f", rng[1], rng[2])
}
