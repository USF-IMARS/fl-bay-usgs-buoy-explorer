source("R/ndbc_time.R")

ndbc_stations <- list(
  list(name = "bkyf1", long_name = "Buoy_Key"),
  list(name = "bnkf1", long_name = "Butternut_Kay"),
  list(name = "bobf1", long_name = "Bob_Allen_Key"),
  list(name = "bwsf1", long_name = "Blackwater_Sound"),
  list(name = "dkkf1", long_name = "Duck_Key"),
  list(name = "gbtf1", long_name = "Garfield_Bight"),
  list(name = "hcef1", long_name = "Highway_Creek"),
  list(name = "jbyf1", long_name = "Joe_Bay"),
  list(name = "jkyf1", long_name = "Johnson_Key"),
  list(name = "lbsf1", long_name = "Little_Blackwater_Sound"),
  list(name = "lmdf1", long_name = "Little_Madeira_Bay"),
  list(name = "lrkf1", long_name = "Little_Rabbit_Key"),
  list(name = "lsnf1", long_name = "Long_Sound"),
  list(name = "mdkf1", long_name = "Middle_Key"),
  list(name = "mukf1", long_name = "Murray_Key"),
  list(name = "pkyf1", long_name = "Peterson_Key"),
  list(name = "tbyf1", long_name = "Terrapin_Bay"),
  list(name = "thrf1", long_name = "Thursday_Point"),
  list(name = "wrbf1", long_name = "Whipray_Basin")
)

ndbc_field_hours <- c(6, 12, 18, 24)

fetch_ndbc_erddap_end_time <- function(station = "bkyf1") {
  for (lookback_days in c(90, 365, 3650)) {
    start_time <- format(
      Sys.time() - lookback_days * 24 * 3600,
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    url <- sprintf(
      "https://erddap.secoora.org/erddap/tabledap/gov-nps-ever-%s.json?time&time%%3E=%s",
      station,
      start_time
    )

    resp <- tryCatch(
      httr2::request(url) |>
        httr2::req_error(is_error = function(r) httr2::resp_status(r) >= 400) |>
        httr2::req_perform(),
      error = function(err) {
        warning("Failed to read ERDDAP time extent from ", station, ": ", conditionMessage(err))
        NULL
      }
    )
    if (is.null(resp)) {
      next
    }

    rows <- httr2::resp_body_json(resp)$table$rows
    times <- as.POSIXct(
      unlist(rows, use.names = FALSE),
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    times <- times[!is.na(times)]
    if (length(times) > 0) {
      return(max(times))
    }
  }

  warning("Could not determine ERDDAP end time; falling back to current time.")
  Sys.time()
}

ndbc_query_window <- function(lookback_hours = 48) {
  end_dt <- fetch_ndbc_erddap_end_time()
  end_time <- format(end_dt, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  start_time <- format(
    end_dt - lookback_hours * 3600,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  list(
    start_time = start_time,
    end_time = end_time,
    data_as_of = end_dt
  )
}

fetch_ndbc_timeseries <- function(station_info, param, start_time, end_time) {
  url <- sprintf(
    "https://erddap.secoora.org/erddap/tabledap/gov-nps-ever-%s.csv?time%%2Clatitude%%2Clongitude%%2C%s&time%%3E=%s&time%%3C=%s",
    station_info$name,
    param,
    start_time,
    end_time
  )

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400) |>
      httr2::req_perform(),
    error = function(err) {
      warning("Failed to fetch ", station_info$name, ": ", conditionMessage(err))
      NULL
    }
  )
  if (is.null(resp)) {
    return(NULL)
  }

  df <- readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
  if (param == "sea_water_practical_salinity") {
    value_col <- "salinity"
  } else if (param == "sea_water_temperature") {
    value_col <- "temperature"
  } else {
    stop("Unsupported ERDDAP parameter: ", param)
  }

  names(df) <- c("time", "latitude", "longitude", value_col)
  df <- df[-1, ] |>
    dplyr::mutate(
      time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      !!value_col := as.numeric(.data[[value_col]])
    ) |>
    dplyr::filter(!is.na(.data[[value_col]]))

  if (nrow(df) == 0) {
    return(NULL)
  }

  df |>
    dplyr::mutate(
      station_id = station_info$name,
      station = station_info$long_name,
      location = sprintf(
        "%s (%s)",
        station_info$long_name,
        sub("f1$", "", station_info$name)
      )
    )
}

reference_day_for_hours <- function(
    start_time,
    end_time,
    target_hours = ndbc_field_hours,
    tz = ndbc_display_tz
) {
  start_dt <- as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_dt <- as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  candidate_dates <- seq(
    as.Date(start_dt, tz = tz),
    as.Date(end_dt, tz = tz),
    by = "day"
  )

  hour_targets <- function(ref_date) {
    day_targets <- as.POSIXct(
      sprintf(
        "%s %02d:00:00",
        format(ref_date, "%Y-%m-%d"),
        target_hours[target_hours != 24]
      ),
      tz = tz
    )
    midnight <- as.POSIXct(
      sprintf("%s 00:00:00", format(ref_date + 1, "%Y-%m-%d")),
      tz = tz
    )
    c(day_targets, midnight)
  }

  for (ref_idx in rev(seq_along(candidate_dates))) {
    ref_date <- candidate_dates[ref_idx]
    targets <- hour_targets(ref_date)
    if (all(as.numeric(targets) >= as.numeric(start_dt) &
            as.numeric(targets) <= as.numeric(end_dt))) {
      return(ref_date)
    }
  }

  as.Date(end_dt, tz = tz) - 1
}

hour_target_times <- function(
    ref_date,
    target_hours = ndbc_field_hours,
    tz = ndbc_display_tz
) {
  regular <- target_hours[target_hours != 24]
  targets <- stats::setNames(
    as.POSIXct(
      sprintf("%s %02d:00:00", format(ref_date, "%Y-%m-%d"), regular),
      tz = tz
    ),
    as.character(regular)
  )
  targets[["24"]] <- as.POSIXct(
    sprintf("%s 00:00:00", format(ref_date + 1, "%Y-%m-%d")),
    tz = tz
  )
  targets[as.character(target_hours)]
}

snapshot_ndbc_at_hour <- function(all_series, target_time, value_col) {
  dplyr::bind_rows(lapply(all_series, function(station_df) {
    if (is.null(station_df) || nrow(station_df) == 0) {
      return(NULL)
    }

    idx <- which.min(abs(as.numeric(station_df$time - target_time)))
    row <- station_df[idx, ]
    dplyr::tibble(
      station_id = row$station_id,
      station = row$station,
      location = row$location,
      time = row$time,
      latitude = row$latitude,
      longitude = row$longitude,
      target_time = target_time,
      !!value_col := row[[value_col]]
    )
  }))
}

fetch_ndbc_latest_readings <- function(
    param,
    value_col,
    lookback_hours = 25,
    stations = ndbc_stations
) {
  window <- ndbc_query_window(lookback_hours)
  all_series <- lapply(
    stations,
    fetch_ndbc_timeseries,
    param = param,
    start_time = window$start_time,
    end_time = window$end_time
  )

  latest <- dplyr::bind_rows(lapply(seq_along(stations), function(i) {
    station_info <- stations[[i]]
    station_df <- all_series[[i]]
    location <- sprintf(
      "%s (%s)",
      station_info$long_name,
      sub("f1$", "", station_info$name)
    )

    if (is.null(station_df) || nrow(station_df) == 0) {
      return(dplyr::tibble(
        station_id = station_info$name,
        station = station_info$long_name,
        location = location,
        time = as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"),
        latitude = NA_real_,
        longitude = NA_real_,
        !!value_col := NA_real_
      ))
    }

    row <- station_df[which.max(station_df$time), ]
    dplyr::tibble(
      station_id = station_info$name,
      station = station_info$long_name,
      location = location,
      time = row$time,
      latitude = row$latitude,
      longitude = row$longitude,
      !!value_col := row[[value_col]]
    )
  }))

  list(
    data = latest,
    start_time = window$start_time,
    end_time = window$end_time,
    data_as_of = window$data_as_of
  )
}

fetch_ndbc_hourly_snapshots <- function(
    param,
    value_col,
    target_hours = ndbc_field_hours,
    lookback_hours = 48,
    stations = ndbc_stations
) {
  window <- ndbc_query_window(lookback_hours)
  all_series <- lapply(
    stations,
    fetch_ndbc_timeseries,
    param = param,
    start_time = window$start_time,
    end_time = window$end_time
  )

  ref_date <- reference_day_for_hours(
    window$start_time,
    window$end_time,
    target_hours
  )
  targets <- hour_target_times(ref_date, target_hours)

  snapshots <- lapply(names(targets), function(hour_label) {
    obs <- snapshot_ndbc_at_hour(all_series, targets[[hour_label]], value_col)
    list(
      hour = as.integer(hour_label),
      target_time = targets[[hour_label]],
      observations = obs
    )
  })
  names(snapshots) <- names(targets)

  latest <- snapshot_ndbc_at_hour(
    all_series,
    as.POSIXct(window$end_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value_col
  )

  list(
    snapshots = snapshots,
    latest = latest,
    reference_date = ref_date,
    start_time = window$start_time,
    end_time = window$end_time,
    data_as_of = window$data_as_of
  )
}

interpolate_ndbc_field <- function(
    observations,
    value_col,
    suffix = "",
    cache_id = "ndbc_buoys"
) {
  obs_file <- file.path(
    "data",
    "interpolated",
    sprintf("%s_obs%s.csv", value_col, suffix)
  )
  field_file <- file.path(
    "data",
    "interpolated",
    sprintf("%s_field%s.csv", value_col, suffix)
  )
  dir.create(dirname(obs_file), recursive = TRUE, showWarnings = FALSE)

  observations |>
    dplyr::select(longitude, latitude, dplyr::all_of(value_col)) |>
    readr::write_csv(obs_file)

  interp_log <- system2(
    "scripts/run_interpolate_field.sh",
    c(obs_file, field_file, cache_id),
    stdout = TRUE,
    stderr = TRUE
  )
  exit_code <- attr(interp_log, "status")
  if (!is.null(exit_code) && exit_code != 0L) {
    stop(paste("Julia interpolation failed:\n", paste(interp_log, collapse = "\n")))
  }

  field <- readr::read_csv(field_file, show_col_types = FALSE)
  list(field = field, log = interp_log)
}

interpolate_ndbc_hourly_fields <- function(
    snapshots,
    value_col,
    cache_id = "ndbc_buoys"
) {
  lapply(snapshots, function(snapshot) {
    if (nrow(snapshot$observations) == 0) {
      return(list(
        hour = snapshot$hour,
        target_time = snapshot$target_time,
        observations = snapshot$observations,
        field = NULL,
        log = character()
      ))
    }

    suffix <- sprintf("_%02dh", snapshot$hour)
    result <- interpolate_ndbc_field(
      snapshot$observations,
      value_col,
      suffix = suffix,
      cache_id = cache_id
    )
    list(
      hour = snapshot$hour,
      target_time = snapshot$target_time,
      observations = snapshot$observations,
      field = result$field,
      log = result$log
    )
  })
}
