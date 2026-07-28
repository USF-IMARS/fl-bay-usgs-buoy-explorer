source("R/ndbc_time.R")

load_cached_hourly_fields <- function(
    value_col,
    hours = c(6, 12, 18, 24),
    snapshots = NULL
) {
  lapply(hours, function(hour) {
    suffix <- sprintf("_%02dh", hour)
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

    observations <- if (!is.null(snapshots) && !is.null(snapshots[[as.character(hour)]])) {
      snapshots[[as.character(hour)]]$observations
    } else if (file.exists(obs_file)) {
      readr::read_csv(obs_file, show_col_types = FALSE)
    } else {
      dplyr::tibble()
    }

    field <- if (file.exists(field_file)) {
      readr::read_csv(field_file, show_col_types = FALSE)
    } else {
      NULL
    }

    target_time <- if (!is.null(snapshots) && !is.null(snapshots[[as.character(hour)]])) {
      snapshots[[as.character(hour)]]$target_time
    } else {
      as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
    }

    list(
      hour = hour,
      target_time = target_time,
      observations = observations,
      field = field,
      cached = TRUE
    )
  })
}

interpolate_or_load_fields <- function(
    snapshots,
    value_col,
    cache_id = "ndbc_buoys"
) {
  tryCatch(
    interpolate_ndbc_hourly_fields(
      snapshots,
      value_col = value_col,
      cache_id = cache_id
    ),
    error = function(err) {
      warning(
        "Live DIVAnd interpolation failed; using cached fields. ",
        conditionMessage(err)
      )
      load_cached_hourly_fields(value_col, snapshots = snapshots)
    }
  )
}
