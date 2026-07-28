ndbc_display_tz <- "America/New_York"

format_ndbc_time <- function(time, tz = ndbc_display_tz) {
  if (length(time) == 0) {
    return(character())
  }
  if (all(is.na(time))) {
    return(NA_character_)
  }
  if (!inherits(time, "POSIXt")) {
    time <- as.POSIXct(time, tz = "UTC")
  }
  format(time, "%Y-%m-%d %H:%M %Z", tz = tz)
}

format_ndbc_erddap_iso <- function(iso_time, tz = ndbc_display_tz) {
  dt <- as.POSIXct(iso_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  format(dt, "%Y-%m-%d %H:%M %Z", tz = tz)
}

format_ndbc_date <- function(date, tz = ndbc_display_tz) {
  if (inherits(date, "Date")) {
    format(date, "%Y-%m-%d")
  } else if (inherits(date, "POSIXt")) {
    format(date, "%Y-%m-%d", tz = tz)
  } else {
    as.character(date)
  }
}

format_hour_panel_label <- function(target_time, hour, tz = ndbc_display_tz) {
  tz_abbrev <- format(target_time, "%Z", tz = tz)
  if (hour == 24L) {
    ref_day <- as.Date(target_time, tz = tz) - 1
    paste0(format(ref_day, "%Y-%m-%d"), " 24:00 ", tz_abbrev)
  } else {
    format(target_time, "%Y-%m-%d %H:%M %Z", tz = tz)
  }
}

ndbc_local_tz_label <- function(time = Sys.time(), tz = ndbc_display_tz) {
  format(time, "%Z", tz = tz)
}
