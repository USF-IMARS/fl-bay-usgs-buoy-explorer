source(here::here("R/ndbc_time.R"))

# a single buoy's time series, with the cruise's model window shaded
plot_cruise_timeseries <- function(
    df,
    value_col,
    unit,
    title,
    window_start,
    window_end,
    line_color = "#08519c"
) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  window_start_dt <- as.POSIXct(
    sprintf("%s 00:00:00", format(window_start)),
    tz = ndbc_display_tz
  )
  window_end_dt <- as.POSIXct(
    sprintf("%s 00:00:00", format(window_end + 1)),
    tz = ndbc_display_tz
  )

  ggplot2::ggplot(df, ggplot2::aes(x = time, y = .data[[value_col]])) +
    ggplot2::annotate(
      "rect",
      xmin = window_start_dt,
      xmax = window_end_dt,
      ymin = -Inf,
      ymax = Inf,
      fill = "#fdae61",
      alpha = 0.18
    ) +
    ggplot2::geom_line(color = line_color, linewidth = 0.6) +
    ggplot2::labs(
      title = title,
      subtitle = "Shaded band = cruise model window",
      x = NULL,
      y = unit
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# station checkpoints (circles) and their matched nearest buoys (diamonds),
# connected by a line
plot_cruise_station_map <- function(matched_stations) {
  if (is.null(matched_stations) || nrow(matched_stations) == 0) {
    return(NULL)
  }

  buoys <- matched_stations |>
    dplyr::distinct(nearest_station_id, nearest_station, nearest_latitude, nearest_longitude)

  map <- leaflet::leaflet(matched_stations) |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addScaleBar(position = "bottomleft")

  for (i in seq_len(nrow(matched_stations))) {
    row <- matched_stations[i, ]
    map <- map |>
      leaflet::addPolylines(
        lng = c(row$longitude, row$nearest_longitude),
        lat = c(row$latitude, row$nearest_latitude),
        color = "#999999",
        weight = 1,
        dashArray = "3,4"
      )
  }

  map <- map |>
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 5,
      color = "#1a1a1a",
      weight = 1,
      fillColor = "#4daf4a",
      fillOpacity = 0.9,
      label = ~sprintf(
        "Station %s (%s) -> nearest buoy: %s (%.1f km)",
        station, region, nearest_station, nearest_distance_km
      )
    ) |>
    leaflet::addCircleMarkers(
      data = buoys,
      lng = ~nearest_longitude,
      lat = ~nearest_latitude,
      radius = 8,
      color = "#1a1a1a",
      weight = 1.5,
      fillColor = "#d94801",
      fillOpacity = 0.95,
      label = ~sprintf("Buoy: %s", nearest_station)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("#4daf4a", "#d94801"),
      labels = c("Cruise checkpoint", "Nearest NDBC buoy"),
      opacity = 0.9
    )

  lons <- c(matched_stations$longitude, buoys$nearest_longitude)
  lats <- c(matched_stations$latitude, buoys$nearest_latitude)
  map |>
    leaflet::fitBounds(
      lng1 = min(lons, na.rm = TRUE),
      lat1 = min(lats, na.rm = TRUE),
      lng2 = max(lons, na.rm = TRUE),
      lat2 = max(lats, na.rm = TRUE)
    )
}
