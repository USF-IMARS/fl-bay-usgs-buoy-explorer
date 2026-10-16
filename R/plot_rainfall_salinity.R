source(here::here("R/ndbc_time.R"))

# generic buoy map: circle markers colored by one numeric column
plot_station_value_map <- function(
    df,
    value_col,
    legend_title,
    palette,
    popup_suffix = "",
    na_color = "#999999",
    radius = 8,
    zoom = 9
) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  df$map_value <- df[[value_col]]
  pal <- leaflet::colorNumeric(palette, domain = df$map_value, na.color = na_color)

  leaflet::leaflet(df) |>
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") |>
    leaflet::setView(
      lng = mean(df$longitude, na.rm = TRUE),
      lat = mean(df$latitude, na.rm = TRUE),
      zoom = zoom
    ) |>
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = radius,
      stroke = TRUE,
      weight = 1,
      color = "#333333",
      fillColor = ~pal(map_value),
      fillOpacity = 0.85,
      label = ~sprintf("%s: %.1f%s", location, map_value, popup_suffix)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = df$map_value,
      title = legend_title,
      opacity = 0.9
    )
}

# network mean daily rainfall (bars) stacked above network mean daily
# salinity (line), sharing an x-axis, so rain events can be lined up against
# salinity response
plot_rainfall_salinity_timeseries <- function(daily_precip, daily_salinity) {
  if (nrow(daily_precip) == 0 || nrow(daily_salinity) == 0) {
    return(NULL)
  }

  date_range <- range(c(daily_precip$date, daily_salinity$date))

  p_rain <- ggplot2::ggplot(daily_precip, ggplot2::aes(x = date, y = mean_daily_mm)) +
    ggplot2::geom_col(fill = "#2166ac", width = 0.9) +
    ggplot2::scale_x_date(limits = date_range) +
    ggplot2::labs(title = "Network mean daily rainfall", x = NULL, y = "Rainfall (mm/day)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  p_sal <- ggplot2::ggplot(daily_salinity, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "#08519c", linewidth = 0.7) +
    ggplot2::scale_x_date(limits = date_range) +
    ggplot2::labs(title = "Network mean daily salinity", x = NULL, y = "Salinity (PSU)") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  patchwork::wrap_plots(p_rain, p_sal, ncol = 1, heights = c(1, 1.4))
}

# per-buoy cumulative rainfall vs. that buoy's latest salinity: a tight
# negative relationship would point to local rain-driven freshening, a weak
# one points to freshening arriving from elsewhere (e.g. Everglades outflow)
plot_rainfall_salinity_scatter <- function(joined_df, lookback_days) {
  if (nrow(joined_df) < 3) {
    return(NULL)
  }

  ggplot2::ggplot(joined_df, ggplot2::aes(x = cumulative_mm, y = salinity, label = station)) +
    ggplot2::geom_smooth(
      method = "lm", formula = y ~ x, se = FALSE,
      color = "#d94801", linewidth = 0.6, linetype = "dashed"
    ) +
    ggplot2::geom_point(size = 3, color = "#08519c", alpha = 0.85) +
    ggplot2::geom_text(
      vjust = -1, size = 3, color = "#333333", check_overlap = TRUE
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(
      title = sprintf("Cumulative rainfall (past %d days) vs. latest salinity", lookback_days),
      subtitle = "Each point is one buoy. A downward trend suggests locally rain-driven freshening.",
      x = "Cumulative rainfall (mm)",
      y = "Latest salinity (PSU)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
