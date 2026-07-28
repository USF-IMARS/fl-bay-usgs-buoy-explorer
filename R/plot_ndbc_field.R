source("R/ndbc_time.R")

field_slice_grid <- function(field_slice) {
  lons <- sort(unique(field_slice$longitude))
  lats <- sort(unique(field_slice$latitude))
  n_lon <- length(lons)
  n_lat <- length(lats)

  dx <- if (n_lon > 1) diff(lons)[1] / 2 else 0.01
  dy <- if (n_lat > 1) diff(lats)[1] / 2 else 0.01

  field_slice$lon1 <- field_slice$longitude - dx
  field_slice$lon2 <- field_slice$longitude + dx
  field_slice$lat1 <- field_slice$latitude - dy
  field_slice$lat2 <- field_slice$latitude + dy
  field_slice
}

field_raster <- function(field, value_col) {
  xyz <- field[, c("longitude", "latitude", value_col)]
  names(xyz)[3] <- "value"
  raster::rasterFromXYZ(
    xyz,
    crs = "+proj=longlat +datum=WGS84"
  )
}

plot_ndbc_field <- function(
    field,
    observations,
    value_col,
    legend_title,
    popup_suffix = "",
    palette = viridisLite::turbo(256),
    height = 520,
    value_range = NULL,
    show_legend = TRUE,
    show_scale_bar = TRUE,
    show_layers_control = TRUE
) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  obs <- as.data.frame(observations)
  obs$marker_value <- obs[[value_col]]
  if (is.null(value_range)) {
    value_range <- range(
      c(field[[value_col]], obs$marker_value),
      na.rm = TRUE
    )
  }
  pal <- leaflet::colorNumeric(
    palette,
    domain = value_range,
    na.color = "transparent"
  )

  lon_min <- min(field$longitude, na.rm = TRUE)
  lon_max <- max(field$longitude, na.rm = TRUE)
  lat_min <- min(field$latitude, na.rm = TRUE)
  lat_max <- max(field$latitude, na.rm = TRUE)

  field_rast <- field_raster(field, value_col)
  grid <- field_slice_grid(field)
  grid$popup_label <- sprintf(
    paste0("%.1f", popup_suffix),
    grid[[value_col]]
  )

  map <- leaflet::leaflet(
    width = "100%",
    height = height,
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) |>
    leaflet::addMapPane("fieldValues", zIndex = 620) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.OceanBasemap,
      group = "Ocean"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "Light"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap,
      group = "OpenStreetMap"
    ) |>
    leaflet::addRasterImage(
      field_rast,
      colors = pal,
      opacity = 0.85,
      group = "Interpolated field"
    ) |>
    leaflet::addRectangles(
      data = grid,
      lng1 = ~lon1,
      lat1 = ~lat1,
      lng2 = ~lon2,
      lat2 = ~lat2,
      fillColor = "#000000",
      fillOpacity = 0.001,
      color = "#000000",
      opacity = 0,
      weight = 0,
      popup = ~popup_label,
      group = "Interpolated field",
      options = leaflet::pathOptions(
        pane = "fieldValues",
        interactive = TRUE
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 1,
        color = "#333333",
        fillOpacity = 0.15,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addCircleMarkers(
      data = obs,
      lng = ~longitude,
      lat = ~latitude,
      radius = 6,
      fillColor = ~pal(marker_value),
      fillOpacity = 0.95,
      color = "#1a1a1a",
      weight = 1,
      stroke = TRUE,
      label = ~sprintf(
        "%s: %.1f%s\n%s",
        location,
        marker_value,
        popup_suffix,
        format_ndbc_time(time)
      ),
      group = "Observations"
    )

  if (show_layers_control) {
    map <- map |>
      leaflet::addLayersControl(
        baseGroups = c("Ocean", "Light", "OpenStreetMap"),
        overlayGroups = c("Interpolated field", "Observations"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )
  }

  if (show_legend) {
    map <- map |>
      leaflet::addLegend(
        pal = pal,
        values = value_range,
        title = legend_title,
        position = "bottomright"
      )
  }

  if (show_scale_bar) {
    map <- map |>
      leaflet::addScaleBar(position = "bottomleft")
  }

  map <- map |>
    leaflet::fitBounds(
      lng1 = lon_min,
      lat1 = lat_min,
      lng2 = lon_max,
      lat2 = lat_max
    )

  map
}

plot_ndbc_field_panels <- function(
    field_results,
    value_col,
    legend_title,
    popup_suffix = "",
    palette = viridisLite::turbo(256),
    panel_height = 280,
    label_fn = NULL
) {
  valid <- Filter(function(x) !is.null(x$field) && nrow(x$field) > 0, field_results)
  if (length(valid) == 0) {
    return(NULL)
  }

  all_values <- unlist(lapply(valid, function(x) {
    c(x$field[[value_col]], x$observations[[value_col]])
  }), use.names = FALSE)
  value_range <- range(all_values, na.rm = TRUE)
  last_valid_idx <- max(which(vapply(
    field_results,
    function(x) !is.null(x$field) && nrow(x$field) > 0,
    logical(1)
  )))

  htmltools::tags$div(
    class = "ndbc-field-row",
    lapply(seq_along(field_results), function(i) {
      result <- field_results[[i]]
      label <- if (is.null(label_fn)) {
        format_ndbc_time(result$target_time)
      } else {
        label_fn(result$target_time, result$hour)
      }

      htmltools::tags$div(
        class = "ndbc-field-panel",
        htmltools::tags$p(class = "ndbc-field-panel-label", label),
        if (is.null(result$field) || nrow(result$field) == 0) {
          htmltools::tags$p(class = "ndbc-field-panel-empty", "No data for this hour.")
        } else {
          plot_ndbc_field(
            result$field,
            result$observations,
            value_col = value_col,
            legend_title = legend_title,
            popup_suffix = popup_suffix,
            palette = palette,
            height = panel_height,
            value_range = value_range,
            show_legend = i == last_valid_idx,
            show_scale_bar = FALSE,
            show_layers_control = FALSE
          )
        }
      )
    })
  )
}

plot_salinity_field <- function(field, observations) {
  plot_ndbc_field(
    field,
    observations,
    value_col = "salinity",
    legend_title = "Salinity (PSU)",
    popup_suffix = " PSU"
  )
}
