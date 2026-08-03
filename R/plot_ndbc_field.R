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
    show_layers_control = TRUE,
    prefer_canvas = TRUE,
    show_marker_labels = TRUE,
    interactive_grid = TRUE,
    use_svg_markers = FALSE
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
  grid <- if (interactive_grid) {
    grid_df <- field_slice_grid(field)
    grid_df$popup_label <- sprintf(
      paste0("%.1f", popup_suffix),
      grid_df[[value_col]]
    )
    grid_df
  } else {
    NULL
  }

  map_options <- leaflet::leafletOptions(
    preferCanvas = prefer_canvas,
    zoomAnimation = FALSE,
    fadeAnimation = FALSE,
    markerZoomAnimation = FALSE
  )
  tile_options <- leaflet::providerTileOptions(
    updateWhenIdle = TRUE,
    updateWhenZooming = FALSE,
    keepBuffer = 2
  )

  map <- leaflet::leaflet(
    width = "100%",
    height = height,
    options = map_options
  )

  if (show_layers_control) {
    map <- map |>
      leaflet::addMapPane("fieldValues", zIndex = 620) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.OceanBasemap,
        group = "Ocean",
        options = tile_options
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        group = "Light",
        options = tile_options
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap,
        group = "OpenStreetMap",
        options = tile_options
      )
  } else {
    map <- map |>
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.Positron,
        options = tile_options
      )
  }

  map <- map |>
    leaflet::addRasterImage(
      field_rast,
      colors = pal,
      opacity = 0.85,
      group = "Interpolated field"
    )

  if (interactive_grid) {
    map <- map |>
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
      )
  }

  marker_popup <- ~sprintf(
    "%s: %.1f%s<br/>%s",
    location,
    marker_value,
    popup_suffix,
    format_ndbc_time(time)
  )
  marker_label <- if (show_marker_labels) {
    ~sprintf(
      "%s: %.1f%s\n%s",
      location,
      marker_value,
      popup_suffix,
      format_ndbc_time(time)
    )
  } else {
    NULL
  }

  map <- map |>
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
      popup = marker_popup,
      label = marker_label,
      group = "Observations",
      options = if (use_svg_markers) {
        leaflet::pathOptions(
          renderer = leaflet::JS("L.svg({ padding: 0.5 })")
        )
      } else {
        NULL
      }
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

field_value_range <- function(field_results, value_col) {
  valid <- Filter(function(x) !is.null(x$field) && nrow(x$field) > 0, field_results)
  if (length(valid) == 0) {
    return(c(NA_real_, NA_real_))
  }
  range(
    unlist(lapply(valid, function(x) {
      c(x$field[[value_col]], x$observations[[value_col]])
    }), use.names = FALSE),
    na.rm = TRUE
  )
}

is_leaflet_map <- function(x) {
  inherits(x, "leaflet")
}

sync_leaflet_pair <- function(map1, map2) {
  pair_id <- paste0("ndbc-sync-", sample.int(999999L, 1L))
  sync_script <- sprintf(
    paste0(
      "(function() {",
      "function bindPair(source, target, lock) {",
      "var frame = null;",
      "source.on('move', function() {",
      "if (lock.active) return;",
      "if (frame !== null) return;",
      "frame = requestAnimationFrame(function() {",
      "frame = null;",
      "lock.active = true;",
      "target.setView(source.getCenter(), source.getZoom(), {animate: false, reset: true});",
      "lock.active = false;",
      "});",
      "});",
      "}",
      "function linkMaps(container, attempt) {",
      "if (!container || container.dataset.syncLinked === 'true') return;",
      "var maps = [];",
      "container.querySelectorAll('.leaflet.html-widget').forEach(function(el) {",
      "var widget = HTMLWidgets.find('#' + el.id);",
      "if (widget && widget.getMap()) maps.push(widget.getMap());",
      "});",
      "if (maps.length < 2) {",
      "if ((attempt || 0) < 50) {",
      "setTimeout(function() { linkMaps(container, (attempt || 0) + 1); }, 100);",
      "}",
      "return;",
      "}",
      "maps.forEach(function(map) { map.invalidateSize(); });",
      "var lock = {active: false};",
      "bindPair(maps[0], maps[1], lock);",
      "bindPair(maps[1], maps[0], lock);",
      "container.dataset.syncLinked = 'true';",
      "}",
      "var container = document.getElementById('%s');",
      "if (container) linkMaps(container, 0);",
      "document.addEventListener('shown.bs.tab', function() {",
      "if (!container) return;",
      "container.querySelectorAll('.leaflet.html-widget').forEach(function(el) {",
      "var widget = HTMLWidgets.find('#' + el.id);",
      "if (widget && widget.getMap()) widget.getMap().invalidateSize();",
      "});",
      "if (container.dataset.syncLinked !== 'true') linkMaps(container, 0);",
      "});",
      "})();"
    ),
    pair_id
  )

  htmltools::tagList(
    htmltools::tags$div(
      id = pair_id,
      class = "ndbc-paired-row ndbc-sync-maps",
      htmltools::tags$div(class = "ndbc-field-panel", map1),
      htmltools::tags$div(class = "ndbc-field-panel", map2)
    ),
    htmlwidgets::onStaticRenderComplete(sync_script)
  )
}

plot_ndbc_hour_pair <- function(
    sal_result,
    temp_result,
    sal_value_range,
    temp_value_range,
    sal_palette,
    temp_palette,
    panel_height = 320,
    show_sal_legend = FALSE,
    show_temp_legend = FALSE,
    show_hour_label = TRUE,
    label_fn = NULL
) {
  label <- if (is.null(label_fn)) {
    format_ndbc_time(sal_result$target_time)
  } else {
    label_fn(sal_result$target_time, sal_result$hour)
  }

  sal_map <- if (is.null(sal_result$field) || nrow(sal_result$field) == 0) {
    htmltools::tags$p(class = "ndbc-field-panel-empty", "No salinity data for this hour.")
  } else {
    plot_ndbc_field(
      sal_result$field,
      sal_result$observations,
      value_col = "salinity",
      legend_title = "Salinity (PSU)",
      popup_suffix = " PSU",
      palette = sal_palette,
      height = panel_height,
      value_range = sal_value_range,
      show_legend = show_sal_legend,
      show_scale_bar = FALSE,
      show_layers_control = FALSE,
      prefer_canvas = TRUE,
      show_marker_labels = FALSE,
      interactive_grid = FALSE,
      use_svg_markers = TRUE
    )
  }

  temp_map <- if (is.null(temp_result$field) || nrow(temp_result$field) == 0) {
    htmltools::tags$p(class = "ndbc-field-panel-empty", "No temperature data for this hour.")
  } else {
    plot_ndbc_field(
      temp_result$field,
      temp_result$observations,
      value_col = "temperature",
      legend_title = "Temperature (°C)",
      popup_suffix = " °C",
      palette = temp_palette,
      height = panel_height,
      value_range = temp_value_range,
      show_legend = show_temp_legend,
      show_scale_bar = FALSE,
      show_layers_control = FALSE,
      prefer_canvas = TRUE,
      show_marker_labels = FALSE,
      interactive_grid = FALSE,
      use_svg_markers = TRUE
    )
  }

  htmltools::tags$div(
    class = "ndbc-paired-hour",
    if (show_hour_label) {
      htmltools::tags$p(class = "ndbc-paired-hour-label", label)
    },
    if (is_leaflet_map(sal_map) && is_leaflet_map(temp_map)) {
      htmltools::tagList(
        htmltools::tags$div(
          class = "ndbc-paired-labels",
          htmltools::tags$p(class = "ndbc-field-panel-label", "Salinity"),
          htmltools::tags$p(class = "ndbc-field-panel-label", "Temperature")
        ),
        sync_leaflet_pair(sal_map, temp_map)
      )
    } else {
      htmltools::tags$div(
        class = "ndbc-paired-row",
        htmltools::tags$div(
          class = "ndbc-field-panel",
          htmltools::tags$p(class = "ndbc-field-panel-label", "Salinity"),
          sal_map
        ),
        htmltools::tags$div(
          class = "ndbc-field-panel",
          htmltools::tags$p(class = "ndbc-field-panel-label", "Temperature"),
          temp_map
        )
      )
    }
  )
}

plot_ndbc_paired_field_panels <- function(
    salinity_fields,
    temperature_fields,
    sal_palette,
    temp_palette,
    panel_height = 320,
    label_fn = NULL
) {
  if (length(salinity_fields) != length(temperature_fields)) {
    stop("Salinity and temperature field results must have the same length.")
  }

  sal_range <- field_value_range(salinity_fields, "salinity")
  temp_range <- field_value_range(temperature_fields, "temperature")
  last_idx <- length(salinity_fields)

  htmltools::tags$div(
    class = "ndbc-paired-stack",
    lapply(seq_along(salinity_fields), function(i) {
      plot_ndbc_hour_pair(
        salinity_fields[[i]],
        temperature_fields[[i]],
        sal_value_range = sal_range,
        temp_value_range = temp_range,
        sal_palette = sal_palette,
        temp_palette = temp_palette,
        panel_height = panel_height,
        show_sal_legend = i == last_idx,
        show_temp_legend = i == last_idx,
        show_hour_label = TRUE,
        label_fn = label_fn
      )
    })
  )
}
