source(here::here("R/ndbc_time.R"))

context_status_colors <- c(
  low = "#2166ac",
  usual = "#4daf4a",
  high = "#b2182b",
  missing = "#999999"
)

network_year_style <- function(
    year_labels,
    latest_color = "#d94801",
    old_color = "#1a1a1a",
    old_linewidth = 0.3,
    latest_linewidth = 1.5
) {
  n_years <- length(year_labels)
  colors <- rep(old_color, n_years)
  colors[n_years] <- latest_color
  names(colors) <- year_labels

  widths <- rep(old_linewidth, n_years)
  widths[n_years] <- latest_linewidth
  names(widths) <- year_labels

  list(colors = colors, widths = widths)
}

plot_context_distribution <- function(
    history_df,
    latest_df,
    value_col,
    title,
    unit,
    qc_label = NULL
) {
  if (nrow(history_df) == 0) {
    return(NULL)
  }

  if (is.null(qc_label)) {
    qc_label <- ndbc_qc_label(value_col)
  }

  current <- latest_df[[value_col]]
  current <- current[!is.na(current)]
  q_lines <- stats::quantile(
    history_df[[value_col]],
    c(0.1, 0.5, 0.9),
    na.rm = TRUE
  )

  ggplot2::ggplot(
    history_df,
    ggplot2::aes(x = .data[[value_col]])
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 35,
      fill = "#c6dbef",
      color = "#9ecae1",
      alpha = 0.85
    ) +
    ggplot2::geom_density(color = "#08519c", linewidth = 0.8) +
    ggplot2::geom_vline(
      xintercept = q_lines,
      linetype = "dashed",
      color = "#636363",
      linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      data = latest_df,
      ggplot2::aes(xintercept = .data[[value_col]]),
      color = "#d94801",
      linewidth = 0.9,
      alpha = 0.8
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "QC range %s (%s). Gray dashed lines: 10th / 50th / 90th percentile after removing sentinel and out-of-range values. Orange: latest readings (%d buoys).",
        qc_label,
        unit,
        length(current)
      ),
      x = unit,
      y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_station_percentiles <- function(
    summary_df,
    value_col,
    title,
    unit
) {
  if (nrow(summary_df) == 0) {
    return(NULL)
  }

  plot_df <- summary_df |>
    dplyr::filter(!is.na(.data$percentile)) |>
    dplyr::mutate(
      station_label = .data$station,
      status = factor(
        .data$status,
        levels = c("low", "usual", "high", "missing")
      )
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = stats::reorder(.data$station_label, .data$percentile),
      y = .data$percentile,
      fill = .data$status
    )
  ) +
    ggplot2::geom_col(width = 0.75, alpha = 0.9) +
    ggplot2::geom_hline(
      yintercept = c(10, 50, 90),
      linetype = "dashed",
      color = "#636363",
      linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(values = context_status_colors, name = NULL) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title,
      subtitle = "Where the latest reading falls in each buoy's QC-filtered past record (0 = lowest, 100 = highest).",
      x = NULL,
      y = "Percentile rank"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

plot_network_mean_by_year <- function(
    daily_df,
    clim_df,
    title,
    unit,
    lookback_years = 5,
    subtitle = NULL
) {
  if (nrow(daily_df) == 0 || nrow(clim_df) == 0) {
    return(NULL)
  }

  years <- sort(unique(daily_df$year))
  n_years <- length(years)
  year_labels <- as.character(years)

  year_style <- network_year_style(year_labels)
  year_colors <- year_style$colors
  line_widths <- year_style$widths

  month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  month_labels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )

  plot_df <- daily_df |>
    dplyr::filter(.data$doy <= 365L) |>
    dplyr::mutate(
      year_label = factor(as.character(.data$year), levels = year_labels)
    )

  ribbon_df <- clim_df |>
    dplyr::filter(.data$doy <= 365L) |>
    dplyr::arrange(.data$doy)

  if (is.null(subtitle)) {
    subtitle <- sprintf(
      paste0(
        "Network mean daily %s by calendar year (past %d years), overlaid on day of year. ",
        "Shaded band = usual range (10th–90th percentile by day of year); ",
        "dashed = climatological median. Older years are thin black lines; the latest year is orange and thicker."
      ),
      tolower(gsub("\\(.*\\)", "", unit)),
      lookback_years
    )
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$doy, y = .data$value)) +
    ggplot2::geom_ribbon(
      data = ribbon_df,
      ggplot2::aes(
        x = .data$doy,
        ymin = .data$p10,
        ymax = .data$p90
      ),
      inherit.aes = FALSE,
      fill = "#c6dbef",
      alpha = 0.55
    ) +
    ggplot2::geom_line(
      data = ribbon_df,
      ggplot2::aes(x = .data$doy, y = .data$p50),
      inherit.aes = FALSE,
      color = "#252525",
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        color = .data$year_label,
        linewidth = .data$year_label,
        group = .data$year_label
      ),
      alpha = 0.95,
      lineend = "round"
    ) +
    ggplot2::scale_color_manual(values = year_colors, name = "Year") +
    ggplot2::scale_linewidth_manual(values = line_widths, name = NULL, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = month_breaks,
      labels = month_labels,
      limits = c(1, 365),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = unit
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

plot_context_timeseries <- function(
    daily_df,
    clim_df,
    value_col,
    title,
    unit,
    recent_days = 30,
    subtitle = NULL
) {
  if (nrow(daily_df) == 0 || nrow(clim_df) == 0) {
    return(NULL)
  }

  plot_df <- if (is.null(recent_days)) {
    daily_df
  } else {
    recent_cutoff <- max(daily_df$date, na.rm = TRUE) - recent_days
    daily_df |>
      dplyr::filter(.data$date >= recent_cutoff)
  }

  plot_df <- plot_df |>
    dplyr::left_join(clim_df, by = "doy")

  if (is.null(subtitle)) {
    subtitle <- if (is.null(recent_days)) {
      sprintf(
        "Network mean daily %s for the full past year. Shaded band = usual range (10th–90th percentile by day of year across buoys); dashed = climatological median.",
        tolower(gsub("\\(.*\\)", "", unit))
      )
    } else {
      sprintf(
        "Network mean daily %s over the last %d days. Band = usual range (10th–90th percentile by day of year); dashed = median.",
        tolower(gsub("\\(.*\\)", "", unit)),
        recent_days
      )
    }
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$p10, ymax = .data$p90),
      fill = "#c6dbef",
      alpha = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$p50),
      color = "#636363",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$value),
      color = "#08519c",
      linewidth = if (is.null(recent_days)) 0.7 else 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      color = "#08519c",
      size = if (is.null(recent_days)) 0.8 else 1.2
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = unit
    ) +
    ggplot2::scale_x_date(date_labels = if (is.null(recent_days)) "%b %Y" else "%b %d") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

format_network_context <- function(ctx, unit, digits = 1) {
  if (length(ctx) == 0 || is.na(ctx$latest_value)) {
    return("Network mean unavailable.")
  }

  lookback_years <- if (is.null(ctx$lookback_years)) 5 else ctx$lookback_years
  pct_label <- if (is.na(ctx$record_percentile)) {
    "n/a"
  } else {
    sprintf("%.0f", ctx$record_percentile)
  }

  sprintf(
    "Latest network mean: **%.1f %s** on %s (%d buoys). Today's usual range: %.1f–%.1f %s (median %.1f). This sits at the **%sth percentile** of network daily means over the past %d years (record mean %.1f; range %.1f–%.1f).",
    ctx$latest_value,
    unit,
    format(ctx$latest_date, "%Y-%m-%d"),
    ctx$n_stations,
    ctx$clim_p10,
    ctx$clim_p90,
    unit,
    ctx$clim_p50,
    pct_label,
    lookback_years,
    ctx$record_mean,
    ctx$record_min,
    ctx$record_max
  )
}
