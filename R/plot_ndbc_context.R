source("R/ndbc_time.R")

context_status_colors <- c(
  low = "#2166ac",
  usual = "#4daf4a",
  high = "#b2182b",
  missing = "#999999"
)

plot_context_distribution <- function(
    history_df,
    latest_df,
    value_col,
    title,
    unit
) {
  if (nrow(history_df) == 0) {
    return(NULL)
  }

  current <- latest_df[[value_col]]
  current <- current[!is.na(current)]

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
      xintercept = stats::quantile(
        history_df[[value_col]],
        c(0.1, 0.5, 0.9),
        na.rm = TRUE
      ),
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
        "Gray dashed lines: 10th / 50th / 90th percentile of the past year (all buoys). Orange: latest readings (%d buoys).",
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
      subtitle = "Where the latest reading falls in each buoy's past-year distribution (0 = lowest, 100 = highest).",
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

plot_context_timeseries <- function(
    daily_df,
    clim_df,
    value_col,
    title,
    unit,
    recent_days = 30
) {
  if (nrow(daily_df) == 0 || nrow(clim_df) == 0) {
    return(NULL)
  }

  recent_cutoff <- max(daily_df$date, na.rm = TRUE) - recent_days
  recent <- daily_df |>
    dplyr::filter(.data$date >= recent_cutoff) |>
    dplyr::left_join(clim_df, by = "doy")

  ggplot2::ggplot(recent, ggplot2::aes(x = .data$date)) +
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
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      color = "#08519c",
      size = 1.2
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "Network mean daily %s over the last %d days. Band = usual range (10th–90th percentile by day of year); dashed = median.",
        tolower(gsub("\\(.*\\)", "", unit)),
        recent_days
      ),
      x = NULL,
      y = unit
    ) +
    ggplot2::scale_x_date(date_labels = "%b %d") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_station_context_ts <- function(
    history_df,
    clim_df,
    station_id,
    value_col,
    title,
    unit,
    recent_days = 30
) {
  station_df <- history_df |>
    dplyr::filter(.data$station_id == .env$station_id)

  if (nrow(station_df) == 0) {
    return(NULL)
  }

  daily <- station_df |>
    dplyr::group_by(.data$date, .data$doy) |>
    dplyr::summarize(
      value = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )

  recent_cutoff <- max(daily$date, na.rm = TRUE) - recent_days
  station_clim <- station_df |>
    dplyr::group_by(.data$doy) |>
    dplyr::summarize(
      p10 = stats::quantile(.data[[value_col]], 0.1, na.rm = TRUE),
      p50 = stats::quantile(.data[[value_col]], 0.5, na.rm = TRUE),
      p90 = stats::quantile(.data[[value_col]], 0.9, na.rm = TRUE),
      .groups = "drop"
    )

  recent <- daily |>
    dplyr::filter(.data$date >= recent_cutoff) |>
    dplyr::left_join(station_clim, by = "doy")

  ggplot2::ggplot(recent, ggplot2::aes(x = .data$date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$p10, ymax = .data$p90),
      fill = "#fee0d2",
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$p50),
      color = "#636363",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$value),
      color = "#a63603",
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      color = "#a63603",
      size = 1.2
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("Daily %s with station-specific usual range (past year by day of year).", tolower(unit)),
      x = NULL,
      y = unit
    ) +
    ggplot2::scale_x_date(date_labels = "%b %d") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}
