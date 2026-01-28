.plot_scree <- function(
  x,
  elbow_threshold = NULL,
  rescale_distance = FALSE,
  size_title = 12,
  base_size = 10,
  size_axis_title = base_size,
  theme = NULL,
  verbose = TRUE,
  ...
) {
  insight::check_if_installed("ggrepel")

  theme <- .set_default_theme(
    x,
    theme,
    base_size,
    size_axis_title,
    size_title,
    default_theme = ggplot2::theme_grey()
  )

  att <- attributes(x)
  method <- tools::toTitleCase(att$method)

  d <- data_plot(x, rescale_distance = rescale_distance)

  md <- d$Distance
  crit <- att$threshold[[1]]
  ordered_idx <- order(md)
  obs <- seq_along(md)

  rescaled <- attr(d, "rescale_distance")
  if (isTRUE(rescaled)) {
    y_lab <- paste(method, "Distance (rescaled range 0-1)")
    crit <- attr(d, "rescale_threshold")
  } else {
    y_lab <- paste(method, "Distance")
  }

  if (is.null(d$ID)) {
    ID <- obs[ordered_idx]
  } else {
    ID <- d$ID[ordered_idx]
  }

  df_plot <- data.frame(
    obs = obs,
    mdist = sort(md),
    id = ID,
    stringsAsFactors = FALSE
  )

  # Chi-squared outliers
  df_plot$chi_outlier <- df_plot$mdist > crit

  # Elbow-based outliers
  diffs <- diff(df_plot$mdist)

  if (is.null(elbow_threshold)) {
    elbow_idx <- which.max(diffs)
    elbow_threshold <- diffs[elbow_idx]
    if (length(elbow_threshold) == 0) {
      insight::format_error(
        "Could not automatically determine an elbow point from the outlier distances."
      )
    }
  } else {
    elbow_idx <- which(diffs > elbow_threshold)[1]
  }

  if (is.na(elbow_idx)) {
    df_plot$elbow_outlier <- FALSE
  } else {
    df_plot$elbow_outlier <- df_plot$obs > elbow_idx
  }

  df_plot$outlier_type <- "none"
  df_plot$outlier_type[df_plot$chi_outlier] <- "chi"
  df_plot$outlier_type[df_plot$elbow_outlier] <- "elbow"

  # Build base plot
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$obs, y = .data$mdist)) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data$outlier_type),
      shape = 21,
      size = 2,
      stroke = 0.4,
      colour = "black"
    ) +
    ggplot2::geom_hline(
      yintercept = crit,
      colour = "#d33f49",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::scale_fill_manual(
      values = c(none = "gray85", chi = "#fcbba1", elbow = "#5b9bd5"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Scree Outlier Detection",
      x = "Observations (sorted)",
      y = y_lab
    ) +
    theme

  # Add elbow guideline segments (scree-style) — solid, with gap
  if (length(elbow_idx) > 0) {
    gap_proportion <- 0.15
    gaps <- (df_plot$mdist[elbow_idx + 1] - df_plot$mdist[elbow_idx]) *
      gap_proportion
    elbow_lines <- data.frame(
      x = df_plot$obs[elbow_idx],
      xend = df_plot$obs[elbow_idx + 1],
      y = df_plot$mdist[elbow_idx] + gaps,
      yend = df_plot$mdist[elbow_idx + 1] - gaps
    )
    p <- p +
      ggplot2::geom_segment(
        data = elbow_lines,
        ggplot2::aes(
          x = .data$x,
          xend = .data$xend,
          y = .data$y,
          yend = .data$yend
        ),
        inherit.aes = FALSE,
        colour = "#5b9bd5",
        linetype = "solid",
        linewidth = 0.6
      )
  }

  # Create a data frame with all points to be labeled
  label_data <- df_plot[df_plot$outlier_type %in% c("chi", "elbow"), ]

  # Add labels to plot
  p <- p +
    ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(label = .data$id, colour = .data$outlier_type),
      size = 2.8,
      min.segment.length = 0,
      direction = "x",
      hjust = 1,
      nudge_x = -5,
      max.overlaps = 10,
      box.padding = 0.3,
      segment.color = "gray60",
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = c(chi = "#d33f49", elbow = "#5b9bd5"),
      guide = "none"
    )

  # Return the outlier data for user reference
  if (verbose) {
    print(list(
      threshold_outliers = df_plot$id[df_plot$chi_outlier],
      threshold = crit,
      elbow_outliers = df_plot$id[df_plot$elbow_outlier],
      elbow_threshold = elbow_threshold
    ))
  }

  p
}
