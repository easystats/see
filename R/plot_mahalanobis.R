#' Mahalanobis outlier detection plot
#'
#' Produces a scree-style Mahalanobis distance plot that highlights two types of
#' multivariate outliers. Observations exceeding the chi-squared cutoff are shown
#' in warm colors, while observations following large jumps ("elbows") in the sorted
#' Mahalanobis distances are shown in cool colors. Elbow outliers are defined based
#' on sudden increases in distance, analogous to inflection points in scree plots.
#'
#' This plot method is automatically called when plotting the result of
#' [performance::check_outliers()] with `method = "mahalanobis"`.
#'
#' @param x A `check_outliers` object created with `method = "mahalanobis"`.
#' @param idvar Optional character string giving the name of a variable in the original
#'   data to use as point labels (e.g., participant ID). If `NULL`, row names are used.
#' @param elbow_threshold Optional scalar specifying the minimum jump in Mahalanobis
#'   distance (between adjacent sorted observations) used to detect the elbow point.
#'   If supplied, all observations following the first jump greater than this value
#'   are flagged as outliers. If `NULL` (default), the largest jump is used
#'   automatically. Higher values yield more conservative outlier detection.
#' @param verbose Logical. If `TRUE` (default), prints a summary list of outlier IDs.
#' @param ... Additional arguments passed to plotting layers.
#'
#' @return A `ggplot2` object displaying the Mahalanobis outlier plot.
#'   When `verbose = TRUE`, also prints a list with the IDs of `chi_outliers` and `elbow_outliers`.
#'
#' @references
#' This implementation was inspired by a visualization approach developed by
#' Prof. Marina Doucerain (Université du Québec à Montréal).
#'
#' @examplesIf require("ggrepel")
#' set.seed(123)
#' x <- matrix(rnorm(200 * 5), ncol = 5)
#' colnames(x) <- paste0("Var", seq_len(ncol(x)))
#' df <- as.data.frame(x)
#' df$ID <- paste0("Obs", seq_len(nrow(df)))
#' x <- performance::check_outliers(df, threshold = 12)
#' plot(x, idvar = "ID")
plot_mahalanobis <- function(x,
                             idvar = NULL,
                             elbow_threshold = NULL,
                             verbose = TRUE, ...) {
  insight::check_if_installed("ggrepel")

  # Extract Mahalanobis distances
  att <- attributes(x)
  out_data <- att$data
  md <- out_data$Distance_Mahalanobis
  crit <- att$threshold$mahalanobis

  dat <- att$raw_data
  ordered_idx <- order(md)
  df_plot <- data.frame(
    obs = seq_along(md),
    mdist = sort(md),
    id = if (is.null(idvar) && idvar %in% names(dat)) {
      rownames(dat)[ordered_idx]
    } else {
      dat[[idvar]][ordered_idx]
    },
    stringsAsFactors = FALSE
  )

  # Chi-squared outliers
  df_plot$chi_outlier <- df_plot$mdist > crit

  # Elbow-based outliers
  diffs <- diff(df_plot$mdist)
  if (is.null(elbow_threshold)) {
    elbow_idx <- which.max(diffs)
    elbow_threshold <- diffs[elbow_idx]
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
      shape = 21, size = 2, stroke = 0.4, colour = "black"
    ) +
    ggplot2::geom_hline(yintercept = crit, colour = "#d33f49", linetype = "dashed", linewidth = 0.8) +
    ggplot2::scale_fill_manual(
      values = c(none = "gray85", chi = "#fcbba1", elbow = "#5b9bd5"),
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Mahalanobis Outlier Detection",
      x = "Observations (sorted)", y = "Mahalanobis Distance"
    ) +
    see::theme_modern()

  # Add elbow guideline segments (scree-style) — solid, with gap
  if (length(elbow_idx) > 0) {
    gap_proportion <- 0.15
    gaps <- (df_plot$mdist[elbow_idx + 1] - df_plot$mdist[elbow_idx]) * gap_proportion
    elbow_lines <- data.frame(
      x = df_plot$obs[elbow_idx],
      xend = df_plot$obs[elbow_idx + 1],
      y = df_plot$mdist[elbow_idx] + gaps,
      yend = df_plot$mdist[elbow_idx + 1] - gaps
    )
    p <- p + ggplot2::geom_segment(
      data = elbow_lines,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      inherit.aes = FALSE,
      colour = "#5b9bd5",
      linetype = "solid",
      linewidth = 0.6
    )
  }

  # Create a data frame with all points to be labeled
  label_data <- df_plot[df_plot$outlier_type %in% c("chi", "elbow"), ]

  # Add labels to plot
  p <- p + ggrepel::geom_text_repel(
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
      chi_outliers = df_plot$id[df_plot$chi_outlier],
      elbow_outliers = df_plot$id[df_plot$elbow_outlier],
      elbow_threshold = elbow_threshold
    ))
  }

  p
}
