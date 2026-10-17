skip_on_cran()
skip_if_not_installed("performance")
# without ggrepel, the fallback labels are nudged and widen the y range
skip_if_not_installed("ggrepel")

# 400 cases, residuals of about -2.7 to 3.6: no contour line comes near them
.f1_data <- function() {
  set.seed(1)
  x <- stats::rnorm(400)
  data.frame(x = x, y = x + stats::rnorm(400))
}

# Poisson data, plotted with two contour levels set by hand
.f5_data <- function() {
  set.seed(2)
  x <- stats::rnorm(200)
  data.frame(x = x, y = stats::rpois(200, exp(0.5 + 0.3 * x)))
}

.outliers_panel <- function(
  model,
  entry = c("check_model", "check_outliers"),
  cook_levels = NULL,
  show_labels = NULL,
  ...
) {
  entry <- match.arg(entry)
  if (entry == "check_outliers") {
    return(plot(performance::check_outliers(model, method = "cook")))
  }
  cm <- suppressMessages(performance::check_model(
    model,
    check = "outliers",
    residual_type = "normal",
    ...
  ))
  if (!is.null(cook_levels)) {
    attr(cm$INFLUENTIAL, "cook_levels") <- cook_levels
  }
  # performance::check_model() has no `show_labels` argument
  if (!is.null(show_labels)) {
    attr(cm, "show_labels") <- show_labels
  }
  plot(cm)
}

.panel_ranges <- function(b) {
  list(
    x = b$layout$panel_params[[1]]$x.range,
    y = b$layout$panel_params[[1]]$y.range
  )
}

.annotation_layers <- function(b, geom) {
  keep <- vapply(
    b$plot$layers,
    function(l) inherits(l$geom, geom) && isFALSE(l$inherit.aes),
    logical(1)
  )
  b$data[keep]
}

# the Cook's distance level of a contour line, from its first row
.line_level <- function(line, n_params) {
  h <- line$x[1]
  line$y[1]^2 * h / ((1 - h) * n_params)
}

# for each contour level and side, whether a row of its line is inside the y
# range, and the label anchors with that level's text on that side
.contour_summary <- function(b, cook_levels, n_params = 2) {
  r <- .panel_ranges(b)
  lines <- .annotation_layers(b, "GeomLine")
  texts <- do.call(rbind, .annotation_layers(b, "GeomText"))
  if (is.null(texts)) {
    texts <- data.frame(label = character(), x = numeric(), y = numeric())
  }
  out <- expand.grid(level = cook_levels, side = c(1, -1))
  out$inside <- FALSE
  out$labels_inside <- 0L
  out$label_at_max_hat <- FALSE
  for (i in seq_len(nrow(out))) {
    for (line in lines) {
      if (
        nrow(line) &&
          sign(line$y[1]) == out$side[i] &&
          abs(.line_level(line, n_params) - out$level[i]) < 1e-6
      ) {
        out$inside[i] <- out$inside[i] ||
          any(line$y >= r$y[1] & line$y <= r$y[2])
      }
    }
    lab <- texts[
      texts$label == insight::format_value(out$level[i], digits = 1) &
        sign(texts$y) == out$side[i],
      ,
      drop = FALSE
    ]
    in_panel <- lab$x >= r$x[1] &
      lab$x <= r$x[2] &
      lab$y >= r$y[1] &
      lab$y <= r$y[2]
    out$labels_inside[i] <- sum(in_panel)
    out$label_at_max_hat[i] <- any(
      abs(lab$x[in_panel] - max(b$data[[1]]$x)) < 1e-12
    )
  }
  out
}

.expect_y_range_follows_residuals <- function(b, model) {
  res <- stats::rstandard(model)
  res <- range(res[is.finite(res)])
  yr <- .panel_ranges(b)$y
  margin <- 0.1 * diff(res)
  expect_lte(yr[1], res[1])
  expect_gte(yr[2], res[2])
  expect_gte(yr[1], res[1] - margin)
  expect_lte(yr[2], res[2] + margin)
}


test_that("outliers panel y range follows the residuals", {
  m1 <- stats::lm(y ~ x, data = .f1_data())
  m2 <- stats::lm(mpg ~ wt + hp, data = mtcars)
  for (model in list(m1, m2)) {
    for (entry in c("check_model", "check_outliers")) {
      b <- ggplot2::ggplot_build(.outliers_panel(model, entry))
      .expect_y_range_follows_residuals(b, model)
    }
  }
})


test_that("contour lines are drawn where they cross the panel", {
  # F3: a high-leverage outlier with a Cook's distance above the level
  m3 <- stats::lm(y ~ x, data = rbind(.f1_data(), data.frame(x = 6, y = -4)))
  expect_gt(max(stats::cooks.distance(m3)), 0.46)
  b <- ggplot2::ggplot_build(.outliers_panel(m3))
  s <- .contour_summary(b, cook_levels = 0.46)
  expect_true(any(s$inside))
  .expect_y_range_follows_residuals(b, m3)

  # F5: two levels, and only the lines of 0.5 reach the residuals
  m5 <- stats::glm(
    y ~ x,
    family = stats::poisson(),
    data = rbind(.f5_data(), data.frame(x = 3.5, y = 0))
  )
  b <- ggplot2::ggplot_build(.outliers_panel(m5, cook_levels = c(0.5, 1)))
  s <- .contour_summary(b, cook_levels = c(0.5, 1))
  expect_true(any(s$inside[s$level == 0.5]))
  expect_false(any(s$inside[s$level == 1]))
  .expect_y_range_follows_residuals(b, m5)
})


test_that("contour labels are inside the panel only for lines inside it", {
  f1 <- .f1_data()
  m5 <- stats::glm(
    y ~ x,
    family = stats::poisson(),
    data = rbind(.f5_data(), data.frame(x = 3.5, y = 0))
  )
  fixtures <- list(
    F1 = list(model = stats::lm(y ~ x, data = f1), levels = 0.46),
    F3 = list(
      model = stats::lm(y ~ x, data = rbind(f1, data.frame(x = 6, y = -4))),
      levels = 0.46
    ),
    # F4: a low-leverage residual that reaches the line at the largest
    # leverage but not at 80% of the leverage range
    F4 = list(
      model = stats::lm(y ~ x, data = rbind(f1, data.frame(x = 0, y = 6.8))),
      levels = 0.46
    ),
    F5 = list(model = m5, levels = c(0.5, 1))
  )
  for (f in names(fixtures)) {
    fx <- fixtures[[f]]
    b <- ggplot2::ggplot_build(.outliers_panel(
      fx$model,
      cook_levels = if (f == "F5") fx$levels
    ))
    s <- .contour_summary(b, cook_levels = fx$levels)
    expect_identical(s$labels_inside > 0, s$inside, label = f)
    expect_true(all(s$labels_inside <= 1), label = f)
  }

  b <- ggplot2::ggplot_build(.outliers_panel(fixtures$F4$model))
  s <- .contour_summary(b, cook_levels = 0.46)
  expect_identical(s$inside, c(TRUE, FALSE))
  expect_true(s$label_at_max_hat[s$side == 1])
})


test_that("outliers panel subtitle says when no contour line is inside", {
  f1 <- .f1_data()
  b <- ggplot2::ggplot_build(.outliers_panel(stats::lm(y ~ x, data = f1)))
  expect_identical(
    b$plot$labels$subtitle,
    "All points are inside the contour lines (outside the plot area)"
  )

  m3 <- stats::lm(y ~ x, data = rbind(f1, data.frame(x = 6, y = -4)))
  b <- ggplot2::ggplot_build(.outliers_panel(m3))
  expect_identical(
    b$plot$labels$subtitle,
    "Points should be inside the contour lines"
  )
})


test_that("a user coordinate system works on the outliers panel", {
  p <- .outliers_panel(stats::lm(y ~ x, data = .f1_data()))

  expect_no_message(expect_no_warning({
    b <- ggplot2::ggplot_build(p + ggplot2::coord_cartesian(ylim = c(-4, 4)))
  }))
  expect_equal(.panel_ranges(b)$y, c(-4.4, 4.4), tolerance = 1e-8)

  expect_no_message(expect_no_warning({
    b <- ggplot2::ggplot_build(
      p + ggplot2::coord_cartesian(xlim = c(0, 0.02))
    )
  }))
  .expect_y_range_follows_residuals(b, stats::lm(y ~ x, data = .f1_data()))
})


test_that("outliers panel without dots keeps the residual range", {
  # F1: without a layer for the residuals, the y axis follows the LOESS line,
  # about -0.9 to 0.3
  m1 <- stats::lm(y ~ x, data = .f1_data())
  b <- ggplot2::ggplot_build(.outliers_panel(
    m1,
    show_dots = FALSE,
    show_labels = FALSE
  ))
  .expect_y_range_follows_residuals(b, m1)

  m3 <- stats::lm(y ~ x, data = rbind(.f1_data(), data.frame(x = 6, y = -4)))
  b <- ggplot2::ggplot_build(.outliers_panel(
    m3,
    show_dots = FALSE,
    show_labels = FALSE
  ))
  expect_false(any(vapply(
    b$plot$layers,
    function(l) inherits(l$geom, "GeomPoint"),
    logical(1)
  )))
  .expect_y_range_follows_residuals(b, m3)
  expect_true(any(.contour_summary(b, cook_levels = 0.46)$inside))
  expect_identical(
    b$plot$labels$subtitle,
    "Points should be inside the contour lines"
  )
})


test_that("outliers panel subtitle makes no claim without contour lines", {
  cm <- suppressMessages(performance::check_model(
    stats::lm(y ~ x, data = .f1_data()),
    check = "outliers",
    residual_type = "normal"
  ))
  no_params <- cm
  attr(no_params$INFLUENTIAL, "n_params") <- NULL
  no_levels <- cm
  attr(no_levels$INFLUENTIAL, "cook_levels") <- NA_real_
  for (x in list(no_params, no_levels)) {
    # all-NA levels warn in the label count (min() of no values), as on
    # origin/main
    b <- suppressWarnings(ggplot2::ggplot_build(plot(x)))
    expect_identical(
      b$plot$labels$subtitle,
      "Points should be inside the contour lines"
    )
  }
})
