# VIF intervals set by hand, so that the tests do not depend on the interval
# formula in performance, which changed in easystats/performance#977
.vif_ci <- function(low, high) {
  data.frame(
    VIF_CI_low = low,
    VIF_CI_high = high,
    Tolerance_CI_low = 1 / high,
    Tolerance_CI_high = 1 / low
  )
}

# model with VIFs near 1, whose intervals had no upper limit before
# easystats/performance#977 (easystats/see#399)
.vif_inf_model <- function() {
  set.seed(1)
  d <- data.frame(y = rnorm(300), a = rnorm(300), b = rnorm(300))
  lm(y ~ a + b, data = d)
}

# VIFs 1.002; infinite upper bounds, as performance 0.18.2 gives them
.vif_inf_ci <- .vif_ci(c(1, 1), c(Inf, Inf))

.vif_inf <- function() {
  cc <- performance::check_collinearity(.vif_inf_model())
  attr(cc, "CI") <- .vif_inf_ci
  cc
}

.vif_inf_caption <- paste(
  "Some VIF intervals run off the top:",
  "the interval formula fails near VIF = 1. This is not high collinearity."
)

.vif_mtcars_model <- function() {
  lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
}

# the intervals of performance 0.18.2
.vif_mtcars_ci <- .vif_ci(
  c(3.209219, 3.42313, 1.186708, 6.082135),
  c(8.41114, 9.03919, 2.511318, 16.84704)
)

# VIFs wt 5.05, cyl 5.41, gear 1.53, disp 9.97; axis top 11.5
.vif_mtcars <- function() {
  cc <- performance::check_collinearity(.vif_mtcars_model())
  attr(cc, "CI") <- .vif_mtcars_ci
  cc
}

# the check_model() panel reads the intervals from attr(x$VIF, "CI")
.vif_check_model <- function(model, ci) {
  cm <- performance::check_model(model, check = "vif", panel = FALSE)
  attr(cm$VIF, "CI") <- ci
  cm
}

# the plot reads the VIFs from the stored copy in attr(x, "data")
.with_vif <- function(cc, term, vif = NULL, ci_high = NULL) {
  if (!is.null(vif)) {
    attr(cc, "data")$VIF[term] <- vif
  }
  if (!is.null(ci_high)) {
    attr(cc, "CI")$VIF_CI_high[term] <- ci_high
  }
  cc
}

# the y range (log10 units) and the lower edge of the "Low" band
.vif_axis <- function(p) {
  built <- ggplot2::ggplot_build(p)
  bands <- Filter(
    function(d) all(c("xmin", "ymin", "fill") %in% colnames(d)),
    built$data
  )
  low_band <- Filter(function(d) identical(d$fill[1], "#3aaf85"), bands)
  list(
    y_range = built$layout$panel_params[[1]]$y.range,
    band_min = low_band[[1]]$ymin
  )
}


test_that("`plot.see_check_collinearity()` works", {
  result <- .vif_mtcars()
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.check_collinearity",
    fig = plot(result)
  )
})


test_that("plot of VIF intervals running off the top has a caption", {
  cc <- .vif_inf()
  expect_identical(plot(cc)$labels$caption, .vif_inf_caption)
  expect_no_warning(ggplot2::ggplotGrob(plot(cc)))

  # one infinite upper bound is enough
  p <- plot(.with_vif(cc, 2, ci_high = 5))
  expect_identical(p$labels$caption, .vif_inf_caption)

  # finite upper bounds above the axis top
  p <- plot(.with_vif(cc, 1:2, ci_high = c(1280, 1280)))
  expect_identical(p$labels$caption, .vif_inf_caption)

  # VIF below 2, finite bound above the axis top of 11.5
  p <- plot(.with_vif(.vif_mtcars(), 3, vif = 1.9, ci_high = 12))
  expect_identical(p$labels$caption, .vif_inf_caption)

  # axis top 23 (largest VIF 20), bound above it
  cc <- .with_vif(.vif_mtcars(), 4, vif = 20)
  p <- plot(.with_vif(cc, 3, ci_high = 24))
  expect_identical(p$labels$caption, .vif_inf_caption)

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.check_collinearity infinite intervals",
    fig = plot(.vif_inf())
  )
})


test_that("plot without low VIFs running off the top has no caption", {
  cc <- .vif_mtcars()
  vif <- attr(cc, "data")$VIF
  ci_high <- attributes(cc)$CI$VIF_CI_high
  above <- ci_high > max(10, ceiling(max(vif))) * 1.15
  expect_true(any(above))
  expect_true(all(vif[above] >= 2))
  expect_null(plot(cc)$labels$caption)

  # infinite bound on a term with VIF 9.97
  expect_null(plot(.with_vif(cc, 4, ci_high = Inf))$labels$caption)
  # VIF between 2 and 5, bound above the axis top
  p <- plot(.with_vif(cc, 3, vif = 4.9, ci_high = 11.7))
  expect_null(p$labels$caption)
  # VIF exactly 2
  expect_null(plot(.with_vif(cc, 3, vif = 2, ci_high = 12))$labels$caption)
  # VIF below 2, bound above 10 but below the axis top of 11.5
  p <- plot(.with_vif(cc, 3, vif = 1.9, ci_high = 11))
  expect_null(p$labels$caption)
  # NaN bound, as performance 0.18.2 gives for VIF exactly 1
  expect_null(plot(.with_vif(cc, 3, ci_high = NaN))$labels$caption)
  # axis top 23 (largest VIF 20), bound below it
  cc <- .with_vif(cc, 4, vif = 20)
  expect_null(plot(.with_vif(cc, 3, ci_high = 15))$labels$caption)

  cc <- .vif_inf()
  attr(cc, "CI") <- NULL
  expect_null(plot(cc)$labels$caption)
})


test_that("check_model() VIF panel has the caption for infinite intervals", {
  p <- plot(.vif_check_model(.vif_inf_model(), .vif_inf_ci))
  expect_identical(
    gsub("\\s*\\n\\s*", " ", p$VIF$labels$caption),
    .vif_inf_caption
  )
  # two lines in the smaller panel, split at the colon
  expect_identical(
    strsplit(p$VIF$labels$caption, "\n", fixed = TRUE)[[1]][1],
    "Some VIF intervals run off the top:"
  )

  p <- plot(.vif_check_model(.vif_mtcars_model(), .vif_mtcars_ci))
  expect_null(p$VIF$labels$caption)
})


# rows of the arrow layer, one per interval bound above the axis top
.vif_n_arrows <- function(p) {
  segment <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomSegment"),
    logical(1)
  )
  stopifnot(sum(segment) == 1)
  nrow(ggplot2::layer_data(p, which(segment)))
}


test_that("missing upper bounds give no warning", {
  # terms 3 (VIF 1.53) and 4 (VIF 9.97, an arrow above the axis top)
  bounds <- list(
    nan_low_vif = list(term = 3, ci_high = NaN, arrows = 1L),
    na_low_vif = list(term = 3, ci_high = NA_real_, arrows = 1L),
    na_arrow = list(term = 4, ci_high = NA_real_, arrows = 0L),
    all_na = list(term = 1:4, ci_high = rep(NA_real_, 4), arrows = 0L)
  )
  for (b in bounds) {
    p <- plot(.with_vif(.vif_mtcars(), b$term, ci_high = b$ci_high))
    expect_no_warning(ggplot2::ggplotGrob(p))
    expect_identical(.vif_n_arrows(p), b$arrows)

    ci <- .vif_mtcars_ci
    ci$VIF_CI_high[b$term] <- b$ci_high
    p <- plot(.vif_check_model(.vif_mtcars_model(), ci))
    expect_no_warning(ggplot2::ggplotGrob(p$VIF))
    expect_identical(.vif_n_arrows(p$VIF), b$arrows)
  }
})


test_that("faceted plot has the caption only for low VIFs running off the top", {
  # two panels, as for a zero-inflated model: wt and cyl in the first, gear
  # (VIF 1.53) and disp (VIF 9.97) in the second; axis top 11.5
  cc <- .vif_mtcars()
  component <- rep(c("conditional", "zero inflated"), each = 2)
  attr(cc, "data")$Component <- component
  attr(cc, "CI")$Component <- component
  cc <- .with_vif(cc, 4, ci_high = 11)
  expect_length(unique(ggplot2::layer_data(plot(cc))$PANEL), 2)
  expect_null(plot(cc)$labels$caption)

  # low VIF in the second panel only, bound above the axis top
  p <- plot(.with_vif(cc, 3, ci_high = 12))
  expect_identical(p$labels$caption, .vif_inf_caption)

  # low VIF in the second panel, bound above the axis top only on wt
  # (VIF 5.05) in the first panel
  p <- plot(.with_vif(cc, 1, ci_high = 12))
  expect_null(p$labels$caption)
})


test_that("VIF axis extends below 1, and the Low band starts at 1", {
  plots <- list(
    plot(.vif_inf()),
    plot(.vif_mtcars()),
    plot(.vif_check_model(.vif_inf_model(), .vif_inf_ci))$VIF
  )
  for (p in plots) {
    axis <- .vif_axis(p)
    expect_lte(axis$y_range[1], -0.05 * diff(axis$y_range))
    expect_identical(axis$band_min, 0)
  }
})
