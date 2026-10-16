test_that("`plot.see_check_collinearity()` works", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- performance::check_collinearity(m)
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.check_collinearity",
    fig = plot(result)
  )
})


# model whose VIF intervals have no upper limit (easystats/see#399)
.vif_inf_model <- function() {
  set.seed(1)
  d <- data.frame(y = rnorm(300), a = rnorm(300), b = rnorm(300))
  lm(y ~ a + b, data = d)
}

.vif_inf_caption <- paste(
  "Some VIF intervals run off the top:",
  "the interval formula fails near VIF = 1. This is not high collinearity."
)

# VIFs wt 5.05, cyl 5.41, gear 1.53, disp 9.97; axis top 11.5
.vif_mtcars <- function() {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  performance::check_collinearity(m)
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


test_that("plot of VIF intervals running off the top has a caption", {
  # the infinite bounds come from the interval formula in performance;
  # a fixed formula must not break reverse dependency checks
  skip_on_cran()
  cc <- performance::check_collinearity(.vif_inf_model())
  expect_true(all(is.infinite(attributes(cc)$CI$VIF_CI_high)))
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
    fig = plot(performance::check_collinearity(.vif_inf_model()))
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
  # NaN bound, as performance gives for VIF exactly 1
  expect_null(plot(.with_vif(cc, 3, ci_high = NaN))$labels$caption)
  # axis top 23 (largest VIF 20), bound below it
  cc <- .with_vif(cc, 4, vif = 20)
  expect_null(plot(.with_vif(cc, 3, ci_high = 15))$labels$caption)

  cc <- performance::check_collinearity(.vif_inf_model())
  attr(cc, "CI") <- NULL
  expect_null(plot(cc)$labels$caption)
})


test_that("check_model() VIF panel has the caption for infinite intervals", {
  skip_on_cran()
  cm <- performance::check_model(.vif_inf_model(), check = "vif", panel = FALSE)
  expect_true(all(is.infinite(attributes(cm$VIF)$CI$VIF_CI_high)))
  p <- plot(cm)
  expect_identical(
    gsub("\\s*\\n\\s*", " ", p$VIF$labels$caption),
    .vif_inf_caption
  )
  # two lines in the smaller panel, split at the colon
  expect_identical(
    strsplit(p$VIF$labels$caption, "\n", fixed = TRUE)[[1]][1],
    "Some VIF intervals run off the top:"
  )

  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  p <- plot(performance::check_model(m, check = "vif", panel = FALSE))
  expect_null(p$VIF$labels$caption)
})


test_that("VIF axis extends below 1, and the Low band starts at 1", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  plots <- list(
    plot(performance::check_collinearity(.vif_inf_model())),
    plot(performance::check_collinearity(m)),
    plot(performance::check_model(
      .vif_inf_model(),
      check = "vif",
      panel = FALSE
    ))$VIF
  )
  for (p in plots) {
    axis <- .vif_axis(p)
    expect_lte(axis$y_range[1], -0.05 * diff(axis$y_range))
    expect_identical(axis$band_min, 0)
  }
})
