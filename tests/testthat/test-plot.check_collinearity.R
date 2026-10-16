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
  "Some VIF intervals have no upper limit:",
  "the data cannot show how much collinearity there is."
)

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


test_that("plot of infinite VIF intervals has a caption", {
  cc <- performance::check_collinearity(.vif_inf_model())
  expect_true(all(is.infinite(attributes(cc)$CI$VIF_CI_high)))
  expect_identical(plot(cc)$labels$caption, .vif_inf_caption)

  # one infinite upper bound is enough
  attr(cc, "CI")$VIF_CI_high[2] <- 5
  expect_identical(plot(cc)$labels$caption, .vif_inf_caption)
})


test_that("plot of finite or missing VIF intervals has no caption", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  cc <- performance::check_collinearity(m)
  expect_false(any(is.infinite(attributes(cc)$CI$VIF_CI_high)))
  expect_null(plot(cc)$labels$caption)

  cc <- performance::check_collinearity(.vif_inf_model())
  attr(cc, "CI") <- NULL
  expect_null(plot(cc)$labels$caption)
})


test_that("check_model() VIF panel has the caption for infinite intervals", {
  p <- plot(performance::check_model(
    .vif_inf_model(),
    check = "vif",
    panel = FALSE
  ))
  expect_identical(
    gsub("\\s*\\n\\s*", " ", p$VIF$labels$caption),
    .vif_inf_caption
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
