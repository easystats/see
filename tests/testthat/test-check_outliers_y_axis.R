skip_on_cran()
skip_if_not_installed("performance")

set.seed(1)
x <- rnorm(400)
d <- data.frame(x = x, y = x + rnorm(400))
m_plain <- lm(y ~ x, data = d)
m_outlier <- lm(y ~ x, data = rbind(d, data.frame(x = 6, y = -4)))
inside <- "Points should be inside the contour lines"

.outliers_check <- function(model, ...) {
  suppressMessages(performance::check_model(
    model,
    check = "outliers",
    residual_type = "normal",
    ...
  ))
}

.annotations <- function(p, geom) {
  which(vapply(
    p$layers,
    function(l) inherits(l$geom, geom) && isFALSE(l$inherit.aes),
    logical(1)
  ))
}


test_that("outliers panel y range follows the residuals", {
  skip_if_not_installed("ggrepel")
  no_dots <- .outliers_check(m_plain, show_dots = FALSE)
  attr(no_dots, "show_labels") <- FALSE
  for (cm in list(.outliers_check(m_outlier), no_dots)) {
    res <- range(cm$INFLUENTIAL$Std_Residuals)
    y <- ggplot2::ggplot_build(plot(cm))$layout$panel_params[[1]]$y.range
    expect_lte(y[1], res[1])
    expect_gte(y[2], res[2])
    expect_lt(diff(y), 1.2 * diff(res))
  }
})


test_that("outliers panel draws only contour lines that reach the residuals", {
  m <- lm(y ~ x, data = rbind(d, data.frame(x = 0, y = 6.8)))
  p <- plot(.outliers_check(m))
  expect_length(.annotations(p, "GeomLine"), 1)
  b <- ggplot2::ggplot_build(p)
  label <- b$data[[.annotations(p, "GeomText")]]
  expect_identical(nrow(label), 1L)
  expect_identical(label$x, max(b$data[[1]]$x))
})


test_that("outliers panel subtitle says when no contour line is inside", {
  expect_identical(
    plot(.outliers_check(m_plain))$labels$subtitle,
    "All points are inside the contour lines (outside the plot area)"
  )
  expect_identical(plot(.outliers_check(m_outlier))$labels$subtitle, inside)
})


test_that("outliers panel subtitle makes no claim it cannot check", {
  cm <- .outliers_check(m_plain)
  no_params <- cm
  attr(no_params$INFLUENTIAL, "n_params") <- NULL
  expect_identical(plot(no_params)$labels$subtitle, inside)
  attr(cm$INFLUENTIAL, "cook_levels") <- NA_real_
  expect_warning(p <- plot(cm), "no non-missing arguments to min")
  expect_identical(p$labels$subtitle, inside)

  # a row left out by the sampling has a Cook's distance above 0.3
  cm <- .outliers_check(
    lm(y ~ x, data = rbind(d, data.frame(x = 4.5, y = 0.3))),
    maximum_dots = 100
  )
  attr(cm$INFLUENTIAL, "cook_levels") <- 0.3
  expect_identical(plot(cm)$labels$subtitle, inside)
})
