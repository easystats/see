skip_if_not_installed("performance")

# Models whose fitted values take only a few distinct values (one per cell)
# get cell means in place of a LOESS smoother (#464).

.layer_geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1))
}

.has_smooth <- function(p) {
  any(vapply(p$layers, function(l) inherits(l$stat, "StatSmooth"), logical(1)))
}

.cell_means <- function(d) {
  m <- tapply(d$y, signif(d$x, 8), mean)
  as.vector(m[order(as.numeric(names(m)))])
}

few_value_models <- list(
  list(model = lm(mpg ~ factor(am), data = mtcars), k = 2),
  list(model = lm(Sepal.Length ~ Species, data = iris), k = 3),
  list(model = lm(mpg ~ factor(am) + factor(vs), data = mtcars), k = 4),
  list(model = lm(mpg ~ factor(am) * factor(vs), data = mtcars), k = 4)
)


test_that("few-value panels draw cell means and no smoother", {
  for (fm in few_value_models) {
    p <- plot(performance::check_model(fm$model, panel = FALSE))
    for (panel in c("NCV", "HOMOGENEITY")) {
      pp <- p[[panel]]
      expect_no_warning(built <- ggplot2::ggplot_build(pp))
      expect_false(.has_smooth(pp))

      geoms <- .layer_geoms(pp)
      means_point <- built$data[[max(which(geoms == "GeomPoint"))]]
      means_line <- built$data[[which(geoms == "GeomLine")]]
      expected <- .cell_means(pp$data)

      expect_identical(nrow(means_point), as.integer(fm$k))
      expect_identical(nrow(means_line), as.integer(fm$k))
      expect_equal(means_point$y[order(means_point$x)], expected, tolerance = 1e-8)
      expect_equal(means_line$y[order(means_line$x)], expected, tolerance = 1e-8)
    }
  }
})


.with_show_ci <- function(model, show_ci) {
  x <- performance::check_model(model, panel = FALSE)
  attr(x, "show_ci") <- show_ci
  plot(x)
}


test_that("few-value panels draw error bars only when show_ci is TRUE", {
  for (fm in few_value_models) {
    p_ci <- .with_show_ci(fm$model, TRUE)
    p_no <- .with_show_ci(fm$model, FALSE)
    for (panel in c("NCV", "HOMOGENEITY")) {
      geoms <- .layer_geoms(p_ci[[panel]])
      expect_identical(sum(geoms == "GeomErrorbar"), 1L)
      built <- ggplot2::ggplot_build(p_ci[[panel]])
      expect_identical(nrow(built$data[[which(geoms == "GeomErrorbar")]]), as.integer(fm$k))
      expect_false("GeomErrorbar" %in% .layer_geoms(p_no[[panel]]))
    }
  }

  # performance sets `show_ci = TRUE` for this model (its interaction term is
  # not of type factor), so the test does not override the attribute
  m <- lm(mpg ~ factor(am) * factor(vs), data = mtcars)
  p <- plot(performance::check_model(m, show_ci = TRUE, panel = FALSE))
  for (panel in c("NCV", "HOMOGENEITY")) {
    geoms <- .layer_geoms(p[[panel]])
    expect_identical(sum(geoms == "GeomErrorbar"), 1L)
    built <- ggplot2::ggplot_build(p[[panel]])
    expect_identical(nrow(built$data[[which(geoms == "GeomErrorbar")]]), 4L)
  }
})


test_that("a one-observation cell gets no error bar", {
  d <- data.frame(
    g = factor(c(rep("a", 5), rep("b", 5), "c")),
    y = c(1, 2, 3, 4, 5, 3, 4, 5, 6, 7, 10)
  )
  m <- lm(y ~ g, data = d)
  p <- .with_show_ci(m, TRUE)
  single_x <- signif(fitted(m)[d$g == "c"], 8)
  for (panel in c("NCV", "HOMOGENEITY")) {
    expect_no_warning(built <- ggplot2::ggplot_build(p[[panel]]))
    geoms <- .layer_geoms(p[[panel]])
    bars <- built$data[[which(geoms == "GeomErrorbar")]]
    expect_identical(nrow(bars), 2L)
    expect_false(any(signif(bars$x, 8) == single_x))
  }
})


test_that("heteroscedasticity and GLM homogeneity panels use cell means", {
  m <- lm(mpg ~ factor(am), data = mtcars)
  p_het <- plot(performance::check_heteroscedasticity(m))
  expect_no_warning(ggplot2::ggplot_build(p_het))
  expect_false(.has_smooth(p_het))

  g <- glm(am ~ factor(vs), family = binomial, data = mtcars)
  p_glm <- plot(performance::check_model(g, panel = FALSE))$HOMOGENEITY
  expect_no_warning(ggplot2::ggplot_build(p_glm))
  expect_false(.has_smooth(p_glm))
})


test_that("a cell with a fitted value of zero stays one cell", {
  d <- data.frame(
    g = factor(rep(c("a", "b", "c"), each = 10)),
    y = c(rep(c(-1, 1), 5) - 2, rep(c(-1, 1), 5), rep(c(-1, 1), 5) + 2)
  )
  d$y <- d$y + stats::rnorm(30, sd = 1e-10)
  d$y <- d$y - mean(d$y[d$g == "b"])
  m <- lm(y ~ g, data = d)
  expect_true(.few_unique_values(stats::fitted(m)))

  p <- plot(performance::check_model(m, panel = FALSE))
  geoms <- .layer_geoms(p$NCV)
  built <- ggplot2::ggplot_build(p$NCV)
  expect_false(.has_smooth(p$NCV))
  expect_identical(nrow(built$data[[which(geoms == "GeomLine")]]), 3L)
})


test_that("error bars do not overlap unevenly spaced cells", {
  layers <- .smoother_or_cell_means(
    data.frame(
      x = rep(c(0, 0.001, 0.002, 100), each = 5),
      y = rep(c(-1, 0, 1, 2, 3), 4)
    ),
    size_point = 2,
    size_line = 0.8
  )
  errorbar <- layers[[1]]
  expect_s3_class(errorbar$geom, "GeomErrorbar")
  expect_lte(errorbar$geom_params$width, 0.0005)

  # a single cell keeps ggplot2's default width
  layers <- .smoother_or_cell_means(
    data.frame(x = rep(1, 5), y = c(-1, 0, 1, 2, 3)),
    size_point = 2,
    size_line = 0.8
  )
  expect_null(layers[[1]]$geom_params$width)
})


test_that("cell means use all rows when the dots are downsampled", {
  d <- data.frame(
    g = factor(c(rep("a", 2500), rep("b", 2497), rep("c", 3))),
    y = c(
      rep(c(-1, 1), 1250),
      rep(c(-1, 1, 3), length.out = 2497) + 5,
      c(20, 22, 24)
    )
  )
  m <- lm(y ~ g, data = d)
  x <- performance::check_model(m, panel = FALSE)
  attr(x, "show_ci") <- TRUE
  p <- plot(x)
  for (panel in c("NCV", "HOMOGENEITY")) {
    pp <- p[[panel]]
    # the raw dots are downsampled to `maximum_dots`
    expect_lte(nrow(pp$data), 2000)
    geoms <- .layer_geoms(pp)
    built <- ggplot2::ggplot_build(pp)
    means_point <- built$data[[max(which(geoms == "GeomPoint"))]]
    bars <- built$data[[which(geoms == "GeomErrorbar")]]
    expect_identical(nrow(means_point), 3L)
    expect_identical(nrow(bars), 3L)
  }

  # group means by factor level, ordered by fitted value
  expected <- tapply(stats::residuals(m), d$g, mean)
  expected <- as.vector(expected[order(tapply(stats::fitted(m), d$g, mean))])
  means_point <- ggplot2::ggplot_build(p$NCV)$data[[
    max(which(.layer_geoms(p$NCV) == "GeomPoint"))
  ]]
  expect_equal(
    means_point$y[order(means_point$x)],
    expected,
    tolerance = 1e-8
  )
})


test_that(".few_unique_values() detects few distinct values", {
  set.seed(123)
  expect_true(.few_unique_values(rep(1:10, 2)))
  expect_true(.few_unique_values(rep(1:2, 2)))
  expect_true(.few_unique_values(rep(c(1, 2), 10) + 1e-12 * stats::runif(20)))

  expect_false(.few_unique_values(rep(1:11, 5)))
  expect_false(.few_unique_values(c(1:10, 1, 2)))
  expect_false(.few_unique_values(
    stats::fitted(lm(mpg ~ wt + hp + cyl, data = mtcars))
  ))
})
