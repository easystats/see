test_that("`plot.see_estimate_contrasts()` works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("collapse")

  model <- stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "Species")
  means <- modelbased::estimate_means(model, by = "Species")
  expect_s3_class(plot(contrasts, means), "gg")
})


# contrasts of `am` within each level of `cyl`
contrasts_by_fixture <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$am <- factor(d$am, labels = c("auto", "manual"))
  model <- lm(mpg ~ cyl * am, data = d)
  list(
    data = d,
    model = model,
    means = modelbased::estimate_means(model, by = c("cyl", "am")),
    contrasts = modelbased::estimate_contrasts(
      model,
      contrast = "am",
      by = "cyl"
    )
  )
}

contrasts_iris <- function(data = iris) {
  model <- lm(Sepal.Width ~ Species, data = data)
  list(
    means = modelbased::estimate_means(model, by = "Species"),
    contrasts = modelbased::estimate_contrasts(model, contrast = "Species")
  )
}

layer_index <- function(p, geom) {
  which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
}


test_that("lighthouses of contrasts with `by` use the means of their `by` level", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  contrasts <- fx$contrasts
  means <- fx$means
  polygons <- data_plot(contrasts, means)$geom_polygon

  expect_identical(nrow(contrasts), 3L)
  expect_true("cyl" %in% names(polygons))
  expect_identical(nrow(polygons), 9L)

  for (i in seq_len(nrow(contrasts))) {
    level1 <- as.character(contrasts$Level1[i])
    level2 <- as.character(contrasts$Level2[i])
    cyl <- as.character(contrasts$cyl[i])
    rows <- polygons[
      as.character(polygons$Level1) == level1 &
        as.character(polygons$Level2) == level2 &
        as.character(polygons$cyl) == cyl,
    ]
    expect_identical(nrow(rows), 3L)

    mean1 <- means$Mean[
      as.character(means$am) == level1 & as.character(means$cyl) == cyl
    ]
    expect_length(mean1, 1)
    expect_equal(
      rows$y[as.character(rows$x) == level1],
      mean1,
      tolerance = 1e-8
    )
    expect_equal(
      sort(rows$y[as.character(rows$x) == level2]),
      sort(c(mean1 - contrasts$CI_high[i], mean1 - contrasts$CI_low[i])),
      tolerance = 1e-8
    )
  }
})


test_that("the x axis of lighthouse plots is the contrast variable, in level order", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  dp <- data_plot(fx$contrasts, fx$means)
  expect_identical(names(fx$means)[1], "cyl")
  expect_true(all(as.character(dp$geom_polygon$x) %in% c("auto", "manual")))
  expect_true(all(as.character(dp$geom_pointrange$x) %in% c("auto", "manual")))
  expect_identical(attr(dp, "info")$xlab, "am")

  built <- ggplot2::ggplot_build(plot(fx$contrasts, fx$means))
  for (panel_params in built$layout$panel_params) {
    expect_identical(panel_params$x$get_limits(), c("auto", "manual"))
  }

  ir <- contrasts_iris()
  polygons <- data_plot(ir$contrasts, ir$means)$geom_polygon
  expect_identical(as.vector(table(polygons$group)), rep(3L, nrow(ir$contrasts)))
  built <- ggplot2::ggplot_build(plot(ir$contrasts, ir$means))
  expect_identical(
    built$layout$panel_params[[1]]$x$get_limits(),
    c("setosa", "versicolor", "virginica")
  )

  iris2 <- iris
  iris2$Species <- factor(
    iris2$Species,
    levels = c("virginica", "setosa", "versicolor")
  )
  ir2 <- contrasts_iris(iris2)
  built <- ggplot2::ggplot_build(plot(ir2$contrasts, ir2$means))
  expect_identical(
    built$layout$panel_params[[1]]$x$get_limits(),
    c("virginica", "setosa", "versicolor")
  )
})


test_that("lighthouse plots of contrasts with `by` get one facet per `by` level", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  p <- plot(fx$contrasts, fx$means)
  expect_s3_class(p$facet, "FacetWrap")
  expect_identical(names(p$facet$params$facets), "cyl")

  built <- ggplot2::ggplot_build(p)
  expect_identical(nrow(built$layout$layout), 3L)
  polygons <- built$data[[layer_index(p, "GeomPolygon")]]
  pointranges <- built$data[[layer_index(p, "GeomPointrange")]]
  for (panel in 1:3) {
    expect_length(unique(polygons$group[polygons$PANEL == panel]), 1)
    expect_identical(sum(pointranges$PANEL == panel), 2L)
  }

  ir <- contrasts_iris()
  expect_s3_class(plot(ir$contrasts, ir$means)$facet, "FacetNull")
})


test_that("lighthouse plots stop for means without the `by` columns and for joint contrasts", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  means_am <- modelbased::estimate_means(fx$model, by = "am")
  expect_error(data_plot(fx$contrasts, means_am), regexp = "cyl")
  expect_error(data_plot(fx$contrasts, means_am), regexp = "lack")

  joint <- modelbased::estimate_contrasts(fx$model, contrast = c("am", "cyl"))
  expect_error(data_plot(joint, fx$means), regexp = "one contrast variable")
})


# three crossed factors with two levels each
contrasts_factorial_data <- function() {
  set.seed(1)
  s <- expand.grid(
    a = factor(c("a1", "a2")),
    b = factor(c("b1", "b2")),
    f = factor(c("f1", "f2")),
    rep = 1:10
  )
  s$y <- rnorm(nrow(s))
  s
}

polygon_rows_per_contrast <- function(polygons) {
  as.vector(table(polygons$group))
}


test_that("lighthouse plots accept `contrast` and `by` given with target values", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  model <- lm(Sepal.Width ~ Species, data = iris)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "Species=c('setosa','virginica')"
  )
  means <- modelbased::estimate_means(model, by = "Species")
  dp <- data_plot(contrasts, means)
  expect_identical(attr(dp, "info")$xlab, "Species")
  expect_identical(polygon_rows_per_contrast(dp$geom_polygon), 3L)

  d <- mtcars
  d$am <- factor(d$am, labels = c("auto", "manual"))
  model <- lm(mpg ~ am * wt, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    by = "wt=c(2,3)"
  )
  means <- modelbased::estimate_means(model, by = c("am", "wt=c(2,3)"))
  dp <- data_plot(contrasts, means)
  expect_identical(
    polygon_rows_per_contrast(dp$geom_polygon),
    rep(3L, nrow(contrasts))
  )
  p <- plot(contrasts, means)
  expect_identical(names(p$facet$params$facets), "wt")
})


test_that("lighthouse plots stop when the means have more rows than the contrasts can use", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  s <- contrasts_factorial_data()
  model <- lm(y ~ a * b * f, data = s)
  finer_means <- modelbased::estimate_means(model, by = c("a", "b", "f"))

  contrasts <- modelbased::estimate_contrasts(model, contrast = "a", by = "b")
  expect_error(data_plot(contrasts, finer_means), regexp = "more than one row")

  contrasts <- modelbased::estimate_contrasts(model, contrast = "a")
  expect_error(data_plot(contrasts, finer_means), regexp = "more than one row")

  fx <- contrasts_by_fixture()
  partial_means <- as.data.frame(fx$means)
  partial_means <- partial_means[partial_means$cyl != "8", ]
  expect_error(data_plot(fx$contrasts, partial_means), regexp = "no row")
})


test_that("lighthouse plots work with two, numeric, and character `by` variables", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  # two `by` variables, means in another column order
  s <- contrasts_factorial_data()
  model <- lm(y ~ a * b * f, data = s)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "a",
    by = c("b", "f")
  )
  means <- modelbased::estimate_means(model, by = c("f", "b", "a"))
  dp <- data_plot(contrasts, means)
  expect_identical(polygon_rows_per_contrast(dp$geom_polygon), rep(3L, 4))
  p <- plot(contrasts, means)
  expect_identical(names(p$facet$params$facets), c("b", "f"))
  expect_identical(nrow(ggplot2::ggplot_build(p)$layout$layout), 4L)

  # numeric `by` variable
  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "Species",
    by = "Petal.Length"
  )
  means <- modelbased::estimate_means(model, by = c("Species", "Petal.Length"))
  dp <- data_plot(contrasts, means)
  expect_identical(
    polygon_rows_per_contrast(dp$geom_polygon),
    rep(3L, nrow(contrasts))
  )

  # character contrast and `by` variables
  d <- mtcars
  d$cyl <- as.character(d$cyl)
  d$am <- ifelse(d$am == 1, "manual", "auto")
  model <- lm(mpg ~ cyl * am, data = d)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am", by = "cyl")
  means <- modelbased::estimate_means(model, by = c("cyl", "am"))
  dp <- data_plot(contrasts, means)
  expect_identical(polygon_rows_per_contrast(dp$geom_polygon), rep(3L, 3))
  expect_identical(levels(dp$geom_pointrange$x), c("auto", "manual"))
})
