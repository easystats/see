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
