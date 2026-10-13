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


test_that("lighthouse plots stop for means without the `by` columns", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  means_am <- modelbased::estimate_means(fx$model, by = "am")
  expect_error(data_plot(fx$contrasts, means_am), regexp = "cyl")
  expect_error(data_plot(fx$contrasts, means_am), regexp = "lack")
})


# joint contrasts of `am` and `cyl`
joint_contrasts_fixture <- function(d) {
  model <- lm(mpg ~ cyl * am, data = d)
  list(
    means = modelbased::estimate_means(model, by = c("cyl", "am")),
    contrasts = modelbased::estimate_contrasts(model, contrast = c("am", "cyl"))
  )
}


test_that("joint contrasts get one lighthouse per contrast from the matching means", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  jf <- joint_contrasts_fixture(contrasts_by_fixture()$data)
  jc <- jf$contrasts
  means <- jf$means
  polygons <- data_plot(jc, means)$geom_polygon

  expect_identical(nrow(jc), 15L)
  expect_identical(as.vector(table(polygons$group)), rep(3L, 15))

  means_labels <- paste(means$am, means$cyl, sep = ", ")
  for (i in seq_len(nrow(jc))) {
    level1 <- as.character(jc$Level1[i])
    level2 <- as.character(jc$Level2[i])
    rows <- polygons[polygons$group == i, ]
    mean1 <- means$Mean[means_labels == level1]
    expect_length(mean1, 1)
    expect_equal(
      rows$y[as.character(rows$x) == level1],
      mean1,
      tolerance = 1e-8
    )
    expect_equal(
      sort(rows$y[as.character(rows$x) == level2]),
      sort(c(mean1 - jc$CI_high[i], mean1 - jc$CI_low[i])),
      tolerance = 1e-8
    )
  }
})


test_that("the x axis of joint contrasts shows combined levels in level order", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  x_limits <- function(jf) {
    built <- ggplot2::ggplot_build(plot(jf$contrasts, jf$means))
    built$layout$panel_params[[1]]$x$get_limits()
  }

  jf <- joint_contrasts_fixture(d)
  expect_identical(
    x_limits(jf),
    c("auto, 4", "auto, 6", "auto, 8", "manual, 4", "manual, 6", "manual, 8")
  )
  expect_identical(attr(data_plot(jf$contrasts, jf$means), "info")$xlab, "am, cyl")

  d_am <- d
  d_am$am <- factor(d_am$am, levels = c("manual", "auto"))
  expect_identical(
    x_limits(joint_contrasts_fixture(d_am)),
    c("manual, 4", "manual, 6", "manual, 8", "auto, 4", "auto, 6", "auto, 8")
  )

  d_cyl <- d
  d_cyl$cyl <- factor(d_cyl$cyl, levels = c("8", "4", "6"))
  expect_identical(
    x_limits(joint_contrasts_fixture(d_cyl)),
    c("auto, 8", "auto, 4", "auto, 6", "manual, 8", "manual, 4", "manual, 6")
  )
})


test_that("joint contrasts without matching means give one warning", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  jf <- joint_contrasts_fixture(contrasts_by_fixture()$data)
  partial_means <- as.data.frame(jf$means)[-1, ]
  warnings <- testthat::capture_warnings(
    dp <- data_plot(jf$contrasts, partial_means)
  )
  expect_length(warnings, 1)
  expect_length(gregexpr("are not drawn", warnings)[[1]], 1)
  expect_match(warnings, "5 of 15 contrasts are not drawn")
  expect_match(warnings, "combinations of levels of am, cyl")
  expect_identical(as.vector(table(dp$geom_polygon$group)), rep(3L, 10))
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


test_that("joint contrasts with `by` get one facet per `by` level", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  s <- contrasts_factorial_data()
  m3 <- lm(y ~ a * b * f, data = s)
  means3 <- modelbased::estimate_means(m3, by = c("a", "b", "f"))
  jc3 <- modelbased::estimate_contrasts(m3, contrast = c("a", "b"), by = "f")

  p <- plot(jc3, means3)
  expect_s3_class(p$facet, "FacetWrap")
  expect_identical(names(p$facet$params$facets), "f")

  built <- ggplot2::ggplot_build(p)
  expect_identical(nrow(built$layout$layout), 2L)
  polygons <- built$data[[layer_index(p, "GeomPolygon")]]
  pointranges <- built$data[[layer_index(p, "GeomPointrange")]]
  for (panel in 1:2) {
    expect_length(unique(polygons$group[polygons$PANEL == panel]), 6)
    expect_identical(sum(pointranges$PANEL == panel), 4L)
  }

  polygons <- data_plot(jc3, means3)$geom_polygon
  expect_identical(nrow(jc3), 12L)
  expect_identical(polygon_rows_per_contrast(polygons), rep(3L, 12))

  means_labels <- paste(means3$a, means3$b, sep = ", ")
  for (i in seq_len(nrow(jc3))) {
    level1 <- as.character(jc3$Level1[i])
    rows <- polygons[polygons$group == i, ]
    mean1 <- means3$Mean[
      means_labels == level1 & as.character(means3$f) == as.character(jc3$f[i])
    ]
    expect_length(mean1, 1)
    expect_equal(
      rows$y[as.character(rows$x) == level1],
      mean1,
      tolerance = 1e-8
    )
  }
})


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


test_that("lighthouse plots stop when the means have extra rows and warn when rows are missing", {
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
  expect_warning(
    dp <- data_plot(fx$contrasts, partial_means),
    regexp = "1 of 3 contrasts are not drawn"
  )
  expect_identical(polygon_rows_per_contrast(dp$geom_polygon), rep(3L, 2))
})


test_that("lighthouse plots warn on trt_vs_ctrl and stop on contrasts without Level1 and Level2", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  model <- lm(mpg ~ cyl, data = fx$data)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "cyl",
    comparison = "trt_vs_ctrl"
  )
  means <- modelbased::estimate_means(model, by = "cyl")
  expect_warning(
    dp <- data_plot(contrasts, means),
    regexp = "1 of 1 contrasts are not drawn"
  )
  expect_identical(nrow(dp$geom_polygon), 0L)

  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "cyl",
    comparison = "poly"
  )
  expect_error(data_plot(contrasts, means), regexp = "Level1")
})


test_that("lighthouse plots keep `by` and contrast variables named like plot columns", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  s <- contrasts_factorial_data()
  s <- data.frame(a = s$a, x = s$b, y = s$f, out = s$y)
  model <- lm(out ~ a * x * y, data = s)

  for (by in c("x", "y")) {
    contrasts <- modelbased::estimate_contrasts(model, contrast = "a", by = by)
    means <- modelbased::estimate_means(model, by = c("a", by))
    dp <- data_plot(contrasts, means)
    expect_identical(polygon_rows_per_contrast(dp$geom_polygon), rep(3L, 2))
    built <- ggplot2::ggplot_build(plot(contrasts, means))
    expect_identical(nrow(built$layout$layout), 2L)
  }

  # the contrasted variable is named y
  contrasts <- modelbased::estimate_contrasts(model, contrast = "y", by = "a")
  means <- modelbased::estimate_means(model, by = c("a", "y"))
  polygons <- data_plot(contrasts, means)$geom_polygon
  for (i in seq_len(nrow(contrasts))) {
    level1 <- as.character(contrasts$Level1[i])
    a <- as.character(contrasts$a[i])
    tip <- polygons$y[
      polygons$group == i & as.character(polygons$x) == level1
    ]
    expected <- means$Mean[
      as.character(means$y) == level1 & as.character(means$a) == a
    ]
    expect_equal(tip, expected, tolerance = 1e-8)
  }
})


test_that("the suggested estimate_means() call can be pasted", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  model <- lm(Sepal.Width ~ Species, data = iris)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = 'Species=c("setosa","virginica")'
  )
  means <- as.data.frame(modelbased::estimate_means(model, by = "Species"))
  names(means)[1] <- "Group"
  expect_error(
    data_plot(contrasts, means),
    regexp = 'c("Species=c(\\"setosa\\",\\"virginica\\")")',
    fixed = TRUE
  )
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


test_that("`show_data = TRUE` draws the raw data first, jittered within each facet", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  d <- fx$data

  p <- plot(fx$contrasts, fx$means, show_data = TRUE)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p$layers[[1]]$position, "PositionJitter")
  expect_identical(p$layers[[1]]$position$height, 0)

  built <- ggplot2::ggplot_build(p)
  panels <- built$layout$layout
  points <- built$data[[1]]
  for (cyl in c("4", "6", "8")) {
    panel <- panels$PANEL[as.character(panels$cyl) == cyl]
    expect_length(panel, 1)
    y <- points$y[points$PANEL == panel]
    expect_length(y, c("4" = 11L, "6" = 7L, "8" = 14L)[[cyl]])
    expect_equal(sort(y), sort(d$mpg[d$cyl == cyl]), tolerance = 1e-8)
  }

  p_default <- plot(fx$contrasts, fx$means)
  expect_length(p_default$layers, 2)
  expect_s3_class(p_default$layers[[1]]$geom, "GeomPolygon")
  expect_s3_class(p_default$layers[[2]]$geom, "GeomPointrange")
})


test_that("`show_data = TRUE` puts joint contrasts' raw data at their combined levels", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  jf <- joint_contrasts_fixture(contrasts_by_fixture()$data)
  p <- plot(jf$contrasts, jf$means, show_data = TRUE)
  raw <- p$layers[[1]]$data
  expect_identical(nrow(raw), 32L)
  expect_identical(as.character(raw$x), paste(raw$am, raw$cyl, sep = ", "))
})


test_that("`show_data = TRUE` stops for a transformed response and a numeric `by`", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  model <- lm(log(mpg) ~ am, data = d)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am")
  means <- modelbased::estimate_means(model, by = "am")
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "transform")
  expect_no_error(plot(contrasts, means))

  model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "Species",
    by = "Petal.Length"
  )
  means <- modelbased::estimate_means(model, by = c("Species", "Petal.Length"))
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "Petal.Length")
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "numeric")
  expect_no_error(plot(contrasts, means))
})


test_that("`show_data = TRUE` draws back-transformed means and a numeric `by` on the means' grid", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  model <- lm(log(mpg) ~ am, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    transform = TRUE
  )
  means <- modelbased::estimate_means(model, by = "am", transform = TRUE)
  p <- plot(contrasts, means, show_data = TRUE)
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_equal(sort(p$layers[[1]]$data$y), sort(d$mpg), tolerance = 1e-8)

  # `cyl` stays numeric; modelbased estimates at its values 4, 6, and 8
  d_num <- mtcars
  d_num$am <- factor(d_num$am, labels = c("auto", "manual"))
  model <- lm(mpg ~ cyl * am, data = d_num)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am", by = "cyl")
  means <- modelbased::estimate_means(model, by = c("cyl", "am"))
  p <- plot(contrasts, means, show_data = TRUE)
  built <- ggplot2::ggplot_build(p)
  expect_identical(nrow(built$layout$layout), 3L)
  panels <- built$layout$layout
  points <- built$data[[1]]
  for (cyl in c(4, 6, 8)) {
    panel <- panels$PANEL[panels$cyl == cyl]
    expect_equal(
      sort(points$y[points$PANEL == panel]),
      sort(d_num$mpg[d_num$cyl == cyl]),
      tolerance = 1e-8
    )
  }
})


test_that("`show_data = TRUE` stops when the contrasts or means are not on the scale of the raw data", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  plot_data <- function(model, ...) {
    contrasts <- modelbased::estimate_contrasts(model, contrast = "am", ...)
    means <- modelbased::estimate_means(model, by = "am", ...)
    expect_no_error(plot(contrasts, means))
    plot(contrasts, means, show_data = TRUE)
  }

  # insight does not recognize this transformation
  expect_null(insight::find_transformation(lm(I(mpg - 10) ~ am, data = d)))
  expect_error(plot_data(lm(I(mpg - 10) ~ am, data = d)), regexp = "transform")
  expect_error(
    plot_data(lm(I(mpg - 10) ~ am, data = d), transform = TRUE),
    regexp = "transform"
  )
  # insight calls this transformation "identity"
  expect_identical(
    insight::find_transformation(lm(scale(mpg) ~ am, data = d)),
    "identity"
  )
  expect_error(plot_data(lm(scale(mpg) ~ am, data = d)), regexp = "transform")
  # means on the link scale
  model <- glm(gear ~ am, data = d, family = poisson)
  expect_error(plot_data(model, predict = "link"), regexp = "predict")
  expect_no_error(plot_data(model))

  # contrasts and means estimated on different scales; the messages are
  # wrapped, so they are matched with single spaces
  error_message <- function(expr) {
    gsub("\\s+", " ", conditionMessage(expect_error(expr)))
  }
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am")
  means_link <- modelbased::estimate_means(model, by = "am", predict = "link")
  expect_match(
    error_message(plot(contrasts, means_link, show_data = TRUE)),
    "the means were estimated with `predict = \"link\"`",
    fixed = TRUE
  )
  model <- lm(log(mpg) ~ am, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    transform = TRUE
  )
  means <- modelbased::estimate_means(model, by = "am")
  expect_match(
    error_message(plot(contrasts, means, show_data = TRUE)),
    "the means are not back-transformed",
    fixed = TRUE
  )

  # means in a data frame without the attributes are taken as estimated like
  # the contrasts
  means_plain <- data.frame(
    am = means$am,
    Mean = means$Mean,
    CI_low = means$CI_low,
    CI_high = means$CI_high
  )
  expect_null(attr(means_plain, "transform", exact = TRUE))
  expect_no_error(plot(contrasts, means_plain, show_data = TRUE))
})


test_that("`show_data = TRUE` stops for data values the means lack", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  model <- lm(Sepal.Width ~ Species, data = iris)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "Species=c('setosa','virginica')"
  )
  means <- modelbased::estimate_means(
    model,
    by = "Species=c('setosa','virginica')"
  )
  expect_no_error(plot(contrasts, means))
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "versicolor")
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "Species")

  d <- mtcars
  d$am <- factor(d$am, labels = c("auto", "manual"))
  model <- lm(mpg ~ am * wt, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    by = "wt=c(2,3)"
  )
  means <- modelbased::estimate_means(model, by = c("am", "wt=c(2,3)"))
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "wt")
  expect_error(plot(contrasts, means, show_data = TRUE), regexp = "numeric")
})


test_that("`show_data` must be TRUE or FALSE and needs the contrasts with their model", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  for (value in list("yes", NA, c(TRUE, FALSE), 1)) {
    expect_error(
      plot(fx$contrasts, fx$means, show_data = value),
      regexp = "`show_data` must be"
    )
  }

  dp <- data_plot(fx$contrasts, fx$means)
  expect_error(plot(dp, show_data = TRUE), regexp = "estimate_contrasts")
  expect_length(plot(dp)$layers, 2)

  no_model <- fx$contrasts
  attr(no_model, "model") <- NULL
  expect_error(
    plot(no_model, fx$means, show_data = TRUE),
    regexp = "the contrasts do not carry it"
  )

  no_response <- fx$contrasts
  attr(no_response, "response") <- "not_a_column"
  expect_error(
    plot(no_response, fx$means, show_data = TRUE),
    regexp = "not_a_column"
  )
})
