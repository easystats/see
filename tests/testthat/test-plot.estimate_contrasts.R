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

  expect_shape(contrasts, nrow = 3L)
  expect_true("cyl" %in% names(polygons))
  expect_shape(polygons, nrow = 9L)

  for (i in seq_len(nrow(contrasts))) {
    level1 <- as.character(contrasts$Level1[i])
    level2 <- as.character(contrasts$Level2[i])
    cyl <- as.character(contrasts$cyl[i])
    rows <- polygons[
      as.character(polygons$Level1) == level1 &
        as.character(polygons$Level2) == level2 &
        as.character(polygons$cyl) == cyl,
    ]
    expect_shape(rows, nrow = 3L)

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
  expect_named(p$facet$params$facets, "cyl")

  built <- ggplot2::ggplot_build(p)
  expect_shape(built$layout$layout, nrow = 3L)
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

  expect_shape(jc, nrow = 15L)
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
  warnings <- testthat::capture_warnings({
    dp <- data_plot(jf$contrasts, partial_means)
  })
  expect_length(warnings, 1)
  expect_length(gregexpr("are not drawn", warnings, fixed = TRUE)[[1]], 1)
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
  expect_named(p$facet$params$facets, "f")

  built <- ggplot2::ggplot_build(p)
  expect_shape(built$layout$layout, nrow = 2L)
  polygons <- built$data[[layer_index(p, "GeomPolygon")]]
  pointranges <- built$data[[layer_index(p, "GeomPointrange")]]
  for (panel in 1:2) {
    expect_length(unique(polygons$group[polygons$PANEL == panel]), 6)
    expect_identical(sum(pointranges$PANEL == panel), 4L)
  }

  polygons <- data_plot(jc3, means3)$geom_polygon
  expect_shape(jc3, nrow = 12L)
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
  expect_named(p$facet$params$facets, "wt")
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
    {
      dp <- data_plot(fx$contrasts, partial_means)
    },
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
    {
      dp <- data_plot(contrasts, means)
    },
    regexp = "1 of 1 contrasts are not drawn"
  )
  expect_shape(dp$geom_polygon, nrow = 0L)

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
    expect_shape(built$layout$layout, nrow = 2L)
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
  expect_named(p$facet$params$facets, c("b", "f"))
  expect_shape(ggplot2::ggplot_build(p)$layout$layout, nrow = 4L)

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


# messages are wrapped, so they are matched with single spaces
error_text <- function(expr) {
  gsub("\\s+", " ", conditionMessage(expect_error(expr)))
}

# each lighthouse starts at the estimate of Level1 and spans, at Level2, that
# estimate minus the interval of the difference ("difference") or divided by
# the interval of the ratio ("ratio")
expect_lighthouses <- function(
  polygons,
  contrasts,
  means,
  x_name,
  y_name = "Mean",
  geometry = "difference",
  by = NULL
) {
  for (i in seq_len(nrow(contrasts))) {
    level1 <- as.character(contrasts$Level1[i])
    level2 <- as.character(contrasts$Level2[i])
    rows <- polygons[polygons$group == i, ]
    expect_shape(rows, nrow = 3L)

    keep <- as.character(means[[x_name]]) == level1
    for (b in by) {
      keep <- keep & as.character(means[[b]]) == as.character(contrasts[[b]][i])
    }
    estimate1 <- means[[y_name]][keep]
    expect_length(estimate1, 1)
    expect_equal(
      rows$y[as.character(rows$x) == level1],
      estimate1,
      tolerance = 1e-8
    )

    ci <- c(contrasts$CI_low[i], contrasts$CI_high[i])
    tips <- if (geometry == "ratio") estimate1 / ci else estimate1 - ci
    expect_equal(
      sort(rows$y[as.character(rows$x) == level2]),
      sort(tips),
      tolerance = 1e-8
    )
  }
}

# a logistic model of vs, with probabilities as the means
binomial_fixture <- function(d = contrasts_by_fixture()$data, ...) {
  model <- glm(vs ~ am, data = d, family = binomial)
  list(
    model = model,
    means = modelbased::estimate_means(model, by = "am", ...),
    contrasts = modelbased::estimate_contrasts(model, contrast = "am", ...)
  )
}


test_that("lighthouse plots draw probabilities and take the estimate column from `coef_name`", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  bf <- binomial_fixture()
  bcon <- bf$contrasts
  bmeans <- bf$means
  dp <- data_plot(bcon, bmeans)
  expect_equal(dp$geom_pointrange$y, bmeans$Probability, tolerance = 1e-8)
  expect_identical(attr(dp, "info")$ylab, "Probability")
  expect_lighthouses(dp$geom_polygon, bcon, bmeans, "am", y_name = "Probability")
  expect_no_error(ggplot2::ggplot_build(plot(bcon, bmeans)))

  # means without attributes use the list of column names
  plain <- data.frame(
    am = bmeans$am,
    Probability = bmeans$Probability,
    CI_low = bmeans$CI_low,
    CI_high = bmeans$CI_high
  )
  expect_null(attr(plain, "coef_name", exact = TRUE))
  expect_equal(
    data_plot(bcon, plain)$geom_pointrange$y,
    bmeans$Probability,
    tolerance = 1e-8
  )

  rate <- plain
  names(rate)[2] <- "Rate"
  expect_equal(data_plot(bcon, rate)$geom_pointrange$y, rate$Rate, tolerance = 1e-8)

  # `coef_name` comes before the list, in which Median comes before Mean
  two <- data.frame(
    am = plain$am,
    Median = plain$Probability + 1,
    Mean = plain$Probability,
    CI_low = plain$CI_low,
    CI_high = plain$CI_high
  )
  attr(two, "coef_name") <- "Mean"
  expect_equal(data_plot(bcon, two)$geom_pointrange$y, two$Mean, tolerance = 1e-8)

  unknown <- plain
  names(unknown)[2] <- "Estimate"
  message <- error_text(plot(bcon, unknown))
  expect_match(message, "Probability", fixed = TRUE)
  expect_match(message, "Rate", fixed = TRUE)
  expect_match(message, "coef_name", fixed = TRUE)
})


test_that("lighthouse plots draw ratio contrasts and leave out ratios whose interval includes 0", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  rc <- modelbased::estimate_contrasts(
    fx$model,
    contrast = "am",
    by = "cyl",
    comparison = ratio ~ pairwise
  )
  expect_true("Ratio" %in% names(rc))
  polygons <- data_plot(rc, fx$means)$geom_polygon
  expect_identical(polygon_rows_per_contrast(polygons), rep(3L, 3))
  expect_lighthouses(
    polygons,
    rc,
    fx$means,
    "am",
    geometry = "ratio",
    by = "cyl"
  )

  # the mean of a is near 0, so the ratios of b and c to a have intervals
  # that include 0
  set.seed(1)
  z <- data.frame(
    g = factor(rep(c("a", "b", "c"), each = 10)),
    y = c(rnorm(10, 0.1), rnorm(10, 2), rnorm(10, 3))
  )
  zm <- lm(y ~ g, data = z)
  zc <- modelbased::estimate_contrasts(
    zm,
    contrast = "g",
    comparison = ratio ~ pairwise
  )
  zmeans <- modelbased::estimate_means(zm, by = "g")
  includes_0 <- zc$CI_low <= 0 & zc$CI_high >= 0
  expect_identical(sum(includes_0), 2L)

  warnings <- testthat::capture_warnings({
    dp <- data_plot(zc, zmeans)
  })
  expect_length(warnings, 1)
  warning_text <- gsub("\\s+", " ", warnings)
  expect_match(warning_text, "ratio", fixed = TRUE)
  expect_match(warning_text, "includes 0", fixed = TRUE)
  expect_match(warning_text, "2 of 3", fixed = TRUE)
  expect_shape(dp$geom_polygon, nrow = 3L)
  expect_identical(unique(dp$geom_polygon$Level1), "c")
  expect_identical(unique(dp$geom_polygon$Level2), "b")
})


test_that("lighthouse plots draw back-transformed log contrasts as ratios", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  # `transform = FALSE` is not accepted, so the default is left out
  estimates <- function(model, transform = TRUE) {
    transform_arg <- if (transform) list(transform = TRUE)
    list(
      contrasts = do.call(
        modelbased::estimate_contrasts,
        c(list(model, contrast = "am"), transform_arg)
      ),
      means = do.call(
        modelbased::estimate_means,
        c(list(model, by = "am"), transform_arg)
      )
    )
  }
  expect_geometry <- function(est, geometry) {
    polygons <- data_plot(est$contrasts, est$means)$geom_polygon
    expect_identical(polygon_rows_per_contrast(polygons), 3L)
    expect_lighthouses(
      polygons,
      est$contrasts,
      est$means,
      "am",
      geometry = geometry
    )
  }

  lg <- lm(log(mpg) ~ am, data = d)
  log_case <- estimates(lg)
  expect_geometry(log_case, "ratio")
  expect_geometry(estimates(lm(log10(mpg) ~ am, data = d)), "ratio")
  expect_geometry(estimates(lm(log2(mpg) ~ am, data = d)), "ratio")
  expect_geometry(estimates(lm(log(mpg, base = 2) ~ am, data = d)), "ratio")
  expect_geometry(estimates(lm(mpg ~ am, data = d)), "difference")
  # insight reports scale() as the identity
  scaled <- lm(scale(mpg) ~ am, data = d)
  expect_identical(insight::find_transformation(scaled), "identity")
  expect_geometry(estimates(scaled), "difference")
  expect_geometry(estimates(lg, transform = FALSE), "difference")

  sqrt_case <- estimates(lm(sqrt(mpg) ~ am, data = d))
  expect_match(
    error_text(plot(sqrt_case$contrasts, sqrt_case$means)),
    "sqrt",
    fixed = TRUE
  )
  shifted <- estimates(lm(I(mpg - 10) ~ am, data = d))
  expect_match(
    error_text(plot(shifted$contrasts, shifted$means)),
    "unknown",
    fixed = TRUE
  )

  # contrasts and means on different scales
  for (message in c(
    error_text(plot(
      log_case$contrasts,
      modelbased::estimate_means(lg, by = "am")
    )),
    error_text(plot(
      modelbased::estimate_contrasts(lg, contrast = "am"),
      log_case$means
    ))
  )) {
    expect_match(message, "`transform`", fixed = TRUE)
    expect_match(message, "estimated means", fixed = TRUE)
  }

  bf <- binomial_fixture()
  bcon_link <- modelbased::estimate_contrasts(
    bf$model,
    contrast = "am",
    predict = "link"
  )
  message <- error_text(plot(bcon_link, bf$means))
  expect_match(message, "`predict`", fixed = TRUE)
  expect_match(message, "estimated means", fixed = TRUE)
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
  expect_shape(raw, nrow = 32L)
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


test_that("`show_data = TRUE` draws a numeric `by` on the means' grid", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  # `cyl` stays numeric; modelbased estimates at its values 4, 6, and 8
  d_num <- mtcars
  d_num$am <- factor(d_num$am, labels = c("auto", "manual"))
  model <- lm(mpg ~ cyl * am, data = d_num)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am", by = "cyl")
  means <- modelbased::estimate_means(model, by = c("cyl", "am"))
  p <- plot(contrasts, means, show_data = TRUE)
  built <- ggplot2::ggplot_build(p)
  expect_shape(built$layout$layout, nrow = 3L)
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
    contrast_est <- modelbased::estimate_contrasts(model, contrast = "am", ...)
    means <- modelbased::estimate_means(model, by = "am", ...)
    expect_no_error(plot(contrast_est, means))
    plot(contrast_est, means, show_data = TRUE)
  }

  # insight does not recognize this transformation
  expect_null(insight::find_transformation(lm(I(mpg - 10) ~ am, data = d)))
  expect_error(plot_data(lm(I(mpg - 10) ~ am, data = d)), regexp = "transform")
  # with `transform = TRUE`, the plot without `show_data` already stops
  shifted <- lm(I(mpg - 10) ~ am, data = d)
  expect_error(
    plot(
      modelbased::estimate_contrasts(shifted, contrast = "am", transform = TRUE),
      modelbased::estimate_means(shifted, by = "am", transform = TRUE)
    ),
    regexp = "unknown"
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

  # contrasts and means estimated on different scales
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am")
  means_link <- modelbased::estimate_means(model, by = "am", predict = "link")
  expect_match(
    error_text(plot(contrasts, means_link)),
    "the estimated means with `predict = \"link\"`",
    fixed = TRUE
  )
  # means in a data frame without the attributes are taken as estimated like
  # the contrasts
  means_plain <- data.frame(
    am = means_link$am,
    Mean = means_link$Mean,
    CI_low = means_link$CI_low,
    CI_high = means_link$CI_high
  )
  expect_null(attr(means_plain, "predict", exact = TRUE))
  expect_no_error(plot(contrasts, means_plain, show_data = TRUE))

  # back-transformed contrasts of a log response are ratios of the
  # back-transformed means; without `transform`, the contrasts and means of a
  # log response are on the log scale, which the raw data are not
  model <- lm(log(mpg) ~ am, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    transform = TRUE
  )
  means <- modelbased::estimate_means(model, by = "am", transform = TRUE)
  expect_equal(
    contrasts$Difference,
    exp(stats::coef(model)[["ammanual"]]),
    tolerance = 1e-6
  )
  expect_equal(
    contrasts$Difference,
    means$Mean[means$am == "manual"] / means$Mean[means$am == "auto"],
    tolerance = 1e-6
  )
  # means in a data frame do not say whether they are back-transformed
  means_plain <- data.frame(
    am = means$am,
    Mean = means$Mean,
    CI_low = means$CI_low,
    CI_high = means$CI_high
  )
  expect_no_error(plot(contrasts, means_plain))
  expect_match(
    error_text(plot(contrasts, means_plain, show_data = TRUE)),
    "the means do not carry the `transform` attribute",
    fixed = TRUE
  )
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am")
  means <- modelbased::estimate_means(model, by = "am")
  expect_no_error(plot(contrasts, means))
  expect_match(
    error_text(plot(contrasts, means, show_data = TRUE)),
    "transforms the response",
    fixed = TRUE
  )

  # with `transform = TRUE`, the plot without `show_data` already stops for
  # this response
  model <- lm(exp(mpg / 10) ~ am, data = d)
  expect_error(
    plot(
      modelbased::estimate_contrasts(model, contrast = "am", transform = TRUE),
      modelbased::estimate_means(model, by = "am", transform = TRUE)
    ),
    regexp = "`exp`",
    fixed = TRUE
  )
})


test_that("`show_data = TRUE` draws binary responses, ratio contrasts, and back-transformed log contrasts", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  raw_y <- function(contrasts, means) {
    plot(contrasts, means, show_data = TRUE)$layers[[1]]$data$y
  }

  bf <- binomial_fixture(d)
  expect_equal(raw_y(bf$contrasts, bf$means), d$vs, tolerance = 1e-8)

  # a two-level factor and a logical response are drawn as 0 and 1
  d$vsf <- factor(d$vs, labels = c("V", "S"))
  d$vsl <- d$vs == 1
  for (model in list(
    glm(vsf ~ am, data = d, family = binomial),
    glm(vsl ~ am, data = d, family = binomial)
  )) {
    expect_equal(
      raw_y(
        modelbased::estimate_contrasts(model, contrast = "am"),
        modelbased::estimate_means(model, by = "am")
      ),
      d$vs,
      tolerance = 1e-8
    )
  }

  # successes and failures in cbind() have no single column of raw data
  model <- glm(cbind(gear, carb) ~ am, data = d, family = binomial)
  message <- error_text(plot(
    modelbased::estimate_contrasts(model, contrast = "am"),
    modelbased::estimate_means(model, by = "am"),
    show_data = TRUE
  ))
  expect_match(message, "cbind", fixed = TRUE)
  expect_match(message, "binomial", fixed = TRUE)
  expect_no_match(message, "lack", fixed = TRUE)

  # ratio contrasts
  fx <- contrasts_by_fixture()
  rc <- modelbased::estimate_contrasts(
    fx$model,
    contrast = "am",
    by = "cyl",
    comparison = ratio ~ pairwise
  )
  p <- plot(rc, fx$means, show_data = TRUE)
  expect_equal(p$layers[[1]]$data$y, d$mpg, tolerance = 1e-8)
  expect_lighthouses(
    p$layers[[layer_index(p, "GeomPolygon")]]$data,
    rc,
    fx$means,
    "am",
    geometry = "ratio",
    by = "cyl"
  )

  # back-transformed contrasts of a log response
  lg <- lm(log(mpg) ~ am, data = d)
  expect_equal(
    raw_y(
      modelbased::estimate_contrasts(lg, contrast = "am", transform = TRUE),
      modelbased::estimate_means(lg, by = "am", transform = TRUE)
    ),
    d$mpg,
    tolerance = 1e-8
  )

  # inverse_link is a response scale
  bi <- binomial_fixture(d, predict = "inverse_link")
  p <- plot(bi$contrasts, bi$means, show_data = TRUE)
  expect_shape(p$layers[[1]]$data, nrow = 32L)
})


test_that("lighthouse plots draw emmeans contrasts of transformed responses as differences", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("emmeans")

  d <- contrasts_by_fixture()$data
  for (model in list(
    lm(log(mpg) ~ am, data = d),
    lm(sqrt(mpg) ~ am, data = d)
  )) {
    contrasts <- modelbased::estimate_contrasts(
      model,
      contrast = "am",
      backend = "emmeans"
    )
    means <- modelbased::estimate_means(model, by = "am", backend = "emmeans")
    # emmeans back-transforms by default and takes differences of the
    # back-transformed means
    expect_true(isTRUE(attr(contrasts, "transform", exact = TRUE)))
    expect_equal(
      contrasts$Difference,
      means$Mean[means$am == "manual"] - means$Mean[means$am == "auto"],
      tolerance = 1e-6
    )
    polygons <- data_plot(contrasts, means)$geom_polygon
    expect_identical(polygon_rows_per_contrast(polygons), 3L)
    expect_lighthouses(polygons, contrasts, means, "am")
  }
})


test_that("lighthouse plots stop for ratio contrasts with `transform = TRUE` and need the interval", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  lg <- lm(log(mpg) ~ am, data = d)
  contrasts <- modelbased::estimate_contrasts(
    lg,
    contrast = "am",
    comparison = ratio ~ pairwise,
    transform = TRUE
  )
  means <- modelbased::estimate_means(lg, by = "am", transform = TRUE)
  # the ratio is not the ratio of the back-transformed means
  expect_false(isTRUE(all.equal(
    contrasts$Ratio,
    means$Mean[means$am == "manual"] / means$Mean[means$am == "auto"],
    tolerance = 1e-3
  )))
  message <- error_text(plot(contrasts, means))
  expect_match(message, "ratio", fixed = TRUE)
  expect_match(message, "`transform = TRUE`", fixed = TRUE)

  fx <- contrasts_by_fixture()
  no_interval <- fx$contrasts
  no_interval$CI_low <- NULL
  expect_error(data_plot(no_interval, fx$means), regexp = "CI_low")
})


test_that("the `transform` check passes the identity and names `transform` functions", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("emmeans")

  d <- contrasts_by_fixture()$data
  model <- lm(mpg ~ am, data = d)
  means <- modelbased::estimate_means(model, by = "am")
  for (contrasts in list(
    modelbased::estimate_contrasts(model, contrast = "am", transform = TRUE),
    modelbased::estimate_contrasts(model, contrast = "am", backend = "emmeans")
  )) {
    expect_true(isTRUE(attr(contrasts, "transform", exact = TRUE)))
    expect_false(isTRUE(attr(means, "transform", exact = TRUE)))
    polygons <- data_plot(contrasts, means)$geom_polygon
    expect_lighthouses(polygons, contrasts, means, "am")
  }

  lg <- lm(log(mpg) ~ am, data = d)
  message <- error_text(plot(
    modelbased::estimate_contrasts(lg, contrast = "am", transform = exp),
    modelbased::estimate_means(lg, by = "am", transform = exp)
  ))
  expect_match(message, "a `transform` function", fixed = TRUE)
  expect_match(message, "the estimated means with `transform = TRUE`", fixed = TRUE)
  expect_no_match(message, "with the same `transform`", fixed = TRUE)

  # the message names what the means record
  message <- error_text(plot(
    modelbased::estimate_contrasts(lg, contrast = "am", transform = exp),
    modelbased::estimate_means(lg, by = "am")
  ))
  expect_match(message, "the estimated means with `transform = FALSE`", fixed = TRUE)
})


test_that("`show_data = TRUE` accepts only the log of the response column itself", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  for (model in list(
    lm(log(mpg / 10) ~ am, data = d),
    lm(scale(mpg) ~ am, data = d)
  )) {
    contrasts <- modelbased::estimate_contrasts(
      model,
      contrast = "am",
      transform = TRUE
    )
    means <- modelbased::estimate_means(model, by = "am", transform = TRUE)
    expect_no_error(plot(contrasts, means))
    expect_match(
      error_text(plot(contrasts, means, show_data = TRUE)),
      "transforms the response",
      fixed = TRUE
    )
  }
  # the back-transformed means of log(mpg / 10) are on the scale of mpg / 10
  means <- modelbased::estimate_means(
    lm(log(mpg / 10) ~ am, data = d),
    by = "am",
    transform = TRUE
  )
  expect_lt(max(means$Mean), 3)
  expect_gt(min(d$mpg), 10)

  # emmeans draws other transformed responses, but not with the raw data
  skip_if_not_installed("emmeans")
  model <- lm(sqrt(mpg) ~ am, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    backend = "emmeans"
  )
  means <- modelbased::estimate_means(model, by = "am", backend = "emmeans")
  expect_no_error(plot(contrasts, means))
  expect_match(
    error_text(plot(contrasts, means, show_data = TRUE)),
    "transforms the response",
    fixed = TRUE
  )

  # a namespaced call is not taken for the log of the response
  expect_true(.is_log_of("log(mpg)", "mpg"))
  expect_false(.is_log_of("base::log(mpg)", "mpg"))

  # the base of a log is found with or without spaces
  expect_true(.is_log_transformation("log(x,base=2)"))
  expect_true(.is_log_transformation("log(x, base = 2)"))
  expect_false(.is_log_transformation("sqrt"))
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

  no_predict <- fx$contrasts
  attr(no_predict, "predict") <- NULL
  expect_error(
    plot(no_predict, fx$means, show_data = TRUE),
    regexp = "do not say which `predict`",
    fixed = TRUE
  )
})


test_that("`show_data = TRUE` stops for joint levels the means lack", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  jf <- joint_contrasts_fixture(contrasts_by_fixture()$data)
  partial_means <- as.data.frame(jf$means)[-1, ]
  # each level of am and of cyl is still in the means, but not "auto, 4"
  first_row <- as.data.frame(jf$means)[1, ]
  expect_identical(
    c(as.character(first_row$am), as.character(first_row$cyl)),
    c("auto", "4")
  )
  expect_true("auto" %in% as.character(partial_means$am))
  expect_true("4" %in% as.character(partial_means$cyl))
  expect_error(
    suppressWarnings(plot(jf$contrasts, partial_means, show_data = TRUE)),
    regexp = "combination of levels"
  )
  expect_error(
    suppressWarnings(plot(jf$contrasts, partial_means, show_data = TRUE)),
    regexp = "auto, 4"
  )
})


test_that("`show_data = TRUE` stops for combinations of `by` levels the means lack", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  d <- contrasts_by_fixture()$data
  d$vs <- factor(d$vs)
  model <- lm(mpg ~ cyl * am + vs, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "am",
    by = c("cyl", "vs")
  )
  means <- as.data.frame(
    modelbased::estimate_means(model, by = c("cyl", "vs", "am"))
  )
  partial_means <- means[!(means$cyl == "4" & means$vs == "1"), ]
  # each level of cyl and of vs is still in the means, and the data have the
  # dropped combination
  expect_true("4" %in% as.character(partial_means$cyl))
  expect_true("1" %in% as.character(partial_means$vs))
  expect_gt(sum(d$cyl == "4" & d$vs == "1"), 0)
  expect_error(
    suppressWarnings(plot(contrasts, partial_means, show_data = TRUE)),
    regexp = "combination of levels of am, cyl, vs"
  )
  expect_no_error(plot(contrasts, means, show_data = TRUE))

  # a combination of the contrasted variable and `by`
  fx <- contrasts_by_fixture()
  partial_means <- as.data.frame(fx$means)
  partial_means <- partial_means[
    !(partial_means$cyl == "4" & partial_means$am == "auto"),
  ]
  expect_true("auto" %in% as.character(partial_means$am))
  expect_true("4" %in% as.character(partial_means$cyl))
  expect_error(
    suppressWarnings(plot(fx$contrasts, partial_means, show_data = TRUE)),
    regexp = "the means lack auto, 4"
  )
})


test_that("`show_data = TRUE` matches integer data to double means", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  set.seed(2)
  d <- data.frame(
    y = rnorm(60),
    g = factor(rep(c("a", "b"), 30)),
    size = rep(c(100000L, 200000L, 300000L), each = 20)
  )
  model <- lm(y ~ g * size, data = d)
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = "g",
    by = "size=c(100000,200000,300000)"
  )
  means <- modelbased::estimate_means(
    model,
    by = c("g", "size=c(100000,200000,300000)")
  )
  # the data print 100000 and the means print 1e+05
  expect_type(d$size, "integer")
  expect_type(means$size, "double")
  expect_false(as.character(d$size[1]) %in% as.character(means$size))

  built <- ggplot2::ggplot_build(plot(contrasts, means, show_data = TRUE))
  panels <- built$layout$layout
  points <- built$data[[1]]
  expect_shape(panels, nrow = 3L)
  for (size in c(100000, 200000, 300000)) {
    panel <- panels$PANEL[panels$size == size]
    expect_length(panel, 1)
    expect_equal(
      sort(points$y[points$PANEL == panel]),
      sort(d$y[d$size == size]),
      tolerance = 1e-8
    )
  }
})


test_that("`show_data = TRUE` stops when the response cannot be checked", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  fx <- contrasts_by_fixture()
  no_response <- fx$contrasts
  attr(no_response, "response") <- NULL
  expect_error(
    plot(no_response, fx$means, show_data = TRUE),
    regexp = "needs the name of the response"
  )

  local_mocked_bindings(
    find_terms = function(...) stop("no terms", call. = FALSE),
    .package = "insight"
  )
  expect_error(
    plot(fx$contrasts, fx$means, show_data = TRUE),
    regexp = "its response term could not be found"
  )
})


test_that("`show_data = TRUE` keeps `by` variables named like plot columns", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  s <- contrasts_factorial_data()
  s <- data.frame(a = s$a, x = s$b, y = s$f, out = s$y)
  model <- lm(out ~ a * x * y, data = s)

  for (by in c("x", "y")) {
    contrasts <- modelbased::estimate_contrasts(model, contrast = "a", by = by)
    means <- modelbased::estimate_means(model, by = c("a", by))
    built <- ggplot2::ggplot_build(plot(contrasts, means, show_data = TRUE))
    panels <- built$layout$layout
    points <- built$data[[1]]
    expect_shape(panels, nrow = 2L)
    for (level in levels(s[[by]])) {
      panel <- panels$PANEL[as.character(panels[[paste0(".by_", by)]]) == level]
      expect_length(panel, 1)
      expect_equal(
        sort(points$y[points$PANEL == panel]),
        sort(s$out[s[[by]] == level]),
        tolerance = 1e-8
      )
    }
  }
})


# an ordinal (polr) and a multinomial (mblogit) model, each with a
# three-level response and a three-level predictor
ordinal_fixtures <- function() {
  polr_model <- MASS::polr(Sat ~ Infl, weights = Freq, data = MASS::housing)
  cells <- expand.grid(
    y = c("lo", "mid", "hi"),
    g = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  cells$n <- c(10, 6, 4, 5, 9, 6, 3, 7, 10)
  d <- cells[rep(seq_len(nrow(cells)), cells$n), c("y", "g")]
  d$y <- factor(d$y, levels = c("lo", "mid", "hi"))
  d$g <- factor(d$g)
  mblogit_model <- mclogit::mblogit(y ~ g, data = d, trace = FALSE)
  list(
    polr = list(
      x_name = "Infl",
      means = suppressMessages(
        modelbased::estimate_means(polr_model, by = "Infl")
      ),
      contrasts = suppressMessages(
        modelbased::estimate_contrasts(polr_model, contrast = "Infl")
      )
    ),
    mblogit = list(
      x_name = "g",
      means = modelbased::estimate_means(mblogit_model, by = "g"),
      contrasts = modelbased::estimate_contrasts(mblogit_model, contrast = "g")
    )
  )
}

# the contrasts within one response category, with that category as a
# `Response` column
within_category <- function(contrasts) {
  contrasts <- as.data.frame(contrasts)
  within <- contrasts[
    as.character(contrasts$Response1) == as.character(contrasts$Response2),
  ]
  within$Response <- within$Response1
  within
}


test_that("lighthouse plots of ordinal and multinomial models get one panel per response category", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("MASS")
  skip_if_not_installed("mclogit")

  for (fx in ordinal_fixtures()) {
    categories <- levels(fx$means$Response)
    expect_length(categories, 3L)
    within <- within_category(fx$contrasts)
    expect_identical(nrow(fx$contrasts), 36L)
    expect_identical(nrow(within), 9L)

    expect_no_warning({
      dp <- data_plot(fx$contrasts, fx$means)
    })
    polygons <- dp$geom_polygon
    expect_identical(polygon_rows_per_contrast(polygons), rep(3L, 9))
    expect_identical(
      sort(unique(as.character(polygons$Level1))),
      sort(unique(as.character(within$Level1)))
    )
    rownames(within) <- NULL
    expect_lighthouses(
      polygons,
      within,
      as.data.frame(fx$means),
      fx$x_name,
      y_name = "Probability",
      by = "Response"
    )

    expect_no_warning({
      p <- plot(fx$contrasts, fx$means)
    })
    layout <- ggplot2::ggplot_build(p)$layout$layout
    expect_identical(as.character(layout$Response), categories)
  }
})


test_that("ordinal lighthouse plots with `by` get one panel per category and `by` level", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("MASS")

  model <- MASS::polr(
    Sat ~ Infl + Type,
    weights = Freq,
    data = MASS::housing
  )
  contrasts <- suppressMessages(
    modelbased::estimate_contrasts(model, contrast = "Infl", by = "Type")
  )
  means <- suppressMessages(
    modelbased::estimate_means(model, by = c("Infl", "Type"))
  )
  expect_no_warning({
    p <- plot(contrasts, means)
  })
  layout <- ggplot2::ggplot_build(p)$layout$layout
  expect_identical(nrow(layout), 12L)
  expect_identical(
    sort(paste(layout$Response, layout$Type)),
    sort(as.vector(outer(
      levels(means$Response),
      levels(means$Type),
      paste
    )))
  )
  polygons <- data_plot(contrasts, means)$geom_polygon
  expect_identical(polygon_rows_per_contrast(polygons), rep(3L, 36))
})


test_that("`show_data = TRUE` stops for ordinal and multinomial contrasts", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("MASS")
  skip_if_not_installed("mclogit")

  for (fx in ordinal_fixtures()) {
    message <- error_text(plot(fx$contrasts, fx$means, show_data = TRUE))
    expect_match(message, "ordinal or multinomial", fixed = TRUE)
    expect_match(message, "`show_data = TRUE`", fixed = TRUE)
  }
})
