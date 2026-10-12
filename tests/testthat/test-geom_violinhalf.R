skip_if_not_installed("ggdist", "3.3.3")

iris_long <- data.frame(
  Species = rep(iris$Species, times = 4),
  variable = rep(names(iris)[1:4], each = nrow(iris)),
  values = unlist(iris[1:4], use.names = FALSE)
)

# group sizes 50, 10, 50
iris_unbalanced <- iris[c(1:50, 51:60, 101:150), ]

panel_y_ranges <- function(p) {
  lapply(ggplot2::ggplot_build(p)$layout$panel_params, `[[`, "y.range")
}

group_max <- function(ld, column) {
  as.vector(tapply(ld[[column]], ld$group, max))
}

test_that("geom_violinhalf() respects free scales in facet_wrap()", {
  base <- ggplot2::ggplot(iris_long, ggplot2::aes(Species, values)) +
    ggplot2::facet_wrap(~variable, scales = "free_y")

  expect_equal(
    panel_y_ranges(base + geom_violinhalf()),
    panel_y_ranges(base + ggplot2::geom_violin()),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf() computes each facet_grid() cell from its own data", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    geom_violinhalf() +
    ggplot2::facet_grid(am ~ vs)
  # one cell (am = 1, vs = 0, cyl = 4) has a single car
  expect_warning(ggplot2::ggplot_build(p), "fewer than two")
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  ld <- b$data[[1]]
  layout <- b$layout$layout

  cells <- stats::aggregate(
    mpg ~ am + vs + cyl,
    data = mtcars,
    FUN = function(v) c(n = length(v), min = min(v), max = max(v))
  )
  cells <- cells[cells$mpg[, "n"] >= 2, ]
  expect_gt(nrow(cells), 0)

  for (i in seq_len(nrow(cells))) {
    panel <- layout$PANEL[layout$am == cells$am[i] & layout$vs == cells$vs[i]]
    cyl_x <- match(cells$cyl[i], sort(unique(mtcars$cyl)))
    y <- ld$y[ld$PANEL == panel & ld$x == cyl_x]
    expect_gt(length(y), 0)
    expect_gte(min(y), cells$mpg[i, "min"])
    expect_lte(max(y), cells$mpg[i, "max"])
  }
})

test_that("geom_violinhalf() scales widths like geom_violin()", {
  for (s in c("area", "count", "width")) {
    base <- ggplot2::ggplot(
      iris_unbalanced,
      ggplot2::aes(Species, Sepal.Length)
    )
    half <- ggplot2::layer_data(base + geom_violinhalf(scale = s))
    full <- ggplot2::layer_data(
      base + ggplot2::geom_violin(scale = s, trim = TRUE)
    )
    half_max <- group_max(half, "thickness")
    full_max <- group_max(full, "violinwidth")

    expect_equal(
      half_max / max(half_max),
      full_max / max(full_max),
      tolerance = 1e-3,
      info = s
    )
  }

  layer <- geom_violinhalf()
  expect_identical(layer$geom_params$normalize, "panels")
})

test_that("geom_violinhalf() puts slabs on the side that flip asks for", {
  base <- ggplot2::ggplot(
    iris_unbalanced,
    ggplot2::aes(Species, Sepal.Length)
  )
  side_by_group <- function(layer) {
    ld <- ggplot2::layer_data(base + layer, 1)
    vapply(split(ld$side, ld$group), unique, character(1))
  }

  expect_identical(
    unname(side_by_group(geom_violinhalf())),
    rep("right", 3)
  )
  expect_identical(
    unname(side_by_group(geom_violinhalf(flip = TRUE))),
    rep("left", 3)
  )
  expect_identical(
    unname(side_by_group(geom_violinhalf(flip = c(1, 3)))),
    c("left", "right", "left")
  )
})

test_that("geom_violinhalf() trims like geom_violin()", {
  base <- ggplot2::ggplot(
    iris_unbalanced,
    ggplot2::aes(Species, Sepal.Length)
  )
  y_range_by_group <- function(ld) {
    lapply(split(ld$y, ld$group), range)
  }
  data_ranges <- lapply(
    split(iris_unbalanced$Sepal.Length, iris_unbalanced$Species),
    range
  )

  trimmed <- ggplot2::layer_data(base + geom_violinhalf(trim = TRUE))
  expect_equal(
    unname(y_range_by_group(trimmed)),
    unname(data_ranges),
    tolerance = testthat::testthat_tolerance()
  )

  untrimmed <- ggplot2::layer_data(base + geom_violinhalf(trim = FALSE))
  reference <- ggplot2::layer_data(base + ggplot2::geom_violin(trim = FALSE))
  expect_equal(
    unname(y_range_by_group(untrimmed)),
    unname(y_range_by_group(reference)),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf(interval = TRUE) adds a median and intervals", {
  base <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length))
  without <- base + geom_violinhalf()
  with <- base + geom_violinhalf(interval = TRUE)
  expect_length(with$layers, length(without$layers) + 1L)

  ld <- ggplot2::layer_data(with, length(with$layers))
  for (i in seq_along(levels(iris$Species))) {
    v <- iris$Sepal.Length[iris$Species == levels(iris$Species)[i]]
    for (w in c(0.66, 0.95)) {
      row <- ld[ld$group == i & abs(ld$.width - w) < 1e-12, ]
      expect_identical(nrow(row), 1L)
      q <- stats::quantile(v, c((1 - w) / 2, 1 - (1 - w) / 2), type = 7)
      expect_equal(row$y, stats::median(v), tolerance = 1e-8)
      expect_equal(c(row$ymin, row$ymax), unname(q), tolerance = 1e-8)
    }
  }

  expect_error(geom_violinhalf(interval = "yes"), regexp = "interval")
  expect_error(geom_violinhalf(interval = NA), regexp = "interval")
})

test_that("geom_violinhalf() keeps its old arguments working", {
  # each half violin spans 0.45 x-units on its own side, so flipped
  # neighbours do not overlap and the default dodge leaves them in place
  p <- ggplot2::ggplot(
    iris_unbalanced,
    ggplot2::aes(Species, Sepal.Length)
  ) +
    geom_violinhalf(flip = c(1, 3))
  ld <- ggplot2::layer_data(p, 1)
  left <- ld$group %in% c(1, 3)
  expect_equal(
    ld$xmin,
    ifelse(left, ld$x - 0.45, ld$x),
    tolerance = testthat::testthat_tolerance()
  )
  expect_equal(
    ld$xmax,
    ifelse(left, ld$x, ld$x + 0.45),
    tolerance = testthat::testthat_tolerance()
  )

  nudge <- ggplot2::position_nudge(x = 0.1)
  expect_identical(geom_violinhalf(position = nudge)$position, nudge)

  expect_warning(
    geom_violinhalf(stat = "identity"),
    regexp = "`stat` is ignored",
    fixed = TRUE
  )
})

test_that("geom_violinhalf() works with a layer mapping outside see", {
  # the added aesthetics must find see's helpers from the user's environment
  p <- evalq(
    ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
      see::geom_violinhalf(ggplot2::aes(fill = Species), scale = "width"),
    globalenv()
  )
  ld <- ggplot2::layer_data(p)
  expect_equal(
    group_max(ld, "thickness"),
    c(1, 1, 1),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf(scale = \"width\") scales each facet panel", {
  d <- iris_long[iris_long$variable %in% c("Petal.Width", "Sepal.Width"), ]
  p <- ggplot2::ggplot(d, ggplot2::aes(Species, values)) +
    geom_violinhalf(scale = "width") +
    ggplot2::facet_wrap(~variable, scales = "free_y")
  ld <- ggplot2::layer_data(p)
  per_cell <- tapply(ld$thickness, list(ld$PANEL, ld$group), max)
  expect_equal(
    as.vector(per_cell),
    rep(1, 6),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf(interval = TRUE) dodges intervals with their slabs", {
  p <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(factor(cyl), mpg, fill = factor(am))
  ) +
    geom_violinhalf(interval = TRUE)
  slab <- suppressWarnings(ggplot2::layer_data(p, 1))
  int <- suppressWarnings(ggplot2::layer_data(p, 2))
  slab_x <- tapply(slab$x, slab$group, unique)
  int_x <- tapply(int$x, int$group, unique)
  expect_equal(
    int_x,
    slab_x[names(int_x)],
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf() outlines the whole half violin", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violinhalf()
  grob <- ggplot2::layer_grob(p)[[1]]
  kids <- grob$children
  expect_true(all(vapply(kids, inherits, logical(1), what = "polygon")))
  cols <- unique(vapply(kids, function(k) k$gp$col, character(1)))
  expect_identical(
    grDevices::col2rgb(cols),
    grDevices::col2rgb("grey20")
  )
})

test_that("geom_violinhalf() keeps the 0.45 width in horizontal plots", {
  p <- ggplot2::ggplot(
    iris_unbalanced,
    ggplot2::aes(Sepal.Length, Species)
  ) +
    geom_violinhalf(flip = c(1, 3))
  ld <- ggplot2::layer_data(p)
  left <- ld$group %in% c(1, 3)
  expect_equal(
    ld$ymin,
    ifelse(left, ld$y - 0.45, ld$y),
    tolerance = testthat::testthat_tolerance()
  )
  expect_equal(
    ld$ymax,
    ifelse(left, ld$y, ld$y + 0.45),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf() passes bw and kernel to the density", {
  base <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length))
  max_default <- group_max(
    ggplot2::layer_data(base + geom_violinhalf()),
    "thickness"
  )

  expect_no_warning(ggplot2::layer_data(base + geom_violinhalf(bw = 0.5)))
  ld_bw <- ggplot2::layer_data(base + geom_violinhalf(bw = 0.5))
  ref_bw <- ggplot2::layer_data(base + ggplot2::geom_violin(bw = 0.5))
  expect_equal(
    group_max(ld_bw, "thickness") / max(group_max(ld_bw, "thickness")),
    group_max(ref_bw, "violinwidth") / max(group_max(ref_bw, "violinwidth")),
    tolerance = 1e-3
  )

  ld_kernel <- ggplot2::layer_data(
    base + geom_violinhalf(kernel = "rectangular")
  )
  expect_false(isTRUE(all.equal(
    group_max(ld_kernel, "thickness"),
    max_default
  )))
})

test_that("geom_violinhalf() does not draw groups with one observation", {
  d <- data.frame(
    g = c("a", "a", "a", "b"),
    y = c(1, 2, 3, 5),
    stringsAsFactors = FALSE
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(g, y)) + geom_violinhalf()
  expect_warning(ggplot2::layer_data(p), "fewer than two")
  ld <- suppressWarnings(ggplot2::layer_data(p))
  expect_true(all(is.na(ld$thickness[ld$group == 2])))
  expect_false(anyNA(ld$thickness[ld$group == 1]))
})

test_that("geom_violinhalf() keeps show.legend and inherit.aes in place", {
  args <- names(formals(geom_violinhalf))
  expect_identical(
    args[1:9],
    c(
      "mapping",
      "data",
      "stat",
      "position",
      "trim",
      "flip",
      "scale",
      "show.legend",
      "inherit.aes"
    )
  )
})

test_that("geom_violinhalf() scales the half-width by the axis resolution", {
  set.seed(1)
  d <- data.frame(x = rep(c(10, 20, 30), each = 20), y = stats::rnorm(60))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = x)) + geom_violinhalf()
  ld <- ggplot2::layer_data(p)
  expect_equal(
    unique(ld$xmax - ld$xmin),
    4.5,
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf(width = ) is the width of a whole violin", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violinhalf(width = 0.6)
  ld <- ggplot2::layer_data(p)
  expect_equal(
    ld$xmax - ld$xmin,
    rep(0.3, nrow(ld)),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf() does not draw groups with all values equal", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 5),
    y = c(1, 2, 3, 4, 5, 2, 2, 2, 2, 2),
    stringsAsFactors = FALSE
  )
  for (s in c("area", "width")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(g, y)) + geom_violinhalf(scale = s)
    expect_warning(ggplot2::layer_data(p), "all values equal")
    ld <- suppressWarnings(ggplot2::layer_data(p))
    expect_true(all(is.na(ld$thickness[ld$group == 2])), info = s)
    expect_true(all(is.finite(ld$thickness[ld$group == 1])), info = s)
  }
})

test_that("geom_violinhalf() passes bounds to the density", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violinhalf(trim = FALSE, bounds = c(4, 8))
  expect_no_warning(ggplot2::layer_data(p))
  ld <- ggplot2::layer_data(p)
  expect_gte(min(ld$y), 4)
  expect_lte(max(ld$y), 8)
  # without bounds, the tails reach below 4
  unbounded <- ggplot2::layer_data(
    ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
      geom_violinhalf(trim = FALSE)
  )
  expect_lt(min(unbounded$y), 4)
})

test_that("geom_violinhalf() keeps the outline opaque under alpha", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violinhalf(alpha = 0.3, fill = "red")
  kids <- ggplot2::layer_grob(p)[[1]]$children
  cols <- vapply(kids, function(k) k$gp$col, character(1))
  expect_true(all(grDevices::col2rgb(cols, alpha = TRUE)["alpha", ] == 255))
})

test_that("geom_violinhalf() draws no border inside a slab with several fills", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violinhalf(ggplot2::aes(fill = ggplot2::after_stat(y > 6)))
  kids <- ggplot2::layer_grob(p)[[1]]$children
  pieces <- Filter(
    function(k) inherits(k, "polygon") && length(unique(k$id)) > 1L,
    kids
  )
  # versicolor and virginica cross y = 6, so their slabs have two pieces
  expect_length(pieces, 2)
  for (k in pieces) {
    expect_true(all(is.na(k$gp$col)))
  }
})

test_that("geom_violinhalf(interval = TRUE) passes interval styles on", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violinhalf(interval = TRUE, .width = 0.5, point_interval = "mean_qi")
  expect_no_warning(ggplot2::ggplot_build(p))
  ld <- ggplot2::layer_data(p, 2)
  expect_identical(unique(ld$.width), 0.5)
  expect_equal(
    ld$y[ld$group == 1],
    mean(iris$Sepal.Length[iris$Species == "setosa"]),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violinhalf() leaves a data frame as mapping to ggplot2", {
  expect_error(
    geom_violinhalf(iris),
    "must be created by `aes()`",
    fixed = TRUE
  )
})

test_that("geom_violinhalf() accepts a StatYdensity stat without a warning", {
  expect_no_warning(geom_violinhalf(stat = ggplot2::StatYdensity))
})
