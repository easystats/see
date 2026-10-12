skip_if_not_installed("ggdist")

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
  b <- ggplot2::ggplot_build(p)
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
