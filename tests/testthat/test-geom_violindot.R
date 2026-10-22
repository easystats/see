skip_if_not_installed("ggdist", "3.3.3")

iris_long <- data.frame(
  Species = rep(iris$Species, times = 4),
  variable = rep(names(iris)[1:4], each = nrow(iris)),
  values = unlist(iris[1:4], use.names = FALSE)
)

# the dots layer is the one built on ggdist's GeomDots
dots_layer <- function(layers) {
  is_dots <- vapply(
    layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )
  layers[[which(is_dots)]]
}

test_that("geom_violindot() respects free scales in facet_wrap()", {
  base <- ggplot2::ggplot(iris_long, ggplot2::aes(Species, values)) +
    ggplot2::facet_wrap(~variable, scales = "free_y")
  y_ranges <- function(p) {
    lapply(ggplot2::ggplot_build(p)$layout$panel_params, `[[`, "y.range")
  }

  expect_equal(
    y_ranges(base + geom_violindot()),
    y_ranges(base + ggplot2::geom_violin()),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violindot() lets ggdist choose the bin width", {
  expect_identical(dots_layer(geom_violindot())$geom_params$binwidth, NA)
  expect_identical(
    dots_layer(geom_violindot(binwidth = 0.1))$geom_params$binwidth,
    0.1
  )
})

test_that("geom_violindot() draws dots on the left and the slab on the right", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot()
  b <- ggplot2::ggplot_build(p)
  is_dots <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )

  expect_identical(unique(b$data[[which(is_dots)]]$side), "left")
  expect_identical(unique(b$data[[which(!is_dots)]]$side), "right")
})

test_that("geom_violindot() passes interval on", {
  expect_length(geom_violindot(interval = TRUE), length(geom_violindot()) + 1L)
})

test_that("geom_violindot() keeps its old dot arguments working", {
  expect_identical(
    dots_layer(geom_violindot(dots_size = 1.2))$geom_params$dotsize,
    1.2
  )
  expect_identical(
    dots_layer(geom_violindot(size_dots = 1.2))$geom_params$dotsize,
    1.2
  )

  built_dots <- function(...) {
    p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
      geom_violindot(...)
    is_dots <- vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomDots"),
      logical(1)
    )
    ggplot2::ggplot_build(p)$data[[which(is_dots)]]
  }
  expect_identical(unique(built_dots(dots_color = "red")$colour), "red")
  expect_identical(unique(built_dots(color_dots = "red")$colour), "red")
  expect_identical(unique(built_dots(dots_fill = "blue")$fill), "blue")
  expect_identical(unique(built_dots(fill_dots = "blue")$fill), "blue")

  nudge <- ggplot2::position_nudge(x = -0.1)
  expect_identical(
    dots_layer(geom_violindot(position_dots = nudge))$position,
    nudge
  )
})

test_that("geom_violindot() dot arguments win over the same names in ...", {
  built <- function(...) {
    p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
      geom_violindot(...)
    is_dots <- vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomDots"),
      logical(1)
    )
    b <- ggplot2::ggplot_build(p)
    list(dots = b$data[[which(is_dots)]], slab = b$data[[which(!is_dots)]])
  }

  fills <- built(fill = "red", fill_dots = "blue")
  expect_identical(unique(fills$dots$fill), "blue")
  expect_identical(unique(fills$slab$fill), "red")

  expect_no_warning(built(color = "red", color_dots = "blue"))
  cols <- built(color = "red", color_dots = "blue")
  expect_identical(unique(cols$dots$colour), "blue")
  expect_identical(
    unique(built(colour = "red", color_dots = "blue")$dots$colour),
    "blue"
  )
})

test_that("geom_violindot(flip = TRUE) gives no warning from the dots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(flip = TRUE)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("geom_violindot(interval = TRUE) keeps a position_dots given", {
  nudge <- ggplot2::position_nudge(x = -0.2)
  expect_identical(
    dots_layer(geom_violindot(interval = TRUE, position_dots = nudge))$position,
    nudge
  )
})

test_that("geom_violindot() keeps half-violin arguments away from the dots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(n = 100, bounds = c(4, 8), .width = 0.5)
  expect_no_warning(ggplot2::ggplot_build(p))

  both <- geom_violindot(side = "both")
  expect_identical(dots_layer(both)$aes_params$side, "left")
})

test_that("geom_violindot() dot arguments win over base-graphics aliases", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(
      col = "red",
      color_dots = "blue",
      bg = "red",
      fill_dots = "green"
    )
  expect_no_warning(ggplot2::ggplot_build(p))
  is_dots <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )
  dots <- ggplot2::layer_data(p, which(is_dots))
  expect_identical(unique(dots$colour), "blue")
  expect_identical(unique(dots$fill), "green")
})

test_that("geom_violindot() gives the dots the same space as the half violins", {
  set.seed(1)
  d <- data.frame(x = rep(c(10, 20, 30), each = 20), y = stats::rnorm(60))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = x)) + geom_violindot()
  is_dots <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )
  dots <- ggplot2::layer_data(p, which(is_dots))
  expect_equal(
    unique(round(dots$xmax - dots$xmin, 6)),
    4.5,
    tolerance = testthat::testthat_tolerance()
  )

  p2 <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(width = 0.6)
  dots2 <- ggplot2::layer_data(p2, 2)
  slab2 <- ggplot2::layer_data(p2, 1)
  expect_equal(
    unique(round(dots2$xmax - dots2$xmin, 6)),
    0.3,
    tolerance = testthat::testthat_tolerance()
  )
  expect_equal(
    unique(round(slab2$xmax - slab2$xmin, 6)),
    0.3,
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violindot() keeps slab-only arguments away from the dots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(expand = TRUE, outline_bars = FALSE, show_point = TRUE)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("geom_violindot() draws dodged dots in the free half of their cell", {
  p <- ggplot2::ggplot(
    iris_ab,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot()
  m <- dots_measure(p)

  expect_setequal(unique(m$group), 1:6)
  # each dot centre within [xmin, x] of its group's cell, within 1e-6
  expect_gt(min(m$dot_x - m$pos_min), -1e-6)
  expect_lt(max(m$dot_x - m$pos), 1e-6)
  expect_extents(slab_measure(p))
})

test_that("geom_violindot() keeps dodged dots in narrow cells", {
  d <- iris
  d$g <- factor(rep(1:4, length.out = nrow(d)))
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot(interval = TRUE)
  expect_no_warning(m <- dots_measure(p))

  expect_setequal(unique(m$group), 1:12)
  expect_gt(min(m$dot_x - m$pos_min), -1e-6)
  expect_lt(max(m$dot_x - m$pos), 1e-6)
})

test_that("geom_violindot() dodges dots in horizontal plots", {
  p <- ggplot2::ggplot(
    iris_ab,
    ggplot2::aes(Sepal.Length, Species, fill = interaction(g, Species))
  ) +
    geom_violindot()
  m <- dots_measure(p, horizontal = TRUE)

  expect_setequal(unique(m$group), 1:6)
  expect_gt(min(m$dot_x - m$pos_min), -1e-6)
  expect_lt(max(m$dot_x - m$pos), 1e-6)
})

test_that("geom_violindot() dodges dots in the order of the half violins", {
  d <- iris_ab
  # an order that is constant within each group reverses the dodge order
  d$o <- ifelse(d$g == "a", 2L, 1L)
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species), order = o)
  ) +
    geom_violindot()
  m <- dots_measure(p)

  slab <- ggplot2::layer_data(p, 1)
  expect_gt(slab$x[slab$group == 1][1], slab$x[slab$group == 2][1])
  expect_gt(min(m$dot_x - m$pos_min), -1e-6)
  expect_lt(max(m$dot_x - m$pos), 1e-6)
})

test_that("geom_violindot() does not dodge dots for groups without values", {
  d <- iris_ab
  d$Sepal.Length[d$Species == "setosa" & d$g == "b"] <- NA
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot()
  # (both layers warn about the removed rows)
  m <- suppressWarnings(dots_measure(p))
  dots <- suppressWarnings(ggplot2::layer_data(p, 2))
  dots <- dots[!is.na(dots$y) & as.numeric(dots$x) < 1.5, ]

  # the half violin of setosa a is alone, so its dots keep the box
  # [x - 0.45, x], nudged by -0.025
  expect_gt(nrow(dots), 0)
  expect_equal(unique(as.numeric(dots$xmin)), 0.525)
  expect_equal(unique(as.numeric(dots$xmax)), 0.975)
  dodged <- m[m$pos != 1, ]
  expect_gt(min(dodged$dot_x - dodged$pos_min), -1e-6)
  expect_lt(max(dodged$dot_x - dodged$pos), 1e-6)
})

test_that("geom_violindot() stacks a lone group's dots across its whole box", {
  set.seed(2)
  d <- data.frame(
    x = factor(rep(c("A", "B"), c(40, 400))),
    g = factor(c(rep(c("a", "b"), 20), rep("c", 400))),
    y = c(stats::rnorm(40), sample(1:5, 400, replace = TRUE))
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = g)) + geom_violindot()
  m <- dots_measure(p)
  alone <- m[m$group == 3, ]

  # the dodged groups' dots have 0.1125 - 0.00625 = 0.10625 of room; the lone
  # group's stacks reach further, within its own box [x - 0.475, x - 0.025]
  expect_gt(max(alone$dot_x) - min(alone$dot_x), 0.10625)
  expect_gt(min(alone$dot_x - (alone$pos - 0.475)), -1e-6)
})

test_that("geom_violindot() keeps the dot nudge for one group per position", {
  built_x <- function(...) {
    p <- ggplot2::ggplot(
      iris,
      ggplot2::aes(Species, Sepal.Length, fill = Species)
    ) +
      geom_violindot(...)
    is_dots <- vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomDots"),
      logical(1)
    )
    sort(unique(as.numeric(ggplot2::layer_data(p, which(is_dots))$x)))
  }
  expect_equal(built_x(), c(0.975, 1.975, 2.975))
  expect_equal(built_x(interval = TRUE), c(0.94, 1.94, 2.94))
})

test_that("geom_violindot() uses a user position_dots as given under dodge", {
  nudge <- ggplot2::position_nudge(x = -0.1)
  p <- ggplot2::ggplot(
    iris_ab,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot(position_dots = nudge)
  expect_identical(dots_layer(p$layers)$position, nudge)
})

# each group's dots box, merged with the dodge cell of its half violin
dots_cells <- function(p) {
  slab <- unique(ggplot2::layer_data(p, 1)[c("group", "x", "xmin")])
  dots <- ggplot2::layer_data(p, length(p$layers))
  merge(dots, slab, by = "group", suffixes = c("", "_cell"))
}

# the dots box lies in the lower half of the cell; returns the gap
expect_dots_in_cells <- function(p) {
  cells <- dots_cells(p)
  expect_gt(min(cells$xmin - cells$xmin_cell), -1e-6)
  expect_lt(max(cells$xmax - cells$x_cell), 1e-6)
  unique(round(as.numeric(cells$x_cell - cells$xmax), 9))
}

base_ab <- ggplot2::ggplot(
  iris_ab,
  ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
)

test_that("geom_violindot() draws with only one position aesthetic mapped", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(y = Sepal.Length)) + geom_violindot()
  dots <- ggplot2::layer_data(p, 2)
  expect_lte(max(dots$xmax), min(ggplot2::layer_data(p, 1)$xmin))
  expect_equal(sort(dots$y), sort(iris$Sepal.Length))
})

test_that("geom_violindot() gives `position` to the half violins", {
  dodge <- ggplot2::position_dodge(width = 0.5)
  p <- base_ab + geom_violindot(position = dodge)
  expect_identical(p$layers[[1]]$position, dodge)
  expect_false(identical(dots_layer(p$layers)$position, dodge))
  expect_setequal(unique(dots_cells(p)$group), 1:6)
  expect_dots_in_cells(p)
})

test_that("geom_violindot() does not dodge the dots under position = \"identity\"", {
  p <- base_ab + geom_violindot(position = "identity")
  expect_s3_class(p$layers[[1]]$position, "PositionIdentity")
  dots <- ggplot2::layer_data(p, 2)
  level <- round(as.numeric(dots$x))
  expect_equal(as.numeric(dots$xmax), level - 0.025)
  expect_equal(as.numeric(dots$xmin), level - 0.475)
})

test_that("geom_violindot() keeps the dots at their values in horizontal plots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Species)) +
    geom_violindot()
  dots <- ggplot2::layer_data(p, 2)
  expect_equal(sort(dots$x), sort(iris$Sepal.Length))
  expect_equal(as.numeric(dots$ymax), round(as.numeric(dots$y)) - 0.025)
})

test_that("geom_violindot() scales the dot gap with the axis resolution", {
  set.seed(1)
  d <- data.frame(x = rep(c(10, 20, 30), each = 20), y = stats::rnorm(60))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = x)) +
    geom_violindot(interval = TRUE)
  dots <- ggplot2::layer_data(p, length(p$layers))
  level <- round(dots$x / 10) * 10
  expect_equal(dots$xmax, level - 0.6)
  expect_equal(dots$xmin, level - 5.1)
})

test_that("geom_violindot() shrinks the dot gap with the dodge cell", {
  expect_equal(expect_dots_in_cells(base_ab + geom_violindot()), 0.00625)
})

test_that("geom_violindot() dots follow a reversed dodge, but not dodge2", {
  p <- base_ab +
    geom_violindot(position = ggplot2::position_dodge(reverse = TRUE))
  expect_setequal(unique(dots_cells(p)$group), 1:6)
  expect_dots_in_cells(p)

  p <- base_ab + geom_violindot(position = ggplot2::position_dodge2())
  dots <- ggplot2::layer_data(p, 2)
  expect_equal(as.numeric(dots$xmax), round(as.numeric(dots$x)) - 0.025)
})

test_that("geom_violindot() caps the dot gap in narrow dodged cells", {
  expect_equal(expect_dots_in_cells(base_ab + geom_violindot(width = 0.04)), 0.0025)

  p <- base_ab + geom_violindot(width = 0)
  expect_equal(expect_dots_in_cells(p), 0)
  dots <- ggplot2::layer_data(p, 2)
  expect_true(all(is.finite(c(dots$x, dots$xmin, dots$xmax))))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("geom_violindot() keeps a mapped width away from the dots box", {
  d <- iris
  d$w <- 0.6
  p <- ggplot2::ggplot(d, ggplot2::aes(Species, Sepal.Length, width = w)) +
    geom_violindot()
  slab <- ggplot2::layer_data(p, 1)
  dots <- ggplot2::layer_data(p, 2)
  expect_equal(unique(round(slab$xmax - slab$xmin, 9)), 0.45)
  expect_equal(unique(round(dots$xmax - dots$xmin, 9)), 0.45)
})

test_that("geom_violindot() dots share the cells of a preserved dodge", {
  d <- iris_ab[!(iris_ab$Species == "versicolor" & iris_ab$g == "b"), ]
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot(position = ggplot2::position_dodge(preserve = "single"))
  expect_setequal(unique(dots_cells(p)$group), 1:5)
  expect_dots_in_cells(p)
})

test_that("geom_violindot() leaves groups without values out of a preserved dodge", {
  d <- data.frame(
    x = factor(rep(c("a", "b"), each = 30)),
    h = "u",
    v = stats::rnorm(60)
  )
  d <- rbind(d, data.frame(x = "b", h = "v", v = NA))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, v, fill = h)) +
    geom_violindot(position = ggplot2::position_dodge(preserve = "single"))
  slab <- suppressWarnings(ggplot2::layer_data(p, 1))
  dots <- suppressWarnings(ggplot2::layer_data(p, 2))
  dots <- dots[!is.na(dots$y), ]
  expect_equal(unique(round(as.numeric(slab$xmax - slab$xmin), 9)), 0.45)
  expect_equal(as.numeric(dots$xmax), round(as.numeric(dots$x)) - 0.025)
})

test_that("geom_violindot() gives a partial `position` name to the half violins", {
  p <- base_ab + expect_no_warning(geom_violindot(pos = "identity"))
  expect_s3_class(p$layers[[1]]$position, "PositionIdentity")
  dots <- ggplot2::layer_data(p, 2)
  expect_equal(as.numeric(dots$xmax), round(as.numeric(dots$x)) - 0.025)
})
