# Helpers for the half-violin tests

iris_ab <- iris
iris_ab$g <- factor(rep(c("a", "b"), 75))

# The drawn extent of each half violin along the categorical axis, read from
# the slab polygons and converted to data units, with the group's built data
slab_measure <- function(p, horizontal = FALSE) {
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  ld <- b$data[[1]]
  ld <- ld[!is.na(ld$thickness), ]
  cat_axis <- if (horizontal) "y" else "x"
  val_axis <- if (horizontal) "x" else "y"
  kids <- Filter(
    function(k) inherits(k, "polygon"),
    ggplot2::layer_grob(p, 1)[[1]]$children
  )
  to_data <- function(v, range) range[1] + as.numeric(v) * diff(range)
  polys <- lapply(kids, function(k) {
    list(
      x = to_data(k$x, pp$x.range),
      y = to_data(k$y, pp$y.range)
    )
  })
  t_max <- max(ld$thickness)
  out <- lapply(split(ld, ld$group), function(d) {
    val_range <- range(d[[val_axis]])
    hit <- vapply(
      polys,
      function(poly) all(abs(range(poly[[val_axis]]) - val_range) < 1e-6),
      logical(1)
    )
    poly <- polys[[which(hit)]]
    data.frame(
      group = d$group[1],
      lo = min(poly[[cat_axis]]),
      hi = max(poly[[cat_axis]]),
      pos = d[[cat_axis]][1],
      pos_min = d[[paste0(cat_axis, "min")]][1],
      pos_max = d[[paste0(cat_axis, "max")]][1],
      share = max(d$thickness) / t_max
    )
  })
  do.call(rbind, out)
}

# right-side (or, for `left`, left-side) extents for every group
expect_extents <- function(m, left = integer()) {
  is_left <- m$group %in% left
  reach <- (m$pos_max - m$pos) * m$share
  expect_equal(
    m$lo,
    ifelse(is_left, m$pos - reach, m$pos),
    tolerance = 1e-6
  )
  expect_equal(
    m$hi,
    ifelse(is_left, m$pos, m$pos + reach),
    tolerance = 1e-6
  )
}
