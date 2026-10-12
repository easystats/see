# Helpers for the half-violin tests

iris_ab <- iris
iris_ab$g <- factor(rep(c("a", "b"), 75))

# The drawn extent of each half violin along the categorical axis, read from
# the slab polygons and converted to data units, with the group's built data
slab_measure <- function(p, horizontal = FALSE, panel = 1) {
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[panel]]
  ld <- b$data[[1]]
  ld <- ld[!is.na(ld$thickness) & ld$PANEL == panel, ]
  cat_axis <- if (horizontal) "y" else "x"
  val_axis <- if (horizontal) "x" else "y"
  kids <- Filter(
    function(k) inherits(k, "polygon"),
    ggplot2::layer_grob(p, 1)[[panel]]$children
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
      function(shape) all(abs(range(shape[[val_axis]]) - val_range) < 1e-6),
      logical(1)
    )
    shape <- polys[[which(hit)]]
    data.frame(
      group = d$group[1],
      lo = min(shape[[cat_axis]]),
      hi = max(shape[[cat_axis]]),
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
  # each value within 1e-6
  expect_lt(max(abs(m$lo - ifelse(is_left, m$pos - reach, m$pos))), 1e-6)
  expect_lt(max(abs(m$hi - ifelse(is_left, m$pos, m$pos + reach))), 1e-6)
}
