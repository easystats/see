skip_if_not_installed("performance")

# `show_smooth = FALSE` removes the trend lines (LOESS or cell means) from the
# check_model() panels, and the subtitles then do not mention a line (#464).

.layer_geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1))
}

.has_smooth <- function(p) {
  any(vapply(p$layers, function(l) inherits(l$stat, "StatSmooth"), logical(1)))
}

.check_model_plot <- function(model, ..., show_ci = TRUE) {
  x <- performance::check_model(model, panel = FALSE)
  attr(x, "show_ci") <- show_ci
  plot(x, ...)
}

loess_model <- lm(mpg ~ wt, data = mtcars)
cell_model <- lm(mpg ~ factor(am), data = mtcars)

no_line_subtitles <- c(
  NCV = "Residuals should spread evenly around zero",
  HOMOGENEITY = "Spread of residuals should not change across fitted values"
)
line_subtitle <- "Reference line should be flat and horizontal"


test_that("show_smooth = FALSE removes LOESS and cell means from panels", {
  for (model in list(loess_model, cell_model)) {
    p_off <- .check_model_plot(model, show_smooth = FALSE)
    for (panel in c("NCV", "HOMOGENEITY")) {
      pp <- p_off[[panel]]
      expect_no_warning(built <- ggplot2::ggplot_build(pp))
      geoms <- .layer_geoms(pp)
      expect_false(.has_smooth(pp))
      expect_false(any(geoms %in% c("GeomLine", "GeomErrorbar")))
      expect_identical(sum(geoms == "GeomPoint"), 1L)
      expect_identical(
        nrow(built$data[[which(geoms == "GeomPoint")]]),
        nrow(pp$data)
      )
    }
  }

  p_loess <- .check_model_plot(loess_model)
  p_cells <- .check_model_plot(cell_model)
  for (panel in c("NCV", "HOMOGENEITY")) {
    expect_true(.has_smooth(p_loess[[panel]]))
    expect_true(all(
      c("GeomLine", "GeomErrorbar") %in% .layer_geoms(p_cells[[panel]])
    ))
  }
})


test_that("show_smooth = FALSE removes LOESS from the outlier panel only", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  expect_gt(insight::n_unique(stats::hatvalues(model)), 30)

  p_off <- .check_model_plot(model, show_smooth = FALSE)$OUTLIERS
  p_on <- .check_model_plot(model)$OUTLIERS
  expect_false(.has_smooth(p_off))
  expect_true(.has_smooth(p_on))
  # Cook's distance contour lines and the subtitle stay
  expect_identical(sum(.layer_geoms(p_off) == "GeomLine"), 2L)
  expect_identical(sum(.layer_geoms(p_on) == "GeomLine"), 2L)
  expect_identical(p_off$labels$subtitle, p_on$labels$subtitle)

  # the binned residuals panel keeps its own default
  glm_model <- glm(am ~ wt, family = binomial, data = mtcars)
  binned <- lapply(list(FALSE, TRUE, NULL), function(s) {
    p <- if (is.null(s)) {
      .check_model_plot(glm_model)
    } else {
      .check_model_plot(glm_model, show_smooth = s)
    }
    .layer_geoms(p$BINNED_RESID)
  })
  expect_identical(binned[[1]], binned[[3]])
  expect_identical(binned[[2]], binned[[3]])
})


test_that("subtitles do not mention a line when no line is drawn", {
  for (model in list(loess_model, cell_model)) {
    p_off <- .check_model_plot(model, show_smooth = FALSE)
    p_on <- .check_model_plot(model)
    for (panel in c("NCV", "HOMOGENEITY")) {
      expect_identical(p_off[[panel]]$labels$subtitle, no_line_subtitles[[panel]])
      expect_identical(p_on[[panel]]$labels$subtitle, line_subtitle)
    }
  }
})


test_that("check_heteroscedasticity() plot accepts show_smooth", {
  for (model in list(loess_model, cell_model)) {
    h <- performance::check_heteroscedasticity(model)
    p_off <- plot(h, data = model, show_smooth = FALSE)
    geoms <- .layer_geoms(p_off)
    expect_false(.has_smooth(p_off))
    expect_false(any(geoms %in% c("GeomLine", "GeomErrorbar")))
    expect_identical(
      p_off$labels$subtitle,
      no_line_subtitles[["HOMOGENEITY"]]
    )
  }
  expect_true(.has_smooth(plot(
    performance::check_heteroscedasticity(loess_model),
    data = loess_model
  )))
  p_cells <- plot(
    performance::check_heteroscedasticity(cell_model),
    data = cell_model
  )
  expect_false(.has_smooth(p_cells))
  expect_true("GeomLine" %in% .layer_geoms(p_cells))
})


test_that("show_smooth must be TRUE or FALSE", {
  x <- performance::check_model(loess_model, panel = FALSE)
  h <- performance::check_heteroscedasticity(loess_model)
  for (value in list(NA, 1, "no", c(TRUE, FALSE))) {
    expect_error(
      plot(x, show_smooth = value),
      "`show_smooth` must be either `TRUE` or `FALSE`",
      fixed = TRUE
    )
    expect_error(
      plot(h, data = loess_model, show_smooth = value),
      "`show_smooth` must be either `TRUE` or `FALSE`",
      fixed = TRUE
    )
  }
})


test_that("show_smooth is read from the attribute, and the argument wins", {
  x <- performance::check_model(loess_model, panel = FALSE)
  attr(x, "show_smooth") <- FALSE
  expect_false(.has_smooth(plot(x)$NCV))
  expect_true(.has_smooth(plot(x, show_smooth = TRUE)$NCV))
})


test_that("show_smooth works with the default panel layout", {
  skip_if_not_installed("patchwork")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(plot(
    performance::check_model(loess_model),
    show_smooth = FALSE
  ))
})
