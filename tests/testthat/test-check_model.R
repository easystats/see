test_that("`check_model()` works with lm", {
  skip_on_cran()
  skip_if_not_installed("merDeriv")

  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  set.seed(123)
  x <- performance::check_model(m)
  # vdiffr test is too fragile for this complex of an output
  expect_s3_class(x, "check_model")
  # still works with character values for theme
  x <- performance::check_model(m, theme = "ggplot2::theme_dark")
})


test_that("`check_model()` plots use base_size and the title sizes", {
  skip_on_cran()
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  x <- performance::check_model(
    m,
    base_size = 16,
    size_title = 20,
    size_axis_title = 6,
    panel = FALSE
  )
  p <- plot(x)
  expect_identical(
    vapply(p, \(i) i$theme$text$size, numeric(1)),
    rep(16, length(p)),
    ignore_attr = TRUE
  )
  expect_identical(p[[1]]$theme$plot.title$size, 20)
  expect_identical(p[[1]]$theme$axis.title$size, 6)

  # a theme from the user still wins
  x <- performance::check_model(
    m,
    theme = ggplot2::theme_dark(base_size = 20),
    panel = FALSE
  )
  expect_identical(plot(x)[[1]]$theme$text$size, 20)
})
