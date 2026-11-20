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


test_that("plot() of check_model() says when the panels don't fit the device", {
  skip_on_cran()
  skip_if_not_installed("performance")

  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  set.seed(123)
  x <- performance::check_model(m)
  # stop the message lines from wrapping, so fixed matches work
  old_width <- options(width = 300)
  on.exit(options(old_width), add = TRUE)

  # 7 x 5 in, the default figure size of HTML output in Quarto and R Markdown
  grDevices::pdf(NULL, width = 7, height = 5)
  expect_message(
    plot(x),
    "check_model() panels are too small for this 7 x 5 in device",
    fixed = TRUE
  )
  expect_message(plot(x), "at least 8 in tall", fixed = TRUE)
  expect_message(plot(x), "panel = FALSE", fixed = TRUE)
  expect_message(plot(x), "Make the plot window or device", fixed = TRUE)
  # printing check_model() hides other plot messages, but not this one
  expect_message(print(x), "at least 8 in tall", fixed = TRUE)

  # in a knitr document, the message names the chunk options
  old <- options(knitr.in.progress = TRUE)
  expect_message(plot(x), "`fig.height` to at least 8", fixed = TRUE)
  options(old)

  # no message and no error if the drawn grid can't be read
  grid::grid.display.list(FALSE)
  expect_no_message(plot(x), message = "panels are too small")
  grid::grid.display.list(TRUE)
  grDevices::dev.off()

  # too narrow for two columns
  grDevices::pdf(NULL, width = 3, height = 12)
  expect_message(plot(x), "at least 4 in wide", fixed = TRUE)
  grDevices::dev.off()

  # large enough
  grDevices::pdf(NULL, width = 7, height = 10)
  expect_no_message(plot(x), message = "panels are too small")
  grDevices::dev.off()
})
