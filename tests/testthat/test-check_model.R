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

  size_message <- function(expr) {
    grep("panels are too small", capture_messages(expr), value = TRUE)
  }

  # 7 x 5 in, the default figure size of HTML output in Quarto and R Markdown
  grDevices::pdf(NULL, width = 7, height = 5)
  msg <- size_message(plot(x))
  expect_length(msg, 1)
  expect_match(msg, "for this 7 x 5 in device: each row gets 0.14 in", fixed = TRUE)
  expect_match(msg, "Make the plot window or device at least 8 in tall", fixed = TRUE)
  expect_match(msg, "plot(check_model(model, panel = FALSE))", fixed = TRUE)
  # printing check_model() hides other plot messages, but not this one
  expect_length(size_message(print(x)), 1)

  # in a knitr document, the message names the chunk options
  old <- options(knitr.in.progress = TRUE)
  on.exit(options(old), add = TRUE)
  expect_match(
    size_message(plot(x)),
    "Set the chunk option `fig.height` to at least 8 (`fig-height` in Quarto)",
    fixed = TRUE
  )
  options(old)

  # no message and no error if the drawn grid can't be read
  grid::grid.display.list(FALSE)
  on.exit(grid::grid.display.list(TRUE), add = TRUE)
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
