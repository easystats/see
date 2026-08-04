test_that("`plot.see_check_heteroscedasticity()` works", {
  m <- stats::lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- performance::check_heteroscedasticity(m)

  expect_s3_class(plot(result, data = m), "gg")
})

test_that("`plot.see_check_heteroscedasticity()` snapshot", {
  skip_if_not_installed("vdiffr")

  m <- stats::lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- performance::check_heteroscedasticity(m)
  expect_s3_class(plot(result, data = m), c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.check_heteroscedasticity",
    fig = plot(result, data = m)
  )
})

test_that("`plot.see_check_heteroscedasticity()`, glmmTMB", {
  set.seed(1)
  n <- 600
  size <- 20
  x <- runif(n, -3, 3)
  d <- data.frame(x = x, y = rbinom(n, size, plogis(-0.5 + 1.2 * x)))
  d$f <- size - d$y

  m <- glm(cbind(y, f) ~ x, family = binomial, data = d)
  expect_message(
    {
      out <- performance::check_heteroscedasticity(m)
    },
    regex = "There is only a `plot()` method",
    fixed = TRUE
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.check_heteroscedasticity-glm",
    fig = plot(out)
  )

  skip_if_not_installed("glmmTMB")
  m <- glmmTMB::glmmTMB(cbind(y, f) ~ x, family = binomial, data = d)
  expect_message(
    {
      out <- performance::check_heteroscedasticity(m)
    },
    regex = "There is only a `plot()` method",
    fixed = TRUE
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.check_heteroscedasticity-glmmTMB",
    fig = plot(out)
  )
})
