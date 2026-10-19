test_that("`plot.see_equivalence_test()` works", {
  skip_if_not_installed("ggridges")
  m_aov <- stats::aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result_ez <- effectsize::eta_squared(m_aov, verbose = FALSE)
  expect_s3_class(plot(result_ez), "gg")
})

test_that("`plot.see_equivalence_test()` snapshot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggridges")

  m_aov <- stats::aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result_ez <- effectsize::eta_squared(m_aov, verbose = FALSE)
  p <- plot(result_ez)
  expect_s3_class(p, c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.equivalence_test",
    fig = p
  )
})

test_that("`plot.see_equivalence_test_df()` labels the interval", {
  skip_if_not_installed("bayestestR", minimum_version = "0.20.0")
  skip_if_not_installed("ggridges")

  set.seed(123)
  df <- data.frame(a = rnorm(500), b = rnorm(500, mean = 1))

  result <- bayestestR::equivalence_test(df)
  p <- plot(result, data = df)
  expect_s3_class(p, "gg")
  expect_identical(
    p$labels$x,
    "95% Equal-Tailed Interval of Posterior Samples"
  )

  result <- bayestestR::equivalence_test(df, ci = c(0.9, 0.95))
  p <- plot(result, data = df)
  expect_identical(sort(unique(p$data$HDI)), c("90% ETI", "95% ETI"))

  result <- bayestestR::equivalence_test(df, ci_method = "hdi")
  p <- plot(result, data = df)
  expect_identical(
    p$labels$x,
    "95% Highest Density Region of Posterior Samples"
  )

  # results without the attribute keep the old label
  attr(result, "ci_method") <- NULL
  p <- plot(result, data = df)
  expect_identical(
    p$labels$x,
    "95% Highest Density Region of Posterior Samples"
  )
})

test_that("`plot.see_equivalence_test()` labels the interval", {
  skip_if_not_installed("bayestestR", minimum_version = "0.20.0")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::equivalence_test(m_rstan, verbose = FALSE)
  p <- plot(result)
  expect_identical(
    p$labels$x,
    "95% Equal-Tailed Interval of Posterior Samples"
  )

  set.seed(123)
  result <- bayestestR::equivalence_test(
    m_rstan,
    ci = c(0.9, 0.95),
    ci_method = "HDI",
    verbose = FALSE
  )
  p <- plot(result)
  expect_identical(sort(unique(p$data$HDI)), c("90% HDI", "95% HDI"))
})
