test_that("`print.see_performance_pp_check()` works", {
  set.seed(123)
  model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
  result <- performance::check_predictions(model)

  vdiffr::expect_doppelganger(
    title = "pp check - lm",
    fig = plot(result)
  )
})


test_that("plot check_predictions, proportion and cbind binomial", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")

  data("cbpp", package = "lme4")
  m1 <- glmmTMB::glmmTMB(
    incidence / size ~ period + herd,
    weights = size,
    family = binomial,
    data = cbpp
  )

  m2 <- glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + herd,
    weights = NULL,
    family = binomial,
    data = cbpp
  )

  cbpp <- transform(cbpp, prop = incidence/size)
  m3 <- glmmTMB::glmmTMB(
    prop ~ period + herd,
    weights = size,
    family = binomial,
    data = cbpp
  )

  X <- with(cbpp, cbind(incidence, size -  incidence))
  cbpp$X <- X

  m4 <- glmmTMB::glmmTMB(
    X ~ period + herd,
    family = binomial,
    weights = NULL,
    data = cbpp
  )

  set.seed(123)
  out1 <- performance::check_predictions(m1)

  set.seed(123)
  out2 <- performance::check_predictions(m2)

  set.seed(123)
  out3 <- performance::check_predictions(m3)

  set.seed(123)
  out4 <- performance::check_predictions(m4)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "pp check - glm - cbind binomial 1",
    fig = plot(out1)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "pp check - glm - cbind binomial 2",
    fig = plot(out2)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "pp check - glm - cbind binomial 3",
    fig = plot(out3)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "pp check - glm - cbind binomial 4",
    fig = plot(out4)
  )
})
