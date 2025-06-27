test_that("`plot.see_check_normality()` works", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("qqplotr")

  set.seed(123)
  m_lm <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result1 <- performance::check_normality(m_lm)
  result2 <- performance::check_normality(m_lm)
  result3 <- performance::check_normality(m_lm)

  set.seed(123)
  data(sleepstudy, package = "lme4")
  m_lmer <<- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  result4 <- performance::check_normality(m_lmer, "random")
  result5 <- performance::check_normality(m_lmer, "fixed")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "check_normality works - lm - density",
    fig = plot(result1)
  )

  vdiffr::expect_doppelganger(
    title = "check_normality works - lm - qq",
    fig = plot(result2, type = "qq")
  )

  vdiffr::expect_doppelganger(
    title = "check_normality works - lm - pp",
    fig = plot(result3, type = "pp")
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "check_normality works - lmer - random",
    fig = plot(result4)
  )

  vdiffr::expect_doppelganger(
    title = "check_normality works - lmer - fixed",
    fig = plot(result5)
  )
})


test_that("`plot.see_check_normality()` works with FA", {
  skip_if_not_installed("psych")
  skip_if_not_installed("discovr")
  skip_if_not_installed("parameters")
  skip_if_not_installed("performance")
  skip_if_not_installed("qqplotr")

  raq_items <- as.data.frame(discovr::raq)
  raq_items$id <- NULL

  raq_fa <- parameters::factor_analysis(
    raq_items,
    n = 4,
    scores = "tenBerge",
    cor = "poly",
    standardize = FALSE
  )

  out <- performance::check_normality(raq_fa)
  vdiffr::expect_doppelganger(
    title = "check_normality works - FA - nodetrend",
    fig = plot(out, detrend = FALSE)
  )
  vdiffr::expect_doppelganger(
    title = "check_normality works - FA - detrend",
    fig = plot(out, detrend = TRUE)
  )
  vdiffr::expect_doppelganger(
    title = "check_normality works - FA - pp",
    fig = plot(out, type = "pp")
  )
})
