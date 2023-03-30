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
