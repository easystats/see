if (getRversion() >= "4.1" &&
  require("vdiffr") &&
  require("ggplot2") &&
  require("performance") &&
  require("lme4") &&
  require("see") &&
  require("qqplotr")) {
  test_that("`plot.see_check_normality()` works", {
    # skip_if_not(.Platform$OS.type == "windows")

    set.seed(123)
    m_lm <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
    result1 <- check_normality(m_lm)
    result2 <- check_normality(m_lm)
    result3 <- check_normality(m_lm)

    set.seed(123)
    m_lmer <<- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
    result4 <- check_normality(m_lmer, "random")
    result5 <- check_normality(m_lmer, "fixed")

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
}
