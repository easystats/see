test_that("`plot.see_effectsize_table()` works", {
  m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result <- effectsize::eta_squared(m)
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "aov - eta_squared - dot plot",
    fig = plot(result)
  )
})
