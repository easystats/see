test_that("`plot.see_equivalence_test()` works", {
  if (require("effectsize") && require("ggridges")) {
    m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
    result <- eta_squared(m)
    expect_s3_class(plot(result), "gg")
  }
})
