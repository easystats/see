test_that("`plot.see_hdi()` works", {
  if (require("bayestestR") && require("rstanarm") && require("ggridges")) {
    set.seed(123)
    m <<- stan_glm(Sepal.Length ~ Petal.Width * Species,
      data = iris,
      refresh = 0
    )
    result <- hdi(m)
    expect_s3_class(plot(result), "gg")
  }
})
