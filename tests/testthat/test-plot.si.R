test_that("`plot.see_si()` works", {
  if (requiet("rstanarm") && requiet("logspline")) {
    set.seed(123)
    m <- stan_glm(Sepal.Length ~ Petal.Width * Species,
      data = iris,
      refresh = 0
    )
    result <- si(m)

    expect_s3_class(plot(result), "gg")
  }
})
